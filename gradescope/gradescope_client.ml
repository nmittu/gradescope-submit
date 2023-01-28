open! Core
open! Cohttp
open! Cohttp_lwt
open! Cohttp_lwt_unix
open! Lwt.Syntax

(* Cookies *)
type t = Cookie.cookie list

let gradescope_config_dir = Filename.concat (Sys.getenv_exn "HOME") ".gradescope-submit"
let gradescope_config_file = Filename.concat gradescope_config_dir "signed_token"

let read_cookie () =
  if Sys_unix.file_exists_exn gradescope_config_file
  then Some (In_channel.read_all gradescope_config_file)
  else None
;;

let save_cookie c =
  if not (Sys_unix.file_exists_exn gradescope_config_dir)
  then Core_unix.mkdir gradescope_config_dir;
  Out_channel.write_all gradescope_config_file ~data:c
;;

(* Helper function to hide password on terminal *)
let get_password () =
  let open Core_unix.Terminal_io in
  let old_tc = tcgetattr Core_unix.stdin in
  let new_tc = { old_tc with c_echo = false } in
  tcsetattr new_tc Core_unix.stdin ~mode:TCSANOW;
  let password = In_channel.input_line_exn In_channel.stdin in
  tcsetattr old_tc Core_unix.stdin ~mode:TCSANOW;
  printf "\n%!";
  password
;;

let prompt_login () =
  printf "Enter Gradescope email: %!";
  let email = In_channel.input_line_exn In_channel.stdin in
  printf "Enter Gradescope password: %!";
  let password = get_password () in
  email, password
;;

let fix_cookie_headers headers =
  Header.to_list headers
  |> List.map ~f:(fun (k, v) -> String.lowercase k, v)
  |> Header.of_list
;;

let merge_cookies cookies ~new_cookies =
  List.fold
    ~init:new_cookies
    ~f:(fun a (k, v) ->
      if List.exists ~f:(fun (k2, _) -> String.(k = k2)) a then a else (k, v) :: a)
    cookies
;;

let req_with_cookies
  ~(method_ :
     ?ctx:Cohttp_lwt_unix.Net.ctx
     -> ?headers:Header.t
     -> Uri.t
     -> (Response.t * Cohttp_lwt.Body.t) Lwt.t)
  ?(headers : Header.t option)
  ~cookies
  url
  =
  let+ resp, body =
    method_
      ?headers:
        (Option.map
           ~f:(fun headers ->
             Header.of_list
               (("Cookie", snd (Cookie.Cookie_hdr.serialize cookies))
               :: Header.to_list headers))
           headers)
      url
  in
  let headers = fix_cookie_headers (Response.headers resp) in
  let new_cookies =
    Cookie.Set_cookie_hdr.extract headers
    |> List.map ~f:(fun (_, c) -> Cookie.Set_cookie_hdr.cookie c)
  in
  merge_cookies cookies ~new_cookies, resp, body
;;

let signed_token c =
  List.find c ~f:(fun (name, _) -> String.(name = "signed_token"))
  |> Option.map ~f:(fun (_, v) -> v)
;;

let login () =
  let email, password = prompt_login () in
  let* cookies, _, body =
    req_with_cookies
      ~method_:Client.get
      ~cookies:[]
      (Uri.of_string "https://www.gradescope.com/login")
  in
  let* body_str = Body.to_string body in
  let open Soup in
  let document = parse body_str in
  let csrf_token = document $ "input[name=authenticity_token]" |> R.attribute "value" in
  let req =
    Body.of_form
      [ "authenticity_token", [ csrf_token ]
      ; "session[email]", [ email ]
      ; "session[password]", [ password ]
      ; "session[remember_me]", [ "1" ]
      ; "commit", [ "Log In" ]
      ; "session[remember_me_sso]", [ "0" ]
      ]
  in
  let* cookies, _, _ =
    req_with_cookies
      ~method_:(Client.post ~body:req ?chunked:None)
      ~cookies
      ~headers:
        (Header.of_list
           [ "Host", "www.gradescope.com"; "Referer", "https://www.gradescope.com" ])
      (Uri.of_string "https://www.gradescope.com/login")
  in
  let signed_cookie = signed_token cookies in
  Option.map signed_cookie ~f:(fun _ -> cookies) |> Lwt.return
;;

let authenticated cookies =
  let* cookies, resp, _ =
    req_with_cookies
      ~method_:Cohttp_lwt_unix.Client.get
      ~cookies
      ~headers:(Cohttp.Header.of_list [ "Accept", "application/json" ])
      (Uri.of_string "https://www.gradescope.com/login")
  in
  let code = Response.status resp |> Code.code_of_status in
  assert (code = 401);
  cookies |> Lwt.return
;;

let init_cookies cookies =
  let lwt =
    let* cookies, _, _ =
      req_with_cookies
        ~method_:Cohttp_lwt_unix.Client.get
        ~cookies
        (Uri.of_string "https://www.gradescope.com")
    in
    cookies |> authenticated
  in
  Lwt_main.run lwt
;;

let rec initialize_gradescope () =
  let cookies =
    match read_cookie () with
    | Some tok -> [ "signed_token", tok ]
    | None ->
      (match Lwt_main.run (login ()) with
       | Some c ->
         signed_token c |> Option.iter ~f:save_cookie;
         c
       | None ->
         printf "Incorrect username or password\n";
         initialize_gradescope ())
  in
  init_cookies cookies
;;

let get_git_repos ~csrf t =
  let rec fetch_loop page =
    let url =
      sprintf
        "https://www.gradescope.com/github_repositories?uid=&page=%d&per_page=100"
        page
    in
    let* cookies, _, body =
      req_with_cookies
        ~cookies:t
        ~method_:Cohttp_lwt_unix.Client.get
        ~headers:(Cohttp.Header.of_list [ "X-CSRF-Token", csrf ])
        (Uri.of_string url)
    in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let json = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let res =
      json
      |> member "repositories"
      |> to_list
      |> List.map ~f:(fun r ->
           r |> member "full_name" |> to_string, r |> member "id" |> to_int)
    in
    let uid = json |> member "uid" |> to_string in
    if List.length res = 100
    then
      let* () = Lwt_io.printf "." in
      let* cookies, _, res' = fetch_loop (page + 1) in
      Lwt.return (cookies, uid, res @ res')
    else
      let* () = Lwt_io.printf ".\n" in
      Lwt.return (cookies, uid, res)
  in
  let* () = Lwt_io.printf "Fetching Github repositories" in
  fetch_loop 1
;;

let gradescope_submit t ~(git : Github.t) ~(config : Config.t) =
  let lwt =
    let url = sprintf "https://www.gradescope.com/courses/%d" config.course_id in
    let* cookies, _, body =
      req_with_cookies
        ~cookies:t
        ~headers:
          (Header.of_list
             [ "Host", "www.gradescope.com"; "Referer", "https://www.gradescope.com" ])
        ~method_:Cohttp_lwt_unix.Client.get
        (Uri.of_string url)
    in
    let* body_str = Cohttp_lwt.Body.to_string body in
    let open Soup in
    let document = parse body_str in
    let csrf = document $ "meta[name=csrf-token]" |> R.attribute "content" in
    let* cookies, uid, git_repos = get_git_repos cookies ~csrf in
    let git_repo =
      List.find ~f:(fun (n, _) -> String.(n = git.owner ^ "/" ^ git.repo)) git_repos
    in
    let git_id = Option.map ~f:(fun (_, id) -> id) git_repo in
    match git_id with
    | None -> (cookies, None) |> Lwt.return
    | Some git_id ->
      let req_body =
        Cohttp_lwt.Body.of_form
          [ "authenticity_token", [ csrf ]
          ; "submission[method]", [ "github" ]
          ; "submission[repository]", [ Int.to_string git_id ]
          ; "submission[uid]", [ uid ]
          ; "submission[revision]", [ git.branch ]
          ]
      in
      let submit_uri =
        sprintf
          "https://www.gradescope.com/courses/%d/assignments/%d/submissions"
          config.course_id
          config.assignment_id
      in
      let* cookies, _, body =
        req_with_cookies
          ~cookies
          ~method_:(Client.post ~body:req_body ?chunked:None)
          ~headers:
            (Cohttp.Header.of_list
               [ "Accept", "application/json"
               ; "Host", "www.gradescope.com"
               ; "Referer", submit_uri
               ])
          (Uri.of_string submit_uri)
      in
      let* body_str = Cohttp_lwt.Body.to_string body in
      let json = Yojson.Basic.from_string body_str in
      let open Yojson.Basic.Util in
      let error = json |> member "error" in
      let success = json |> member "success" |> to_bool_option in
      (match error, success with
       | `Null, Some _ ->
         ( cookies
         , Some
             (json |> member "url" |> to_string |> sprintf "https://www.gradescope.com%s")
         )
         |> Lwt.return
       | _ -> (cookies, None) |> Lwt.return)
  in
  Lwt_main.run lwt
;;
