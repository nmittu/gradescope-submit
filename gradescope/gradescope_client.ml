open! Core
open! Cohttp
open! Cohttp_lwt
open! Cohttp_lwt_unix
open! Lwt.Syntax

(* Cookies *)
type t = Cookie.cookie list

(* Cohttp doesn't recognize cookies when the header name is capitalized :/ *)
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

(* make a request while keeping track of cookies *)
let req_with_cookies
  ~(method_ : ?ctx:Net.ctx -> ?headers:Header.t -> Uri.t -> (Response.t * Body.t) Lwt.t)
  ?(headers : Header.t option)
  ~cookies
  url
  =
  let header_lst = headers |> Option.map ~f:Header.to_list |> Option.value ~default:[] in
  let+ resp, body =
    method_
      ~headers:(Header.of_list (Cookie.Cookie_hdr.serialize cookies :: header_lst))
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

let login ~email ~password =
  let lwt =
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
  in
  Lwt_main.run lwt
;;

let authenticated cookies =
  let* cookies, resp, _ =
    req_with_cookies
      ~method_:Client.get
      ~cookies
      ~headers:(Header.of_list [ "Accept", "application/json" ])
      (Uri.of_string "https://www.gradescope.com/login")
  in
  let code = Response.status resp |> Code.code_of_status in
  if code = 401 then Some cookies |> Lwt.return else Lwt.return_none
;;

(* Initialize cookies before making any requests *)
let init_cookies cookies =
  let lwt =
    let* cookies, _, _ =
      req_with_cookies
        ~method_:Client.get
        ~cookies
        (Uri.of_string "https://www.gradescope.com")
    in
    cookies |> authenticated
  in
  Lwt_main.run lwt
;;

let initialize_gradescope ~token = init_cookies [ "signed_token", token ]

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
        ~method_:Client.get
        ~headers:(Header.of_list [ "X-CSRF-Token", csrf ])
        (Uri.of_string url)
    in
    let* body_str = Body.to_string body in
    let json = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let res =
      json
      |> member "repositories"
      |> to_option to_list
      |> Option.map ~f:(fun l ->
           l
           |> List.map ~f:(fun r ->
                r |> member "full_name" |> to_string, r |> member "id" |> to_int))
      |> Option.value ~default:[]
    in
    let uid = json |> member "uid" |> to_string_option |> Option.value ~default:"" in
    (* If this page is full, get the next *)
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
    (* Get CSRF token from here *)
    let url = sprintf "https://www.gradescope.com/courses/%d" config.course_id in
    let* cookies, _, body =
      req_with_cookies
        ~cookies:t
        ~headers:
          (Header.of_list
             [ "Host", "www.gradescope.com"; "Referer", "https://www.gradescope.com" ])
        ~method_:Client.get
        (Uri.of_string url)
    in
    let* body_str = Body.to_string body in
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
        Body.of_form
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
            (Header.of_list
               [ "Accept", "application/json"
               ; "Host", "www.gradescope.com"
               ; "Referer", submit_uri
               ])
          (Uri.of_string submit_uri)
      in
      let* body_str = Body.to_string body in
      let json = Yojson.Basic.from_string body_str in
      let open Yojson.Basic.Util in
      let error = json |> member "error" in
      let success = json |> member "success" |> to_bool_option in
      (* Check for no errors and success true *)
      (match error, success with
       | `Null, Some true ->
         ( cookies
         , Some
             (json |> member "url" |> to_string |> sprintf "https://www.gradescope.com%s")
         )
         |> Lwt.return
       | _ -> (cookies, None) |> Lwt.return)
  in
  Lwt_main.run lwt
;;

let token t =
  match signed_token t with
  | Some t -> t
  | None -> failwith "No token in client"
;;
