open! Core

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

let delete_cookie () = Sys_unix.remove gradescope_config_file

let prompt_login () =
  printf "Enter Gradescope email: %!";
  let email = In_channel.input_line_exn In_channel.stdin in
  printf "Enter Gradescope password: %!";
  let password = Terminal_utils.get_password () in
  email, password
;;

let rec get_client () =
  match read_cookie () with
  | Some token ->
    (match Gradescope.initialize_gradescope ~token with
     | Some c -> c
     | None ->
       delete_cookie ();
       get_client ())
  | None ->
    let email, password = prompt_login () in
    (match Gradescope.login ~email ~password with
     | Some c ->
       save_cookie (Gradescope.token c);
       c
     | None ->
       printf "Incorrect email or password\n";
       get_client ())
;;

let () =
  let git = Github.get_current_repo () in
  let config = Gradescope.Config.get_current_config () in
  match config with
  | None -> printf "Unable to read .submit file\n"
  | Some config ->
    let client = get_client () in
    let _, url = Gradescope.gradescope_submit client ~git ~config in
    (match url with
     | None -> printf "Submission failed. Did you connect Github to Gradescope?\n"
     | Some url -> printf "Project submitted: %s\n" url)
;;
