open! Core

let () =
  let git = Github.get_current_repo () in
  let config = Gradescope.Config.get_current_config () in
  match config with
  | None -> printf "Unable to read .submit file\n"
  | Some config ->
    let client = Gradescope.initialize_gradescope () in
    let _, url = Gradescope.gradescope_submit client ~git ~config in
    (match url with
     | None -> printf "Submission failed. Did you connect Github to Gradescope?\n"
     | Some url -> printf "Project submitted: %s\n" url)
;;
