open! Core

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
