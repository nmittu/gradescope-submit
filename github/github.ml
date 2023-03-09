open! Core

type t =
  { owner : string
  ; repo : string
  ; branch : string
  }

let get_current_repo () =
  let command = "git config --get remote.origin.url" in
  let remote_origin = Core_unix.open_process_in command |> In_channel.input_all in
  let origin_regex =
    Str.regexp
      {|\(git@github\.com:\|https:\/\/github\.com\/\)\(.+\)|}
  in
  assert (Str.string_match origin_regex remote_origin 0);
  let repo = Str.matched_group 2 remote_origin in
  let repo = String.chop_suffix_if_exists repo ~suffix:".git" in
  let branch_cmd = "git rev-parse --abbrev-ref HEAD" in
  let branch = Core_unix.open_process_in branch_cmd |> In_channel.input_all in
  match Str.bounded_split (Str.regexp "/") repo 2 with
  | [ owner; repo ] -> { owner; repo; branch }
  | _ -> failwith "unable to get repo name"
;;
