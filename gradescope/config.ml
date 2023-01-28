open! Core

type t =
  { course_id : int
  ; assignment_id : int
  }

let get_current_config () =
  let toml = Toml.Parser.(from_filename ".submit" |> unsafe) in
  let course_id = Toml.Lenses.(get toml (key "course" |-- table |-- key "id" |-- int)) in
  let assignment_id =
    Toml.Lenses.(get toml (key "assignment" |-- table |-- key "id" |-- int))
  in
  Option.both course_id assignment_id
  |> Option.map ~f:(fun (course_id, assignment_id) -> { course_id; assignment_id })
;;
