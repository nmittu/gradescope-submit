open! Core

type t =
  { course_id : int
  ; assignment_id : int
  }

val get_current_config : unit -> t option
