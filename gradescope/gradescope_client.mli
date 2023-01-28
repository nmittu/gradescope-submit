open! Core

type t

val initialize_gradescope : unit -> t
val gradescope_submit : t -> git:Github.t -> config:Config.t -> t * string option
