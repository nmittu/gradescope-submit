open! Core

type t

val login : email:string -> password:string -> t option
val initialize_gradescope : token:string -> t option
val gradescope_submit : t -> git:Github.t -> config:Config.t -> t * string option
val token : t -> string
