
type command =
  | Left
  | Right
  | Up
  | Down
  | Quit

exception Malformed

exception Empty

val parse: string -> command

val fill_rest : int -> int list -> int list

val combine_left : int list -> int -> int list -> int list

val combine_right : int list -> int -> int list -> int list 

val move_left : int list list -> int list list

val move_right : int list list -> int list list 

val transpose : int list list -> int list list

val move_up : int list list -> int list list 

val move_down : int list list -> int list list 
