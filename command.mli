
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

val move_left : Board.t -> Board.t

val move_right : Board.t -> Board.t 

val transpose : int list list -> int list list

val move_up : Board.t -> Board.t 

val move_down : Board.t -> Board.t 
