
(** [command] is the type for a movement on the board. *)
type command =
  | Left
  | Right
  | Up
  | Down
  | Quit

(** [exception Malformed] is the error if the user did not enter a valid
    movement direction.  *)
exception Malformed

(** [exception Empty] is the error if user enters an empty command. *)
exception Empty

(** [parse str] parses the users input to match it with [command].
    Raises [Malformed] or [Empty] if the input was invalid.
    Note: this function is no longer being used in the code due to the
    development of the GUI and is now strictly used for manual testing
    purposes.*)
val parse: string -> command

(** [fill_rest n list] fills the rest of the [list] with 0's until it has a
    length of [n]. *)
val fill_rest : int -> int list -> int list

(** [move_left state] is the new state after moving the entire game board
    left. *)
val move_left : Board.t -> Board.t

(** [move_left state] is the new state after moving the entire game board
    right. *)
val move_right : Board.t -> Board.t 

(** [transpose board] is the matrix transposition of [board]. *)
val transpose : int list list -> int list list

(** [move_left state] is the new state after moving the entire game board
    up. *)
val move_up : Board.t -> Board.t 

(** [move_left state] is the new state after moving the entire game board
    down. *)
val move_down : Board.t -> Board.t 

(** [combine_left_board list n acc] is a new list if a user has moved left
    on [list] *)
val combine_left_board: int list -> int -> int list -> int list

(** [combine_right_board list n acc] is a new list if a user has moved right
    on [list] *)
val combine_right_board: int list -> int -> int list -> int list

(** [combine_score list n acc] is the new score which is the sum of all the 
    numbers in [list]. *)
val combine_score: int list -> int ->int -> int 
