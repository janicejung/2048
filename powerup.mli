
(** [t] is the type for a powerup. *)
type t =
  | Shuffle 
  | DoubleNum
  | HalfNum
  | RemoveRandomTile
  | SortRow

(** [shuffle] returns a new game board that contains the same tiles but located 
    in a new places on the board. It is used as a powerup during gameplay. *)
val shuffle: int list list -> int list list

(** [double_num] returns a new board with all the numbers on the board doubled.
    The locations of the tiles remain the same. It is used as a powerup during 
    gameplay.*)
val double_num: int list list -> int list list

(** [half_num] returns a new board with all the numbers on the board halved.
    The locations of the tiles remain the same. It is used as a powerup during 
    gameplay. *)
val half_num : int list list -> int list list 

(** [double_num] returns a new board with a random tile removed from the
    original board. The other tiles remain unchanged. The random tile removed is a 
    number tile and cannot be a powerup tile. It is used as a powerup during 
    gameplay. *)
val remove_random_tile : int list list -> int list list

(** [sort_row] returns a new board with all the numbers on the board sorted
    from greatest to least. 
    For example, sort_row [[0;0;1;2]] returns [[2;1;0;0]]  *) 
val sort_row : int list list -> int list list

(** [activate_powerup] returns the new Board.t type that results from playing
    the input powerup on the original board. [powerup] is an integer that 
    corresponds to a specific powerup.*)
val activate_powerup : Board.t -> int -> Board.t