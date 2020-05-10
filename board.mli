(** The representation of the board in the game and the functions associated
    with it. *)

(** [t] is a representation of the game board, current score, and the number
    of moves a user has made. *)
type t = {
  board: int list list;
  score:int;
  moves:int;
}

(** [make_board n] initializes an int list list of size n x n and a score of 0*)
val make_board: int -> t

(** [print_board board] prints the [board] of the game in the terminal.
    Note: this function is no longer being used in the code due to the
    development of the GUI and is now strictly used for manual testing
    purposes. *)
val print_board: int list list -> unit

(** [print_score score] prints the [score] of the game in the terminal.
    Note: this function is no longer being used in the code due to the
    development of the GUI and is now strictly used for manual testing
    purposes. *)
val print_score: int -> unit

(** [board_full board] is [true] if [board] only contain numbers greater than
    0 and [false] otherwise. *)
val board_full: int list list -> bool

(** [row_empty_tiles row n acc] gets the indices of the nonzero tiles in [row]
    of length [n] and appends it to [acc]. *)
val row_empty_tiles: int list -> int -> int list -> int list

(** [get_empty_tiles n board] is a nested list of empty indexes in [board] of
    size [nxn]. *)
val get_empty_tiles: int -> int list list -> int list list

(** [gen_random_tile empty_tile_list] is the random coordinate on the board
    where the next tile will be spawned determined by [empty_tile_list]. *)
val gen_random_tile : int list list -> (int * int)

(** [place_num_in_row row y num] places [num] at index [y] in [row]. *)
val place_num_in_row: int list -> int -> int -> int list

(** [place_random_tile_helper board num (x, y)] places [num] in [board]
    at coordinate [(x, y)] *)
val place_random_tile_helper: int list list -> int -> (int * int) 
  -> int list list

(** [place_random_tile n state] places the randomly generated number [n] on the
    board of [state]. *)
val place_random_tile: int -> t -> t

(** [place_random_powerup n state] places the randomly generated powerup [n]
    on the board of [state]. *)
val place_random_powerup: int -> t -> t

(** [place_chosen_tile num n board] places the chosen number [num] on a [board]
    of size [nxn]. *)
val place_chosen_tile : int -> int -> int list list -> int list list

(** [gen_random_num] generates a random 2, 4, or 8 to be placed on the board. *)
val gen_random_num: unit -> int

(** [have_lost_row list] checks to see if there are duplicates in [list].
    Returns true if no dups. *)
val have_lost_row: int list -> bool

(** [have_lost board] checks to see if any further combinations can be made
    in [board]. *)
val have_lost: int list list -> bool

(** [have_won board] checks if there is a 2048 on the board. *)
val have_won: int list list -> bool

(** [compare_board b1 b2] checks to see if [b1] and [b2] have the exact same
    elements in the exact same order as each other. *)
val compare_board: int list list -> int list list -> bool