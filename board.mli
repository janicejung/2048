
type t = {
  board: int list list;
  score:int;
  moves:int;
}

val make_board: int -> t

val print_board: int list list -> unit

val print_score: int -> unit

val board_full: int list list -> bool

val row_empty_tiles: int list -> int -> int list -> int list

val get_empty_tiles: int -> int list list -> int list list

val gen_random_tile : int list list -> (int * int)

val place_num_in_row: int list -> int -> int -> int list

val place_random_tile_helper: int list list -> int -> (int * int) 
  -> int list list

val place_random_tile: int -> t -> t

val place_random_powerup: int -> t -> t

val place_chosen_tile : int -> int -> int list list -> int list list

val gen_random_num: unit -> int

val have_lost_row: int list -> bool

val have_lost: int list list -> bool

val have_won: int list list -> bool

val compare_board: int list list -> int list list -> bool