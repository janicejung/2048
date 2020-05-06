type t =
  | Shuffle 
  | DoubleNum
  | HalfNum
  | RemoveRandomTile
  | SortRow
  (* | RemoveTile *)


(* val shuffle: int list list -> int list list *)

val double_num: int list list -> int list list

val half_num : int list list -> int list list 

val activate_powerup : Board.t -> int -> Board.t