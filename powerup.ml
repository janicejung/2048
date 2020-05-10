open Board

type t =
  | Shuffle 
  | DoubleNum
  | HalfNum
  | RemoveRandomTile
  | SortRow

let shuffle board = 
  let all_tiles = List.flatten board in 
  let dim = List.length board in
  let init_board = (make_board dim).board in 
  let rec shuffle_helper acc all = 
    match all with 
    | h::t -> let acc' = place_chosen_tile h dim acc in shuffle_helper acc' t
    | [] -> acc 
  in shuffle_helper init_board all_tiles

let double_num board = 
  List.map (fun row -> List.map (fun x -> 2*x) row) board

let half_num board =
  List.map (fun row -> List.map (fun x -> if x=2 then x else x/2) row) board

(** [row_nonempty_tiles] returns a list of the indexes of the nonempty tiles
    in a row. Nonempty tiles are tiles that do not contain the value zero.
    For example, row_nonempty_tiles [2;4;0;8] 4 [] returns [0;1;3]*) 
let rec row_nonempty_tiles (row:int list) (n:int) (acc: int list) = 
  if n < 0 then acc
  else if List.nth row n = 0 then row_nonempty_tiles row (n-1) acc 
  else row_nonempty_tiles row (n-1) (n::acc)

(** [get_coord_list] returns a tuple list which is the list of coordinates
    of a board. This list of coordinates designates the position of a tile
    on a board. *)  
let get_coord_list (n:int) (board: int list list) =
  let rec helper (board : int list list) (acc: (int*int) list) (count:int) =
    match board with
    | [] -> acc
    | h::t -> helper t ((List.map (fun x -> (x, count)) h) @ acc) (count + 1) 
  in helper (List.map (fun row -> row_nonempty_tiles row (n-1) []) board) [] 0

let remove_random_tile board = 
  let n = List.length board in
  let coord_list = get_coord_list n board in 
  let random = Random.int (List.length coord_list) in
  let random_coord = List.nth coord_list random in 
  let rev_coord =
    match random_coord with 
    | (x, y) -> (y, x) in
  place_random_tile_helper board 0 rev_coord

let sort_row board =
  List.map (fun row -> List.rev (List.sort compare row)) board

(** [powerup_helper] returns the updated state of the board after the powerup
    is applied to the board.*) 
let powerup_helper state powerup =
  match powerup with
  | DoubleNum -> {state with board = double_num state.board}
  | HalfNum -> {state with board = half_num state.board}
  | SortRow -> {state with board = sort_row state.board}
  | Shuffle -> {state with board = shuffle state.board}
  | RemoveRandomTile -> {state with board = remove_random_tile state.board}

let activate_powerup state powerup = 
  match powerup with 
  | 3 -> powerup_helper state DoubleNum
  | 5 -> powerup_helper state HalfNum
  | 7 -> powerup_helper state SortRow
  | 11 -> powerup_helper state Shuffle
  | 13 -> powerup_helper state RemoveRandomTile
  | _-> failwith "not a valid powerup"