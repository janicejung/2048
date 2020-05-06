open Board

type t =
  | Shuffle 
  | DoubleNum
  | HalfNum
  (* | RemoveTile *)
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

let rec row_nonempty_tiles (row:int list) (n:int) (acc: int list) = 
  if n < 0 then acc
  else if List.nth row n = 0 then row_nonempty_tiles row (n-1) acc 
  else row_nonempty_tiles row (n-1) (n::acc)

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
  place_random_tile_helper board 0 random_coord

let sort_row board =
  List.map (fun row -> List.rev (List.sort compare row)) board

let activate_powerup state powerup = 
  match powerup with 
  | 3 -> { state with board = double_num state.board}
  | 5 -> { state with board = half_num state.board}
  | 7 -> { state with board = sort_row state.board}
  | 11 -> { state with board = shuffle state.board}
  | 13 -> {state with board = remove_random_tile state.board}
  | _-> failwith "not a valid powerup"