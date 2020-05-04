type t = {
  board: int list list;
  score: int;
}

(** [make_board n] initializes an int list list of size n x n and a score of 0*)
let make_board n =
  let rec board_helper num inc acc =
    let rec make_row n acc =
      if n = 0 then acc else make_row (n-1) (0::acc) in 
    if inc = 0 then acc
    else board_helper (num) (inc-1) ((make_row num [])::acc) in
  {board = board_helper n n []; score= 0}

let print_helper length number =
  (if length = 1 then (print_int number; print_string "    | ";)
   else if length = 2 then (print_int number; print_string "   | ";)
   else if length = 3 then (print_int number; print_string "  | ";)
   else (print_int number; print_string " | ";))

let rec print_board (board:int list list) =
  let rec print_row row =
    match row with
    | [] -> ()
    | h::t -> print_helper (String.length (string_of_int h)) h;
      print_row t
  in match board with
  | [] -> ()
  | h::t -> (print_row h; print_endline "\n---------------------------"; print_board t;)

let print_score (score:int)
  = print_endline ("Score: " ^ string_of_int score)

(** [board_full board] is true if [board] is full and false otherwise. *)
let rec board_full board = 
  match board with
  | [] -> true
  | h::t -> if List.mem 0 h then false else board_full t

(** [row_empty_tiles row n acc] is a list of indices in [row] where the value
    is 0*)
let rec row_empty_tiles (row:int list) (n:int) (acc:int list) = 
  if n < 0 then acc
  else if List.nth row n <> 0 then row_empty_tiles row (n-1) acc 
  else row_empty_tiles row (n-1) (n::acc)

(** [get_empty_tiles n board] is a nested list of empty indexes in [board]. *)
let get_empty_tiles (n:int) (board: int list list) = 
  List.map (fun row -> row_empty_tiles row (n-1) []) board

(*for row need to check if board full before gen_random_tile
  cannot pick empty row for column
*)
(** [get_non_empty_row empty_tile_list] is the randow row on the board where
    the next tile will be placed. *)
let rec get_non_empty_row empty_tile_list =
  let row_coord = Random.int (List.length empty_tile_list) in
  let random_row = List.nth empty_tile_list row_coord in 
  if List.length random_row = 0 then get_non_empty_row empty_tile_list 
  else (row_coord, random_row)

(** [gen_random_tile _empty_tile_list] is the random coordinate on the board
    where the next tile will be spawned *)
let gen_random_tile (empty_tile_list: int list list) : int*int =
  let random = get_non_empty_row empty_tile_list in 
  match random with
  |(row_coord, random_row) -> 
    let random_tile = Random.int (List.length random_row) in
    let col_coord = List.nth random_row random_tile in
    (row_coord, col_coord)

(** [place_num_in_row row y num] places [num] at index [y] in [row]. *)
let place_num_in_row (row:int list) (y:int) (num:int) =
  let rec helper row y num y_count acc =
    match row with
    | [] -> failwith "empty"
    | h::t ->
      if y_count = y then (acc @ [num] @ t) 
      else helper t y num (y_count + 1) (acc @ [h]) in
  helper row y num 0 []

(** [place_random_tile_helper board num (x, y)] places [num] in [board]
    at coordinate [(x, y)] *)
let place_random_tile_helper board (num:int) ((x,y):int*int) = 
  let rec helper board x y x_count acc =
    match board with
    | [] -> failwith "empty"
    | h::t ->
      if x_count <> x then helper t x y (x_count+1) (acc @ [h])
      else acc @ [(place_num_in_row h y num)] @ t in 
  helper board x y 0 []

(** [gen_random_num] generates a random 2, 4, or 8 to be placed on the board*)
let gen_random_num = 
  let chance = Random.int 100 in
  if chance < 60 then 2
  else if chance >= 60 && chance < 96 then 4
  else 8

(** [place_random_tile n state] places the number [n] on the board in the
    [state] with a randomly generated number*)
let place_random_tile n state =
  let new_num = gen_random_num in
  {state with board =
                (state.board |> get_empty_tiles n |> gen_random_tile
                 |> place_random_tile_helper state.board new_num)}

(** [have_lost_row list] checks to see if there are duplicates in [list]*)
let rec have_lost_row list =
  match list with
  | [] -> true
  | h::t -> begin
      match t with
      | [] -> true
      | h'::t' -> if h = h' then false else have_lost_row t
    end

let rec transpose board =
  match board with
  | [] -> []
  | []::t -> transpose t
  | (hh::ht)::t ->
    (hh::(List.map List.hd t))::(transpose (ht::(List.map List.tl t)))

(** [have_lost board] checks to see if any combinations can be made in [board]*)
let rec have_lost board =
  not (List.mem false (List.map have_lost_row board)) &&
  not (List.mem false (List.map have_lost_row (transpose board)))

(** [compare_list l1 l2] is if the elements of [l1] and [l2]*)
let rec compare_list l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | h1::t1, h2::t2 -> if h1 = h2 then compare_list t1 t2 else false
  | _ -> false

let rec compare_board b1 b2 = 
  match b1, b2 with
  | [], [] -> true
  | h1::t1, h2::t2 ->
    if compare_list h1 h2 then compare_board t1 t2 else false
  | _ -> false