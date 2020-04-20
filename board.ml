type board = int list list

let make_board n =
  let rec board_helper num inc acc =
    let rec make_row n acc =
      if n = 0 then acc else make_row (n-1) (0::acc) in 
    if inc = 0 then acc
    else board_helper (num) (inc-1) ((make_row num [])::acc) in
  board_helper n n []

let rec print_board board =
  let rec print_row row =
    match row with
    | [] -> ()
    | h::t -> print_int h; print_string " "; print_row t
  in match board with
  | [] -> ()
  | h::t -> print_row h; print_endline ""; print_board t;;


let rec board_full board = 
  match board with
  | [] -> true
  | h::t -> if List.mem 0 h then false else board_full t

(* note: n in this case is n-1 *)
let rec row_empty_tiles row n acc = 
  if n < 0 then acc
  else if List.nth row n <> 0 then row_empty_tiles row (n-1) acc 
  else row_empty_tiles row (n-1) (n::acc)

let get_empty_tiles board n = 
  List.map (fun row -> row_empty_tiles row (n-1) [])

let gen_random_tile board =
  let row_coord = Random.int (List.length board) in
  let random_row = List.nth board row_coord in 
  let random_tile = Random.int (List.length random_row) in
  let col_coord = List.nth random_row random_tile in
  (row_coord, col_coord)

let place_num_in_row row y num =
  let rec helper row y num y_count acc =
    match row with
    | [] -> failwith "empty"
    | h::t ->
      if y_count = y then (acc @ [num] @ t) 
      else helper t y num (y_count + 1) (acc @ [h]) in
  helper row y num 0 []

let place_random_tile board (x, y) num = 
  let rec helper board x y x_count acc =
    match board with
    | [] -> failwith "empty"
    | h::t ->
      if x_count <> x then helper t x y (x_count+1) (acc @ [h])
      else acc @ [(place_num_in_row h y num)] @ t in 
  helper board x y 0 []

let gen_random_num = 
  let chance = Random.int 100 in
  if chance < 60 then 2
  else if chance >= 60 && chance < 96 then 4
  else 8