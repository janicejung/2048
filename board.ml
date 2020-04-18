type board = int list list

let make_board n =
  let rec board_helper num inc acc =
    let rec make_row n acc =
      if n = 0 then acc else make_row (n-1) (0::acc) in 
    if inc = 0 then acc else board_helper (num) (inc-1) ((make_row num [])::acc) in
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