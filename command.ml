open Board
open Powerup
type command =
  | Left
  | Right
  | Up
  | Down
  | Quit

exception Malformed
exception Empty

let parse str = 
  let lst = str |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in
  match lst with
  | [] -> raise (Empty)
  | "w"::t when t = [] -> Up
  | "a"::t when t = [] -> Left
  | "s"::t when t = [] -> Down
  | "d"::t when t = [] -> Right
  | "quit"::t when t = [] -> Quit
  | _ -> raise (Malformed)

let rec fill_rest n list =
  if List.length list <> n then fill_rest n (0::list) else list

let combine_score list n acc =
  let rec helper list n acc was_same =
    match list with
    | [] -> acc
    | h::t -> if was_same then helper t n acc false else begin
        match t with
        | [] -> acc
        | h'::t' ->
          if h = h' then helper t n (h + h'+ acc) true 
          else helper t n acc false
      end in helper (List.filter (fun x -> x <> 0 && x mod 2 = 0) list) 4 acc false

let combine_left_board list n acc  =
  let rec helper list n acc was_same =
    match list with
    | [] -> fill_rest n acc
    | h::t -> if was_same then helper t n acc false else 
        begin
          match t with
          | [] -> fill_rest n (h::acc)
          | h'::t' ->
            if h = h' then helper t n ((h+h')::acc) true 
            else helper t n (h::acc) false
        end in 
  List.rev (helper (List.filter (fun x -> x <> 0 && x mod 2 = 0) list) 4 [] false)

let combine_right_board list n acc = 
  combine_left_board (List.rev list) n acc |> List.rev

let obtained_powerup_left row = 
  let filtered_row = List.filter (fun x -> x <> 0) row in
  match List.find_opt (fun x -> x mod 2 <> 0) filtered_row with 
  | Some x -> 
    let rec helper lst =
      match lst with
      | [] -> 0
      | h::t -> if h = x && List.length t <> 0 then x else helper t
    in helper filtered_row
  | None -> 0

let obtained_powerup_right row = 
  obtained_powerup_left (List.rev row)

let get_powerup_left board =
  let powerup_list = List.map (fun row -> obtained_powerup_left row) board in 
  match List.find_opt (fun x -> x <> 0) powerup_list with 
  | Some x -> x
  | None -> 0

let get_powerup_right board =
  let powerup_list = List.map (fun row -> obtained_powerup_right row) board in 
  match List.find_opt (fun x -> x <> 0) powerup_list with 
  | Some x -> x
  | None -> 0

let move_left (state:Board.t) =
  let powerup = get_powerup_left state.board in
  let new_board =
    List.map (fun row -> combine_left_board row 4 []) state.board in 
  let new_score_list =
    List.map (fun row -> combine_score row 4 0) state.board in  
  let new_score = List.fold_left (+) state.score new_score_list in
  let powerup_board = {state with board = new_board; score = new_score} in
  Powerup.activate_powerup powerup_board powerup

let move_right (state:Board.t) = 
  let powerup = get_powerup_right state.board in
  let new_board =
    List.map (fun row -> combine_right_board row 4 []) state.board in 
  let new_score_list =
    List.map (fun row -> combine_score row 4 0) state.board in  
  let new_score = List.fold_left (+) state.score new_score_list in
  let powerup_board = {state with board = new_board; score = new_score} in
  Powerup.activate_powerup powerup_board powerup

let rec transpose board =
  match board with
  | [] -> []
  | []::t -> transpose t
  | (hh::ht)::t ->
    (hh::(List.map List.hd t))::(transpose (ht::(List.map List.tl t)))

let move_up (state:Board.t) =
  let board1 = state.board |> transpose in 
  let transpose_state = {state with board = board1} in 
  let after_move_state = move_left transpose_state in 
  let board2 = after_move_state.board |> transpose in 
  {after_move_state with board = board2}

let move_down (state:Board.t) =
  let board1 = state.board |> transpose in 
  let transpose_state = {state with board = board1} in 
  let after_move_state = move_right transpose_state in 
  let board2 = after_move_state.board |> transpose in 
  {after_move_state with board = board2}