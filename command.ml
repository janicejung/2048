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

let combine_left list n acc =
  let rec helper list n acc was_same =
    match list with
    | [] -> fill_rest n acc
    | h::t -> if was_same then helper t n acc false else begin
        match t with
        | [] -> fill_rest n (h::acc)
        | h'::t' ->
          if h = h' then helper t n ((h+h')::acc) true 
          else helper t n (h::acc) false
      end in List.rev (helper (List.filter (fun x -> x <> 0) list) 4 [] false)

let combine_right list n acc = 
  combine_left (List.rev list) n acc |> List.rev

let move_left board =
  List.map (fun row -> combine_left row 4 []) board

let move_right board = 
  List.map (fun row -> combine_right row 4 []) board

let rec transpose board =
  match board with
  | [] -> []
  | []::t -> transpose t
  | (hh::ht)::t ->
    (hh::(List.map List.hd t))::(transpose (ht::(List.map List.tl t)))

let move_up board =
  board |> transpose |> move_left |> transpose

let move_down board =
  board |> transpose |> move_right |> transpose