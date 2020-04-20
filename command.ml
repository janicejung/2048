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
  combine_left list n acc |> List.rev

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
  board |> transpose |> move_right |> transpose

let move_down board =
  board |> transpose |> move_left |> transpose