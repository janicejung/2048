open Board

type t =
  (* | Shuffle  *)
  | DoubleNum
  (* | RemoveTile *)
(* | RemoveRandomTile *)


(* let shuffle board = 
   let dim = List.length board in 
   let x = Random.int dim  *)

let double_num board = 
  List.map (fun row -> List.map (fun x -> 2*x) row) board


let activate_powerup state powerup = 
  match powerup with 
  | 3 -> { state with board = double_num state.board}
  | 0 -> state
  | _-> failwith "not a valid powerup"