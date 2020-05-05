open Board
open Command

let spawn_powerup n state =
  let chance = Random.int 100  in 
  if state.moves > 5 && chance < 100 then 
    let new_board = place_random_powerup n state in
    {new_board with moves = 0} 
  else state

(** [new_board_helper] takes in a move function and an entire state to update 
    the board after a user moves left, right, up or down. *) 
let new_board_helper f n (state:Board.t) =
  let new_board = f state in 
  let tile_board = place_random_tile n new_board in

  if compare_board state.board new_board.board then new_board
  else if board_full tile_board.board then 
    if have_lost tile_board.board then
      (print_board tile_board.board; print_endline "You lost :("; exit 0)
    else {tile_board with moves = tile_board.moves + 1}
  else spawn_powerup n {tile_board with moves = tile_board.moves + 1}

(** [play_game] is a read-eval-print-loop (REPL) that reads the user
    input, parses the input, and determines where the user will go next and if 
    their input was valid *)  
let rec play_game n (state:Board.t) = 
  print_board state.board; print_score state.score;
  match parse (read_line ()) with
  | Left -> play_game n (new_board_helper move_left n state)
  | Right -> play_game n (new_board_helper move_right n state)
  | Up -> play_game n (new_board_helper move_up n state)
  | Down -> play_game n (new_board_helper move_down n state)
  | Quit -> exit 0
  | exception Malformed -> 
    print_endline "Please enter w, a, s, or d"; play_game n state
  | exception Empty -> 
    print_endline "Please enter w, a, s, or d"; play_game n state

(** [init_board n] returns a board with n x n dimensions *) 
let init_board n =
  make_board n |> place_random_tile n |> place_random_tile n

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nThis is 2048!\n";
  print_endline "Please type w, a, s, or d and then enter to move the tiles. \n";
  play_game 4 (init_board 4)

(* executes game engine *)
let () = main ()