open Board
open Command

let new_board_helper f n board =
  let new_board = f board in 
  let tile_board = place_random_tile n new_board in
  if compare_board board new_board then new_board
  else if board_full tile_board then if have_lost tile_board then
      (print_board tile_board; print_endline "You lost :("; exit 0)
    else new_board else tile_board

let rec play_game n board = 
  print_board board;
  match parse (read_line ()) with
  | Left -> play_game n (new_board_helper move_left n board)
  | Right -> play_game n (new_board_helper move_right n board)
  | Up -> play_game n (new_board_helper move_up n board)
  | Down -> play_game n (new_board_helper move_down n board)
  | Quit -> exit 0
  | exception Malformed -> 
    print_endline "Please enter w, a, s, or d"; play_game n board
  | exception Empty -> 
    print_endline "Please enter w, a, s, or d"; play_game n board

let init_board n = 
  make_board n |> place_random_tile n |> place_random_tile n

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nThis is 2048!\n");
  print_endline "Please type w, a, s, or d and then enter to move the tiles. \n";
  play_game 4 (init_board 4)

(* executes game engine *)
let () = main ()