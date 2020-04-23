open Board
open Command

let rec play_game board n = 
  print_board board;
  match parse (read_line ()) with
  | Left -> play_game (move_left board |> place_random_tile n) n
  | Right -> play_game (move_right board |> place_random_tile n) n
  | Up -> play_game (move_up board |> place_random_tile n) n
  | Down -> play_game (move_down board |> place_random_tile n) n
  | Quit -> exit 0
  | exception Malformed -> 
    print_endline "Please enter w, a, s, or d"; play_game board n
  | exception Empty -> 
    print_endline "Please enter w, a, s, or d"; play_game board n

let init_board n = 
  make_board n |> place_random_tile n |> place_random_tile n

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nThis is 2048!\n");
  print_endline "Please type w, a, s, or d and then enter to move the tiles. \n";
  play_game (init_board 4) 4

(* executes game engine *)
let () = main ()