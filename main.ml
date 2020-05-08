open Board
open Command
open Interface
open Graphics

let default = {
  text1 = rgb 119 110 10;
  text2 = rgb 249 246 242;
  empty = rgb 205 193 181;
  two = rgb 238 228 218;
  four = rgb 237 224 200;
  eight = rgb 242 177 121;
  sixteen = rgb 245 149 99;
  thirtytwo = rgb 246 124 95;
  sixtyfour= rgb 246 94 59;
  onetwentyeight =  rgb 237 207 114;
  twofiftysix = rgb 237 204 97;
  fivetwelve = rgb 237 200 80;
  tentwentyfour = rgb 237 197 63;
  twentyfortyeight = rgb 238 210 46;
  greater = rgb 0 0 0;
  background= rgb 119 110 101;
}

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
let rec play_game n (state:Board.t) theme = 
  update_screen state theme;
  print_board state.board; print_score state.score;
  let status = wait_next_event [Key_pressed] in
  match status.key with
  | 'w' -> play_game n (new_board_helper move_up n state) theme
  | 'a' -> play_game n (new_board_helper move_left n state) theme
  | 's' -> play_game n (new_board_helper move_down n state) theme
  | 'd' -> play_game n (new_board_helper move_right n state) theme
  | 'q' -> exit 0
  | _ -> play_game n state theme
(* match parse (read_line ()) with
   | Left -> play_game n (new_board_helper move_left n state) theme
   | Right -> play_game n (new_board_helper move_right n state) theme
   | Up -> play_game n (new_board_helper move_up n state) theme
   | Down -> play_game n (new_board_helper move_down n state) theme
   | Quit -> exit 0
   | exception Malformed -> 
   print_endline "Please enter w, a, s, or d"; 
   | exception Empty -> 
   print_endline "Please enter w, a, s, or d"; play_game n state theme *)

(** [init_board n] returns a board with n x n dimensions *) 
let init_board n =
  make_board n |> place_random_tile n |> place_random_tile n

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  start_screen();
  let rect_x = (size_x()/2)-75 in
  let rect_y = (size_y()/2)-37 in 

  let rec loop () (theme:color_theme) : unit =
    let status = wait_next_event [Button_down] in
    if (status.mouse_x<150 + rect_x) && status.mouse_x>=rect_x && 
       (status.mouse_y< 75 + rect_y) && (status.mouse_y>= rect_y) then 
      play_game 4 (init_board 4) theme else loop () theme
  in
  loop () default

(* executes game engine *)
let () = main ()