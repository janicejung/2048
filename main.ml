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

let blue_theme = {
  text1 = rgb 119 110 10;
  text2 = rgb 249 246 242;
  empty = rgb 232 248 255;
  two = rgb 178 222 251;
  four = rgb 189 241 229; 
  eight = rgb 93 188 244;
  sixteen = rgb 75 173 234;
  thirtytwo = rgb 13 186 177;
  sixtyfour= rgb 30 117 217;
  onetwentyeight =  rgb 7 151 222;
  twofiftysix = rgb 4 114 173;
  fivetwelve = rgb 4 90 141;
  tentwentyfour = rgb 237 197 63;
  twentyfortyeight = rgb 10 42 145;
  greater = rgb 0 0 0;
  background= rgb 4 53 93;
}

let pastel_theme = {
  text1 = black;
  text2 = black;
  empty = white;
  two = rgb 255 254 184;
  four = rgb 193 248 198;
  eight = rgb 186 245 244;
  sixteen = rgb 179 208 248;
  thirtytwo = rgb 191 193 255;
  sixtyfour= rgb 222 187 255;
  onetwentyeight =  rgb 255 185 243;
  twofiftysix = rgb 255 185 198;
  fivetwelve = rgb 255 187 166;
  tentwentyfour = rgb 255 224 164;
  twentyfortyeight = rgb 225 255 144;
  greater = rgb 187 255 254;
  background= rgb 255 237 237;
}

let dark_mode_theme = {
  text1 = rgb 119 110 10;
  text2 = rgb 249 246 242;
  empty = rgb 205 193 181;
  two = rgb 238 228 218;
  four = rgb 237 224 200;
  eight = rgb 242 177 121;
  sixteen = rgb 245 149 99;
  thirtytwo = rgb 246 124 95;
  sixtyfour= rgb 156 203 255;
  onetwentyeight =  rgb 237 207 114;
  twofiftysix = rgb 237 204 97;
  fivetwelve = rgb 237 200 80;
  tentwentyfour = rgb 237 197 63;
  twentyfortyeight = rgb 238 210 46;
  greater = rgb 0 0 0;
  background= rgb 119 110 101;
}

let rainbow_theme = {
  text1 = rgb 119 110 10;
  text2 = rgb 249 246 242;
  empty = rgb 205 193 181;
  two = rgb 238 228 218;
  four = rgb 237 224 200;
  eight = rgb 242 177 121;
  sixteen = rgb 245 149 99;
  thirtytwo = rgb 246 124 95;
  sixtyfour= rgb 156 203 255;
  onetwentyeight =  rgb 237 207 114;
  twofiftysix = rgb 237 204 97;
  fivetwelve = rgb 237 200 80;
  tentwentyfour = rgb 237 197 63;
  twentyfortyeight = rgb 238 210 46;
  greater = rgb 0 0 0;
  background= rgb 119 110 101;
}

let cs_theme = {
  text1 = rgb 119 110 10;
  text2 = rgb 249 246 242;
  empty = rgb 205 193 181;
  two = rgb 238 228 218;
  four = rgb 237 224 200;
  eight = rgb 242 177 121;
  sixteen = rgb 245 149 99;
  thirtytwo = rgb 246 124 95;
  sixtyfour= rgb 156 203 255;
  onetwentyeight =  rgb 237 207 114;
  twofiftysix = rgb 237 204 97;
  fivetwelve = rgb 237 200 80;
  tentwentyfour = rgb 237 197 63;
  twentyfortyeight = rgb 238 210 46;
  greater = rgb 0 0 0;
  background= rgb 119 110 101;
}

(** [init_board n] returns a board with n x n dimensions *) 
let init_board n =
  make_board n |> place_random_tile n |> place_random_tile n

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
  if board_full new_board.board then new_board else
    let tile_board = place_random_tile n new_board in

    if compare_board state.board new_board.board then new_board
    else if board_full tile_board.board then (
      if have_lost tile_board.board then
        tile_board
      else {tile_board with moves = tile_board.moves + 1}
    ) else spawn_powerup n {tile_board with moves = tile_board.moves + 1}


(** [play_game] is a read-eval-print-loop (REPL) that reads the user
    input, parses the input, and determines where the user will go next and if 
    their input was valid *)  
let rec play_game n (state:Board.t) theme =
  if have_lost state.board then lose_screen_state state theme else
    update_screen state theme;
  print_board state.board; print_score state.score;
  let status = wait_next_event [Key_pressed; Button_down] in
  if status.keypressed then
    match status.key with
    | 'w' | '\017' -> play_game n (new_board_helper move_up n state) theme
    | 'a' | '\018' -> play_game n (new_board_helper move_left n state) theme
    | 's' | '\019' -> play_game n (new_board_helper move_down n state) theme
    | 'd' | '\020' -> play_game n (new_board_helper move_right n state) theme
    | _ -> play_game n state theme
  else if status.button then
    if (status.mouse_x<77) && (status.mouse_x>=17) && 
       (status.mouse_y< 70 ) && (status.mouse_y>= 50) then exit 0 
    else play_game n state theme
  else play_game n state theme

and choose_theme () = 
  theme_screen();
  let button_height = 100 in 
  let padding = 20 in
  (* Blue Theme Button *)
  let status = wait_next_event [Button_down] in
  if (status.mouse_x<size_x()/2-button_height/2+100) && 
     status.mouse_x>=(size_x()/2-button_height/2) && 
     (status.mouse_y< size_y()/2+padding/2+100) && 
     (status.mouse_y>= (size_y()/2+padding/2)) 
  then main blue_theme

  (* Default Theme Button *)
  else if (status.mouse_x<(size_x()/2-padding-button_height/2)) && 
          status.mouse_x>=(size_x()/2-button_height-padding-button_height/2) && 
          (status.mouse_y< size_y()/2+padding/2+button_height) && 
          (status.mouse_y>= size_y()/2+padding/2) 
  then main default

  (* Pastel Theme *)
  else if (status.mouse_x<size_x()/2+padding+button_height/2+100) && 
          (status.mouse_x>=(size_x()/2+padding+button_height/2)) && 
          (status.mouse_y< size_y()/2 + padding/2 +100) && 
          (status.mouse_y>= (size_y()/2 + padding/2)) 
  then main pastel_theme

  (* Dark Mode Theme Button *)
  else if (status.mouse_x<(size_x()/2-button_height/2)+button_height) && 
          status.mouse_x>=(size_x()/2-button_height/2) && 
          (status.mouse_y< size_y()/2-padding/2) &&
          (status.mouse_y >= (size_y()/2-padding/2-button_height)) 
  then main dark_mode_theme

  (* Rainbow Theme *)
  else if (status.mouse_x < (size_x()/2-padding-button_height/2)) && 
          status.mouse_x >= (size_x()/2-button_height-padding-button_height/2) && 
          (status.mouse_y < size_y()/2-padding/2-button_height+100) && 
          (status.mouse_y >= (size_y()/2-padding/2-button_height)) then 
    main rainbow_theme

  (* CS Theme *)
  else if (status.mouse_x<size_x()/2+padding+button_height/2+100) && 
          status.mouse_x>=(size_x()/2+padding+button_height/2) && 
          (status.mouse_y< size_y()/2 - padding/2-button_height+100) && 
          (status.mouse_y>= (size_y()/2 - padding/2-button_height)) then 
    main cs_theme
  else choose_theme()


(** [main ()] prompts for the game to play, then starts it. *)
and main (theme:color_theme) =
  start_screen ();
  let rect_x = (size_x()/2)-75 in
  let rect_y = (size_y()/2)-37 in 

  let rec loop () (theme:color_theme) : unit =
    let status = wait_next_event [Button_down] in
    (* Start Game Button *)
    if (status.mouse_x<150 + rect_x) && status.mouse_x>=rect_x && 
       (status.mouse_y< 75 + rect_y) && (status.mouse_y>= rect_y) then 
      play_game 4 (init_board 4) theme 

    (* Choose A Theme Button *)
    else if (status.mouse_x<(rect_x-20) && status.mouse_x>=(rect_x-100)) && 
            (status.mouse_y< rect_y+50) && (status.mouse_y>= (rect_y-50)) 
    then choose_theme ()
    else loop () theme
  in
  loop () theme


and lose_screen_state state theme = 
  update_screen state theme;
  lose_screen state theme;
  let rec loop () (theme:color_theme) : unit =
    let status = wait_next_event [Button_down] in
    if (status.mouse_x<290 + size_x()/2) && (status.mouse_x>=size_x()/2+210) && 
       (status.mouse_y< 20 + size_y()/2) && (status.mouse_y>= size_y()/2-20) then 
      main theme
    else if (status.mouse_x<77) && (status.mouse_x>=17) && 
            (status.mouse_y< 70 ) && (status.mouse_y>= 50) then exit 0 
  in
  loop () theme


(* executes game engine *)
let () = main default