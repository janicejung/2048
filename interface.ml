open Graphics 

type color_theme = {
  text1: Graphics.color;
  text2: Graphics.color;
  empty: Graphics.color;
  two: Graphics.color;
  four: Graphics.color;
  eight: Graphics.color;
  sixteen: Graphics.color;
  thirtytwo: Graphics.color;
  sixtyfour: Graphics.color;
  onetwentyeight: Graphics.color;
  twofiftysix: Graphics.color;
  fivetwelve: Graphics.color;
  tentwentyfour: Graphics.color;
  twentyfortyeight: Graphics.color;
  greater: Graphics.color;
  background: Graphics.color;
}

(** [set_tile_color theme n] sets the background color for the tile according
    to the color of [n] in [theme]. *)
let set_tile_color theme n =
  match n with
  | 0 -> set_color(theme.empty)
  | 2 -> set_color(theme.two)
  | 4 -> set_color(theme.four)
  | 8 -> set_color(theme.eight)
  | 16 -> set_color(theme.sixteen)
  | 32 -> set_color(theme.thirtytwo)
  | 64 -> set_color(theme.sixtyfour)
  | 128 -> set_color(theme.onetwentyeight)
  | 256 -> set_color(theme.twofiftysix)
  | 512 -> set_color(theme.fivetwelve)
  | 1024 -> set_color(theme.tentwentyfour)
  | 2048 -> set_color(theme.twentyfortyeight)
  | n when n > 2048 -> set_color(theme.greater)
  | _ -> set_color red

(** [set_font_color theme n] sets the color of the font on the tile according
    to the color of [n] in [theme]. *)
let set_font_color theme n = 
  match n with 
  | n when n = 2 || n =4 -> set_color(theme.text1)
  | _ -> set_color (theme.text2)


let draw_num_helper x y tile_width padding_width str = 
  let (length, height) = text_size str in 
  moveto (x + padding_width + (tile_width/2) - length/2)
    (y + padding_width + (tile_width/2) - (height/2));
  draw_string str

(** [draw_num x y tile_width padding_width n theme] draws the number [n] on
    the tile at [(x,y)] with the [tile_width] and [padding_width] according
    to the [theme]. *)
let draw_num x y tile_width padding_width n theme = 
  set_font_color theme n;
  if n mod 2 <> 0 then
    match n with 
    | 3 -> draw_num_helper x y tile_width padding_width "Double"
    | 5 -> draw_num_helper x y tile_width padding_width "Half"
    | 7 -> draw_num_helper x y tile_width padding_width "Sort"
    | 11 -> draw_num_helper x y tile_width padding_width "Shuffle"
    | 13 -> draw_num_helper x y tile_width padding_width "Remove"
    | _-> failwith "not a powerup"
  else
    let str = string_of_int n in 
    draw_num_helper x y tile_width padding_width str

(** [draw_row x y tile_width padding_width lst theme] draws the tiles in the
    row according to the numbers in [lst] with the colors in [theme] at
    [(x,y)] with size of [tile_width] and distance between each tile as
    [padding_width]. *)
let rec draw_row x y tile_width padding_width lst theme= 
  match lst with 
  | h::t -> set_tile_color theme h;
    fill_rect (x+padding_width) (y+padding_width) tile_width tile_width;
    if h <>0 then
      draw_num x y tile_width padding_width h theme;
    draw_row (x+tile_width+padding_width) y tile_width padding_width t theme;
  | [] -> ()

(** [make_back_board] creates the background of the 2048 game of dimensions 
    n x n including the padding width with the bottom left corner at position
    (x,y). *)
let make_back_board x y n theme =
  begin
    set_color (theme.background);
    fill_rect x y n n
  end

(** [center_text s y] draws [s] in the middle of the graph the height [y]. *)
let center_text s y = 
  begin
    match text_size s with 
    | (a,b) -> let text_length = a in
      moveto (size_x()/2 - text_length/2) y;
      draw_string s;
  end

(** [draw_score int x] draws the score, [int], at location [x] on the screen. *)
let draw_score int x = 
  set_color (black);
  let (scorex, _) = text_size "Score:" in 
  moveto (x + (60 - scorex)/2) 375;
  draw_string "Score:";
  moveto x 350;
  set_color(black);
  fill_rect x 350 60 20;
  set_color (white);
  let (numx, numy) = text_size (string_of_int int) in 
  moveto (x + (60 - numx)/2) (350 + (20 - numy)/2);
  draw_string (string_of_int int)

(** [draw_score int x] draws the score, [int], at location [x] on the screen. *)
let draw_high_score int x = 
  set_color (black);
  let (scorex, _) = text_size "High Score:" in 
  moveto (x + (60 - scorex)/2) 325;
  draw_string "High Score:";
  moveto x 300;
  set_color(black);
  fill_rect x 300 60 20;
  set_color (white);
  let (numx, numy) = text_size (string_of_int int) in 
  moveto (x + (60 - numx)/2) (300 + (20 - numy)/2);
  if int = 0 then draw_string "" else draw_string (string_of_int int)

(** [draw_quit_button ()] draws a quit button on the screen. *)
let draw_quit_button () =
  set_color (rgb 246 124 95); 
  fill_rect 17 50 60 20;
  let (quitx, quity) = text_size ("Quit") in
  moveto (17 + (60 - quitx)/2) (50+(20-quity)/2);
  set_color (black);
  draw_string "Quit"

(** [update_screen state theme] draws the new board and score according to
    [state] and [theme]. *)
let update_screen (state : Board.t) (theme: color_theme) highscore : unit = 
  begin
    clear_graph ();
    draw_score state.score 17; 
    draw_high_score highscore 17;
    draw_quit_button ();
    let boardx = size_x()/2 - 200 in 
    let boardy = size_y()/2 - 200 in 
    make_back_board boardx boardy 400 theme;
    let rec helper board x y tile padding =
      match board with 
      | [] -> ()
      | h::t -> draw_row x y tile padding h theme;
        helper t x (y+tile+padding) tile padding; in
    helper (List.rev state.board) boardx boardy 85 12;
  end

(** [draw_start_message theme] draws the "This is 2048" message with colors
    corresponding to [theme]. *)
let draw_start_message theme : unit =
  set_color black;
  center_text "This is" 350;
  let two_x = 268 in
  set_color theme.sixteen;
  moveto two_x 320;
  draw_string "2";
  set_color theme.sixtyfour;
  moveto (two_x+20) 320;
  draw_string "0";
  set_color theme.fivetwelve;
  moveto (two_x+40) 320;
  draw_string "4";
  set_color theme.twentyfortyeight;
  moveto (two_x+60) 320;
  draw_string "8"

(** [draw_start_button theme rect_x rect_y] draws the start button according
    to [theme] at location of [rect_x] and [rect_y]. *)
let draw_start_button theme rect_x rect_y=
  set_color theme.thirtytwo;
  fill_rect rect_x rect_y 150 75;
  if theme.thirtytwo = black then set_color white else set_color black;
  let (startx, starty) = text_size "Click to Start" in
  center_text "Click to Start" (rect_y + 37 - starty/2)

(** [draw_powerup powerup rect_y] draws the button with size [rect_y] and text
    according to if [powerup] is true or false. *)
let draw_powerup powerup rect_y =
  if powerup then set_color (rgb 0 204 20) else set_color red;
  fill_rect (size_x()/2 + 15) (rect_y-70) 110 50;
  set_color white;
  let (tog_len, tog_height) = text_size "Toggle Powerups" in 
  moveto (size_x()/2 + 70 - tog_len/2) (rect_y-44);
  draw_string "Toggle Powerups";
  let (p_len, p_height) = text_size "Powerups:   " in 
  moveto (size_x()/2 + 70 - p_len/2 - 3) (rect_y-50-10);
  draw_string "Powerups:";
  moveto (size_x()/2 + 70 - p_len/2 + 57)(rect_y-50-10);
  if powerup then draw_string "ON" else draw_string "OFF"

(** [start_screen theme powerup] displays a "click to start game" screen
    and allows the user to change [theme] and toggle [powerup]. *)
let start_screen (theme:color_theme) (powerup:bool) : unit =
  open_graph "";
  draw_start_message theme;

  let rect_x = (size_x()/2)-75 in
  let rect_y = (size_y()/2)-27 in 
  (* Click to start button *)
  draw_start_button theme rect_x rect_y;

  (* Choose a Theme Button *)
  set_color theme.four;
  fill_rect (size_x()/2 - 110 - 15) (rect_y-70) 110 50;
  if theme.four = black then set_color white else set_color theme.text1;
  let (t_len, t_height) = text_size "Choose Theme" in 
  moveto (size_x()/2 - 110 - 15 + 55 - t_len/2) (rect_y-50-2);
  draw_string "Choose Theme";

  (* Powerups Button *)
  draw_powerup powerup rect_y;

  set_color black;
  center_text "by David Chen, Janice Jung, and Edith Vu" 50

(** [draw_play_again button_length button_height] draws the "Play Again" text
    on the button according to the [button_length] and [button_height]. *)
let draw_play_again button_length button_height =
  set_color (rgb 180 200 120); 
  fill_rect (size_x()/2 + 210) (size_y()/2 - button_height/2) 
    button_length button_height;
  set_color (white);
  let (play_length, play_height) = text_size "Play Again" in
  moveto (250 + (size_x()/2 - play_length/2)) (size_y()/2 - play_height/2); 
  draw_string "Play Again"

(** [lose_screen state theme] draws the lose message on the screen with the
    losing board according to [state] and [theme]. *)
let lose_screen (state: Board.t) theme : unit =
  let button_length = 80 in
  let button_height = 40 in
  (* draws "You Lose" *)
  set_color black;
  moveto (size_x()/2 + 225) (size_y()/2 +button_height/2 + 15);
  draw_string "You lost!";

  (* draws play again button *)
  draw_play_again button_length button_height

let win_message state theme highscore = 
  update_screen state theme highscore;
  set_color green;
  moveto (size_x()/2 + 218) 385;
  draw_string "Congrats!";
  set_color black;
  moveto (size_x()/2 + 218) 355;
  draw_string "You reached";
  moveto (size_x()/2 + 218) 340;
  draw_string "2048.";
  moveto (size_x()/2 + 218) 325;
  draw_string "Keep up the ";
  moveto (size_x()/2 + 218) 310;
  draw_string "good work!"

(** [draw_theme_tile x y height c1 c2 c3 c4] draws the tile for the theme
    at [(x, y)] with colors [c1], [c2], [c3], and [c4] with the same
    [height]. *)
let draw_theme_tile x y height c1 c2 c3 c4 =
  set_color c1;
  fill_rect x y height (height/4);
  set_color c2;
  fill_rect x (y+(height/4)) height (height/4);
  set_color c3;
  fill_rect x (y+(2*height/4)) height (height/4);
  set_color c4;
  fill_rect x (y+(3*height/4)) height (height/4)

(** [theme_screen ()] draws the theme selection screen. *)
let theme_screen () : unit = 
  clear_graph();
  set_color black;
  center_text "Click on a tile to choose a color theme!" 370;
  let button_height = 100 in
  let padding = 20 in

  (* blue theme *)
  draw_theme_tile 
    (size_x()/2-button_height/2) (size_y()/2+padding/2) button_height 
    (rgb 10 42 145) (rgb 4 114 173) (rgb 75 173 234) (rgb 178 222 251);

  (* default theme *)
  draw_theme_tile 
    (size_x()/2-button_height-padding-button_height/2) (size_y()/2+padding/2)
    button_height 
    (rgb 237 197 63) (rgb 246 94 59) (rgb 245 149 99) (rgb 237 224 200);

  (* pastel theme *)
  draw_theme_tile 
    (size_x()/2+padding+button_height/2) (size_y()/2 + padding/2) button_height 
    (rgb 225 255 144) (rgb 255 185 198) (rgb 186 245 244) (rgb 255 254 184);

  (* dark mode theme *)
  draw_theme_tile 
    (size_x()/2-button_height/2) (size_y()/2-padding/2-button_height)
    button_height (rgb 2 143 163) (rgb 98 93 82) (rgb 34 34 34) (rgb 97 97 97);

  (* rainbow theme *)
  draw_theme_tile 
    (size_x()/2-button_height-padding-button_height/2) 
    (size_y()/2-padding/2-button_height) button_height 
    (rgb 77 111 247) (rgb 29 189 73) (rgb 255 232 0) (rgb 255 65 65);

  (* 3110 theme? *)
  draw_theme_tile 
    (size_x()/2+padding+button_height/2) (size_y()/2 - padding/2-button_height)
    button_height black black black black;









