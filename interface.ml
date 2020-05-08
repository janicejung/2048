open Graphics 
(* Original Game Theme
   2:EEE4DA: 238 228 218 ; FONT: 776E65: 119 110 101
   4: EDE0C8: 237 224 200; FONT: 776E65
   8: F2B179: 242 177 121; FONT: F9F6F2: 249 246 242
   16: F59563: 245 149 99; FONT: F9F6F2
   32: 567C5F: 246 124 95; FONT: same as 16
   64: f65e3b 246 94 59
   128: EDCF72 237 207 114
   256: EDCC61 237 204 97
   512: EDC850 237 200 80
   1024: EDC53F 237 197 63
   2048: EED22E 238 210 46
   empty tile: 205 193 181
   tile border: 776E65 119 110 101*)
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

let set_font_color theme n = 
  match n with 
  | n when n = 2 || n =4 -> set_color(theme.text1)
  | _ -> set_color (theme.text2)

let draw_num x y tile_width padding_width n theme = 
  set_font_color theme n;
  let str = string_of_int n in 
  let (length, height) = text_size str in 
  moveto (x + padding_width + (tile_width/2) - length/2)
    (y + padding_width + (tile_width/2) - (height/2));
  draw_string str

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

(* Click to go back to home? *)
let losing_screen () : unit = 
  failwith "unimplemented"

let center_text s y = 
  begin
    match text_size s with 
    | (a,b) -> let text_length = a in
      moveto (size_x()/2 - text_length/2) y;
      draw_string s;
  end

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

let draw_quit_button () =
  set_color (rgb 246 124 95); 
  fill_rect 17 50 60 20;
  let (quitx, quity) = text_size ("Quit") in
  moveto (17 + (60 - quitx)/2) (50+(20-quity)/2);
  set_color (black);
  draw_string "Quit"

let update_screen (state : Board.t) (theme: color_theme) : unit = 
  begin
    clear_graph ();
    draw_score state.score 17; 
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



(* set_color (theme.background);
   let grid_width = 200 in 
   let grid_height = 200 in
   let rect_x = (size_x()/2 - grid_height/2) in
   let rect_y = (size_y()/2 - grid_width/2) in 
   fill_rect rect_x rect_y grid_width grid_height *)

(* [start_screen] displays a "click to start game" screen. *)
let start_screen () : unit =
  open_graph "";
  moveto (200) 400;
  (* set_font "ubuntu"; *)
  set_text_size 500;
  center_text "This is 2048" 400;
  moveto 100 350;
  let orange = rgb 198 141 62 in
  set_color orange;
  let rect_x = (size_x()/2)-75 in
  let rect_y = (size_y()/2)-37 in 
  fill_rect rect_x rect_y 150 75;
  set_color black;
  center_text "Click to Start" (size_y()/2);
  center_text "by David Chen, Janice Jung, and Edith Vu" 50






