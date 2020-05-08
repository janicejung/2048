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

let rec init_game_screen () theme: unit = 
  begin
    clear_graph ();
    let boardx = size_x()/2 - 200 in 
    let boardy = size_y()/2 - 200 in 
    make_back_board boardx boardy 400 theme;
    draw_row boardx (boardy+291) 85 12 [512;1024;128;32] theme;
    draw_row boardx (boardy+194) 85 12 [64;128;8;2048] theme;
    draw_row boardx (boardy+97) 85 12 [256;0;8;16] theme;
    draw_row boardx boardy 85 12 [2;0;4;4] theme;
  end

let update_screen (state : Board.t) (theme: color_theme) : unit = 
  begin
    clear_graph ();
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



(* draw_rect (250, 250, 50, 50);
   auto_synchronize false;
   clear_graph ();
   synchronize ();

   let x_max = size_x () in
   let y_max = size_y () in

   let orange = rgb 198 141 62 in
   set_color orange;

   (* Draw ellipse with top left corner (x,y) and width and height *)
   let fe = fun x y w h ->
   let y = y_max - y in
   fill_ellipse (x + (w / 2)) (y - (h / 2)) (w / 2) (h / 2)
   in

   (* Draw polygon using top left corner as origin *)
   let fp = fun arr ->
   fill_poly (Array.map (fun (x, y) -> (x, y_max - y)) arr)
   in

   fe 185 90 250 147; (* main body *)
   fe 269 54 68 98; (* left hump *)
   fe 143 138 127 94; (* torso *)

   set_color white;
   fe 89 (-79) 195 227; (* clipping *)

   set_color orange;
   fe 134 93 62 122; (* neck *)
   fe 97 101 86 47; (* head *)
   fe 354 63 68 118; (* right hump *)
   fe 367 101 98 109; (* rump *)
   fe 247 176 68 94; (* shoulders *)

   let front_legs = [|
   (256, 246); (249, 287); (251, 343); (264, 346); (266, 306); (276, 276);
   (282, 306); (278, 343); (292, 343); (298, 306); (298, 277); (299, 254);
   (255, 246)
   |] in fp front_legs;

   let back_legs = [|
   (432, 243); (441, 289); (430, 334); (445, 334); (462, 275); (469, 328);
   (476, 328); (478, 259); (454, 214); (461, 164); (407, 206)
   |] in fp back_legs;

   (* move the cursor and draw the text *)
   moveto 200 40;
   set_color black;
   draw_string "Bactrian the Double-Humped OCaml";

   synchronize ();

   (* loop forever *)
   let rec loop () : unit =
   let _ =
    wait_next_event [Mouse_motion; Button_down; Button_up; Key_pressed] in
   loop ()
   in
   loop () *)


