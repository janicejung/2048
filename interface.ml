open Graphics 
(* Original Game Theme
   2:EEE4DA ; FONT: 776E65
   4: EDE0C8 ; FONT: 776E65
   8: F2B179 ; FONT: F9F6F2
   16: F59563; FONT: F9F6F2
   32: 567C5F; FONT: same as 16
   64: f65e3b
   128: EDCF72 
   256: EDCC61
   1024: EDC53F
   2048: EED22E
   empty tile: RGB (238,228,218,0.35)
   tile border: 776E65 *)
let update_screen (board : Board.t) : unit = 
  clear_graph();
  set_color (rgb 119 110 101);
  let grid_width = 200 in 
  let grid_height = 200 in
  let rect_x = (size_x()/2 - grid_height/2) in
  let rect_y = (size_y()/2 - grid_width/2) in 
  fill_rect rect_x rect_y grid_width grid_height

let rec draw_row x y tile_width padding_width n = 
  set_color red;
  if n = 1 then
    (draw_rect x y tile_width tile_width)
  else (draw_rect x y tile_width tile_width;
        draw_row (x+tile_width+padding_width) y tile_width padding_width (n-1))


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

let rec init_game_screen () : unit = 
  begin
    clear_graph ();
    draw_row 100 300 50 15 4;
  end

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
  center_text "by David Chen, Janice Jung, and Edith Vu" 50;

  let rec loop () : unit =
    (* let _ = *)
    let status = wait_next_event [Button_down] in
    if (status.mouse_x<150 + rect_x && status.mouse_x>=rect_x) && 
       (status.mouse_y< 75 + rect_y && status.mouse_y>= rect_y) then 
      init_game_screen () else loop ()
  in
  loop ()


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


