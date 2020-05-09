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

val start_screen : color_theme -> bool -> unit

val lose_screen : Board.t -> color_theme -> unit

val update_screen : Board.t -> color_theme -> int -> unit

val theme_screen : unit -> unit

val win_message : Board.t -> color_theme -> int -> unit

val draw_help_screen : unit -> unit