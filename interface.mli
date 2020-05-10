(** Creation of the graphical user interface. *)

(** [color_theme] is the color schedule for the GUI. *)
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

(** [start_screen theme powerup] displays a "click to start game" screen
    and allows the user to change [theme] and toggle [powerup]. *)
val start_screen : color_theme -> bool -> unit

(** [lose_screen state theme] draws the lose message on the screen with the
    losing board according to [state] and [theme]. *)
val lose_screen : Board.t -> color_theme -> unit

(** [update_screen state theme] draws the new board and score according to
    [state] and [theme]. *)
val update_screen : Board.t -> color_theme -> int -> unit

(** [theme_screen ()] draws the theme selection screen. *)
val theme_screen : unit -> unit

(** [win_message state theme highscore] draws the win message on the GUI. *)
val win_message : Board.t -> color_theme -> int -> unit

(** [draw_help_screen ()] draws the screen where the user can look for 
    instructions to play the game. *)
val draw_help_screen : unit -> unit