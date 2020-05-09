open OUnit
open Board
open Command
open Powerup

let tboard = make_board 4

let rec compare_list l1 l2 = 
  match l1, l2 with 
  | h::t , h'::t' -> if h = h' then compare_list t t' else false
  | [], [] -> true
  | _ -> false

let compare_state s1 s2 =
  compare_board s1.board s2.board && s1.score = s2.score && s1.moves = s2.moves

let test = [
  (** make_board tests*)
  "test_make_board" >:: (fun _ -> 
      assert_equal true
        (compare_state (make_board 4) 
           ({board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
             score = 0; moves = 0};)));
  "test_make_board" >:: (fun _ -> 
      assert_equal true
        (compare_state (make_board 2)
           ({board = [[0;0];[0;0]]; score = 0; moves = 0};)));

  "test_make_board" >:: (fun _ -> 
      assert_equal true 
        (compare_state (make_board 3) 
           ({board = [[0;0;0];[0;0;0];[0;0;0]];score = 0; moves = 0};)));

  (** board_full tests*)
  "test_board_full" >:: (fun _ -> 
      assert_equal 
        (board_full [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) false;);
  "test_board_full" >:: (fun _ -> 
      assert_equal 
        (board_full [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;1]]) true;);
  "test_board_full" >:: (fun _ -> 
      assert_equal 
        (board_full [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;0]]) false;);
  "test_board_full" >:: (fun _ -> 
      assert_equal (board_full [[1]]) true;);

  (** row_empty_tiles tests*)
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [0;0;0;0] 3 []) [0;1;2;3];);
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [1;0;1;0] 3 []) [1;3];);
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [1;1] 1 []) [];);

  (** get_empty_tiles tests*)
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal 
        (get_empty_tiles 4 [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;1]]) 
        [[];[];[];[]];);
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal 
        (get_empty_tiles 3 [[5;0;10];[0;0;0];[0;1;0]]) [[1];[0;1;2];[0;2]];);

  (** place_num_in_row tests*)
  "test_place_num_in_row" >:: (fun _ -> 
      assert_equal (place_num_in_row [1;1] 1 2) [1;2];);
  "test_place_num_in_row" >:: (fun _ -> 
      assert_equal (place_num_in_row [1;1;2;3] 2 100) [1;1;100;3];);

  (** have_lost_row tests*)
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;1;2;3]) false;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;2;1;3]) true;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;1;1;1]) false;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1]) true;);

  (** transpose tests*)
  "test_transpose_1" >:: (fun _ -> 
      assert_equal 
        (transpose [[0]]) [[0]];);
  "test_transpose" >:: (fun _ -> 
      assert_equal 
        (transpose [[1;2;3];[4;5;6];[7;8;9]]) [[1;4;7];[2;5;8];[3;6;9]];);
  "test_transpose" >:: (fun _ -> 
      assert_equal 
        (transpose [[1;2;3;10];[4;5;6;0];[7;8;9;99];[0;0;0;0]]) 
        [[1;4;7;0];[2;5;8;0];[3;6;9;0];[10;0;99;0]];);

  (** have_lost tests*)
  "test_have_lost" >:: (fun _ -> 
      assert_equal (have_lost [[1;2;3];[4;5;6];[7;8;9]]) true;);
  "test_have_lost" >:: (fun _ -> 
      assert_equal (have_lost [[1;2;3];[4;5;6];[7;8;0]]) false;);


  (** compare_board tests*)
  "test_compare_board1" >:: (fun _ -> 
      assert_equal true (compare_board [[1;1;1;1]] [[1;1;1;1]]));
  "test_compare_board2" >:: (fun _ -> 
      assert_equal true (compare_board [[]] [[]]));
  "test_compare_board3" >:: (fun _ -> 
      assert_equal false (compare_board [[1]] [[]]));


  (** fill_rest tests*)
  "test_fill_rest" >:: (fun _ -> 
      assert_equal (fill_rest 4 []) [0;0;0;0]);
  "test_fill_rest" >:: (fun _ -> 
      assert_equal (fill_rest 2 [1;1]) [1;1]);

  (** combine_score tests*)
  "test_combine_score1" >:: (fun _ -> 
      assert_equal (combine_score [0;0;0;0] 4 0) 0);
  "test_combine_score2" >:: (fun _ -> 
      assert_equal (combine_score [0;0;0;2] 4 0) 0);
  "test_combine_score3" >:: (fun _ -> 
      assert_equal (combine_score [0;0;2;2] 4 0) 4);
  (** combine_left_board tests*)
  "test_combine_left1" >:: (fun _ -> 
      assert_equal (combine_left_board [2;4;8;8] 4 []) [2;4;16;0]);
  "test_combine_left2" >:: (fun _ -> 
      assert_equal (combine_left_board [0;0;0;8] 4 []) [8;0;0;0]);
  "test_combine_left3" >:: (fun _ -> 
      assert_equal (combine_left_board [0;0;0;0] 4 []) [0;0;0;0]);
  "test_combine_left4" >:: (fun _ -> 
      assert_equal (combine_left_board [2;2;2;2] 4 []) [4;4;0;0]);
  "test_combine_left5" >:: (fun _ -> 
      assert_equal (combine_left_board [2;4;4;2] 4 []) [2;8;2;0]);

  (** combine_right_board tests*)
  "test_combine_right1" >:: (fun _ -> 
      assert_equal (combine_right_board [2;4;8;8] 4 []) [0;2;4;16]);
  "test_combine_right2" >:: (fun _ -> 
      assert_equal (combine_right_board [0;0;0;8] 4 []) [0;0;0;8]);
  "test_combine_right3" >:: (fun _ -> 
      assert_equal (combine_right_board [0;0;0;0] 4 []) [0;0;0;0]);
  "test_combine_right4" >:: (fun _ -> 
      assert_equal (combine_right_board [4;4;2;8] 4 []) [0;8;2;8]);
  "test_combine_right5" >:: (fun _ -> 
      assert_equal (combine_right_board [8;8;8;8] 4 []) [0;0;16;16]);


  (** move_left tests*)
  "test_move_left1" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_left {board = [[4;2;8;16];[0;4;4;0];[0;0;0;0];[0;0;0;0]];
                       score = 0; moves = 0})
           {board = [[4;2;8;16];[8;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 8; moves = 0}));
  "test_move_left2" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_left {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                       score = 0; moves = 0}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 0}));

  (** move_right tests*)
  "test_move_right1" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_right {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                        score = 0; moves = 1}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 1}));

  (** move_up tests*)
  "test_move_up1" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_up {board = [[0;2;4;8];[2;4;4;8];[0;0;2;0];[2;0;2;0]];
                     score = 8; moves = 0}) 
           {board = [[4;2;8;16];[0;4;4;0];[0;0;0;0];[0;0;0;0]]; 
            score = 40; moves = 0}));
  "test_move_up2" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_up {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                     score = 0; moves = 0}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 0}));

  (** move_down tests*)
  "test_move_down1" >:: (fun _ -> 
      assert_equal true 
        (compare_state
           (move_down {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                       score = 0; moves = 0}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 0}));

  (** double_num tests*)
  "test_double_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (double_num [[0;2;4;8];[2;4;4;8];[0;0;2;0];[2;0;2;0]]) 
           [[0;4;8;16];[4;8;8;16];[0;0;4;0];[4;0;4;0]]));
  "test_double_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board(double_num [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));

  (** half_num tests *)  
  "test_half_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (half_num [[0;2;4;8];[2;4;4;8];[0;0;2;0];[2;0;2;0]]) 
           [[0;2;2;4];[2;2;2;4];[0;0;2;0];[2;0;2;0]]));
  "test_half_num1" >:: (fun _ -> 
      assert_equal true 
        (compare_board(half_num [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));

  "test_remove_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (remove_random_tile 
                          [[0;0;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));
]

let suite = "testing suite" >::: test

let _ = run_test_tt_main suite
