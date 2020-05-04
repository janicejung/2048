open OUnit
open Board
open Command

let tboard = make_board 4

let test = [
  (** make_board tests*)
  "test_make_board" >:: (fun _ -> 
      assert_equal (make_board 4) ({board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                                    score = 0}));
  "test_make_board" >:: (fun _ -> 
      assert_equal (make_board 2) ({board = [[0;0];[0;0]];
                                    score = 0}));
  "test_make_board" >:: (fun _ -> 
      assert_equal (make_board 3) ({board = [[0;0;0];[0;0;0];[0;0;0]];
                                    score = 0}));

  (** board_full tests*)
  "test_board_full" >:: (fun _ -> 
      assert_equal (board_full [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) false;);
  "test_board_full" >:: (fun _ -> 
      assert_equal (board_full [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;1]]) true;);
  "test_board_full" >:: (fun _ -> 
      assert_equal (board_full [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;0]]) false;);
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
      assert_equal (get_empty_tiles 4 [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;1]]) [[];[];[];[]];);
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal (get_empty_tiles 3 [[5;0;10];[0;0;0];[0;1;0]]) [[1];[0;1;2];[0;2]];);

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
  "test_transpose" >:: (fun _ -> 
      assert_equal (transpose [[1;2;3];[4;5;6];[7;8;9]]) [[1;4;7];[2;5;8];[3;6;9]];);
  "test_transpose" >:: (fun _ -> 
      assert_equal (transpose [[1;2;3;10];[4;5;6;0];[7;8;9;99];[0;0;0;0]]) [[1;4;7;0];[2;5;8;0];[3;6;9;0];[10;0;99;0]];);

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


  (** combine_left_board tests*)


  (** combine_right_board tests*)


  (** move_left tests*)


  (** move_right tests*)


  (** move_up tests*)


  (** move_down tests*)
]

let suite = "testing suite" >::: test

let _ = run_test_tt_main suite