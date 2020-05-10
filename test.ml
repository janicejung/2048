(* Test Plan:
    What was tested in OUnit:
        Functions Tested:
   - make_board 
   - board_full
   - row_empty_tiles
   - get_empty_tiles
   - place_num_in_row
   - have_lost_row
   - transpose
   - have_lost
   - fill_rest
   - combine_score
   - combine_left
   - combine_right
   - move_left
   - move_right
   - move_up
   - move_down
   - double_num
   - half_num
   - remove_random_tile
   - sort_row
     These functions were specifically tested in OUnit using an int list list 
     board to test whether these functions were producing the right board to 
     be displayed on the screen. These functions were able to be tested by OUnit
     since they did not rely on the random generation of numbers that our other
     functions used. 

    What was tested Manually:
    A lot of our game depends on random generation of numbers and tiles which 
    we could not test with OUnit. We tested these essential functionalities to 
    our game manually by playing our game extensively. The random 
    functionalities we tested in particular were the spawning of a random 
    number with each move, the random location in which the new number was 
    spawned in, the random spawn timing of a powerup after a certain 
    number of moves, the random choice of a powerup when one is spawned, and
    the random location in which the powerup was spawed. During MS1 we printed
    the grid with every move into the terminal to test and during MS2 we used
    our GUI.

    We also tested our GUI manually: we manually played the game to test our 
    'w, a, s, d' keypress and mouse button press functionalities.

    How we developed our tests:
    As we developed our test cases, we used the glass box testing approach. 
    This approach was used since we understood the implementation and structure
    of our code and what each function was supposed to output. After documenting
    our specifications and implementing functions, we tested functions manually 
    and through OUnit to ensure that our function implementation was correct. 
    In doing so, we were able to proactively modify our code to carry out the 
    purpose and functionality of our project.

    The modules that were tested by OUnit were Board, Command, and Powerup. 
    Testing for the Board Module verified if our functions correctly initialized
    and updated the board after a player moves left, right, up, or down. 
    Testing for the Command Module verified if our functions correctly combine
    like tiles and update the board after a player makes a move. This Command
    module also tested to see if scores were added up correctly.
    Testing for the Powerup Module verified if the powerups we made (DoubleNum,
    HalfNum, SortRow, Shuffle, RemoveRandomTile) correctly updated the board 
    after a user swiped onto the powerup.

    Why our test suite demonstrates correctness of system:
    Our OUnit tests ensure that the basic functionalities of our 2048 game, 
    such as the combining of two tiles and the up, down, left, and right
    movement of the tiles, are working. The helper function that we test by 
    passing in random and edge-case boards make up the foundation of the game.

    The random functions of our game also depend on function that were tested 
    in our OUnit test suite, such as [get_empty_tiles] and [place_num_in_row] 
    and because all our OUnit tests were passing, we were able to confirm that 
    our manual tests were also functioning properly instead of simply looking
    like they were working by chance.
*)

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
  (* make_board tests*)
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

  (* board_full tests*)
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

  (* row_empty_tiles tests*)
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [0;0;0;0] 3 []) [0;1;2;3];);
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [1;0;1;0] 3 []) [1;3];);
  "test_row_empty_tiles" >:: (fun _ -> 
      assert_equal (row_empty_tiles [1;1] 1 []) [];);

  (* get_empty_tiles tests*)
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal 
        (get_empty_tiles 4 [[5;3;10;1];[1;1;1;1];[1;1;1;1];[1;1;1;1]]) 
        [[];[];[];[]];);
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal 
        (get_empty_tiles 3 [[5;0;10];[0;0;0];[0;1;0]]) [[1];[0;1;2];[0;2]];);
  "test_get_empty_tiles" >:: (fun _ -> 
      assert_equal 
        (get_empty_tiles 4 [[5;0;10;0];[0;0;0;2];[1;1;1;1]]) 
        [[1;3];[0;1;2];[]];);

  (* place_num_in_row tests*)
  "test_place_num_in_row" >:: (fun _ -> 
      assert_equal (place_num_in_row [1;1] 1 2) [1;2];);
  "test_place_num_in_row" >:: (fun _ -> 
      assert_equal (place_num_in_row [1;1;2;3] 2 100) [1;1;100;3];);
  "test_place_num_in_row" >:: (fun _ -> 
      assert_equal (place_num_in_row [1;1;2;3] 0 100) [100;1;2;3];);

  (* have_lost_row tests*)
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;1;2;3]) false;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;2;1;3]) true;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1;1;1;1]) false;);
  "test_have_lost_row" >:: (fun _ -> 
      assert_equal (have_lost_row [1]) true;);

  (* transpose tests*)
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

  (* have_lost tests*)
  "test_have_lost" >:: (fun _ -> 
      assert_equal (have_lost [[1;2;3];[4;5;6];[7;8;9]]) true;);
  "test_have_lost" >:: (fun _ -> 
      assert_equal (have_lost [[1;2;3];[4;5;6];[7;8;0]]) false;);
  "test_have_lost" >:: (fun _ -> 
      assert_equal (have_lost [[2;4;8;4];[4;2;4;8];[2;4;8;2];[4;2;4;8]])true;);

  (* compare_board tests*)
  "test_compare_board1" >:: (fun _ -> 
      assert_equal true (compare_board [[1;1;1;1]] [[1;1;1;1]]));
  "test_compare_board2" >:: (fun _ -> 
      assert_equal true (compare_board [[]] [[]]));
  "test_compare_board3" >:: (fun _ -> 
      assert_equal false (compare_board [[1]] [[]]));


  (* fill_rest tests*)
  "test_fill_rest" >:: (fun _ -> 
      assert_equal (fill_rest 4 []) [0;0;0;0]);
  "test_fill_rest" >:: (fun _ -> 
      assert_equal (fill_rest 2 [1;1]) [1;1]);

  (* combine_score tests*)
  "test_combine_score1" >:: (fun _ -> 
      assert_equal (combine_score [0;0;0;0] 4 0) 0);
  "test_combine_score2" >:: (fun _ -> 
      assert_equal (combine_score [0;0;0;2] 4 0) 0);
  "test_combine_score3" >:: (fun _ -> 
      assert_equal (combine_score [0;0;2;2] 4 0) 4);

  (* combine_left_board tests*)
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

  (* combine_right_board tests*)
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

  (* move_left tests*)
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
  "test_move_left3" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_left {board = [[4;2;8;16];[0;4;4;0];[2;2;2;2];[4;4;8;2]];
                       score = 0; moves = 0})
           {board = [[4;2;8;16];[8;0;0;0];[4;4;0;0];[8;8;2;0]]; 
            score = 24; moves = 0}));

  (* move_right tests*)
  "test_move_right1" >:: (fun _ -> 
      assert_equal true 
        (compare_state 
           (move_right {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                        score = 0; moves = 1}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 1}));

  (* move_up tests*)
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

  (* move_down tests*)
  "test_move_down1" >:: (fun _ -> 
      assert_equal true 
        (compare_state
           (move_down {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]];
                       score = 0; moves = 0}) 
           {board = [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]; 
            score = 0; moves = 0}));

  (* double_num tests*)
  "test_double_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (double_num [[0;2;4;8];[2;4;4;8];[0;0;2;0];[2;0;2;0]]) 
           [[0;4;8;16];[4;8;8;16];[0;0;4;0];[4;0;4;0]]));
  "test_double_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board(double_num [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));

  (* half_num tests *)  
  "test_half_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (half_num [[0;2;4;8];[2;4;4;8];[0;0;2;0];[2;0;2;0]]) 
           [[0;2;2;4];[2;2;2;4];[0;0;2;0];[2;0;2;0]]));
  "test_half_num1" >:: (fun _ -> 
      assert_equal true 
        (compare_board(half_num [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));

  (* sort_row tests *)
  "test_sort_row" >:: (fun _ -> 
      assert_equal true 
        (compare_board(sort_row [[0;0;1;2];[4;8;0;20];[5;4;3;0];[1;2;3;4]]) 
           [[2;1;0;0];[20;8;4;0];[5;4;3;0];[4;3;2;1]]));
  "test_sort_row2" >:: (fun _ -> 
      assert_equal true 
        (compare_board(sort_row [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));
  (* remove_random_tile test *)
  "test_remove_num" >:: (fun _ -> 
      assert_equal true 
        (compare_board (remove_random_tile 
                          [[0;0;4;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]) 
           [[0;0;0;0];[0;0;0;0];[0;0;0;0];[0;0;0;0]]));
]

let suite = "testing suite" >::: test

let _ = run_test_tt_main suite
