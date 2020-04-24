open OUnit
open Board
open Command

let tboard = make_board 4

let test = [
  "test_compare_board1" >:: (fun _ -> 
      assert_equal true (compare_board [[1;1;1;1]] [[1;1;1;1]]));
  "test_compare_board2" >:: (fun _ -> 
      assert_equal true (compare_board [[]] [[]]));
  "test_compare_board3" >:: (fun _ -> 
      assert_equal false (compare_board [[1]] [[]]));
]

let suite = "testing suite" >::: test

let _ = run_test_tt_main suite