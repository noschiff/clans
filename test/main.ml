open OUnit2
open Clans

let data_dir = "data/test/"

let matrix_tests =
  [
    ( "transpose 2x3" >:: fun _ ->
      assert_equal
        [ [ 1.; 4. ]; [ 2.; 5. ]; [ 3.; 6. ] ]
        (Matrix.of_list [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]
        |> Matrix.transpose |> Matrix.to_list) );
  ]

let controller_tests =
  [
    ( "Empty 1x1 world test" >:: fun _ ->
      Controller.save_to_file
        (data_dir ^ "empty1x1.json")
        (Controller.init @@ Model.new_world 1 1) );
    ( "Cell 1x1 world test" >:: fun _ ->
      let state = Controller.init @@ Model.new_world 1 1 in
      Controller.random_cell state 0 0;
      Controller.step state;
      Controller.save_to_file (data_dir ^ "cell1x1.json") state;
      let nstate = Controller.init @@ Model.new_world 1 1 in
      Controller.load_from_file nstate (data_dir ^ "cell1x1.json");
      assert_equal state nstate);
  ]

let suite =
  "test suite for Clans"
  >::: List.flatten [ matrix_tests; controller_tests ]

let _ = run_test_tt_main suite
