open OUnit2
open Clans

let data_dir = "data/test/"

let rec print_list (l : float list) : string =
  match l with
  | [] -> ""
  | h :: t -> string_of_float h ^ ", " ^ print_list t

let rec print_matrix (m : float list list) : string =
  match m with
  | [] -> "[ [] ]"
  | h :: t -> "[" ^ print_list h ^ "]\n" ^ print_matrix t

let test_matrix = Matrix.of_list [ [ 3.; 1. ]; [ 1.; 0. ] ]

let matrix_tests =
  [
    ( "Test list-matrix-list conversion" >:: fun _ ->
      assert_equal
        [ [ 3.; 1. ]; [ 1.; 0. ] ]
        (Matrix.of_list [ [ 3.; 1. ]; [ 1.; 0. ] ] |> Matrix.to_list)
        ~printer:print_matrix );
    ( "Matrix creation test" >:: fun _ ->
      assert_equal
        [
          [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ];
        ]
        (Matrix.create 4 3 |> Matrix.to_list)
        ~printer:print_matrix );
    ( "Matrix dimensions" >:: fun _ ->
      assert_equal (4, 3) (Matrix.create 4 3 |> Matrix.dims) );
    ( "Get Matrix row" >:: fun _ ->
      assert_equal [ 1.; 0. ] (test_matrix |> Matrix.row 1) );
    ( "Get Matrix col" >:: fun _ ->
      assert_equal [ 3.; 1. ] (test_matrix |> Matrix.col 0) );
    ( "Get Matrix test element" >:: fun _ ->
      assert_equal 1. (test_matrix |> Matrix.get 0 1) );
    ( "Matrix dot product test" >:: fun _ ->
      assert_equal
        [ [ 12.; 14.; 10. ]; [ 11.; 17.; 10. ]; [ 8.; 6.; 6. ] ]
        (Matrix.dot
           (Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ])
           (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
        |> Matrix.to_list)
        ~printer:print_matrix );
    ( "Matrix map test" >:: fun _ ->
      assert_equal
        [ [ 6.; 2. ]; [ 2.; 0. ] ]
        (Matrix.map (fun x -> x *. 2.) test_matrix |> Matrix.to_list)
        ~printer:print_matrix );
    ( "Matrix map2 test" >:: fun _ ->
      assert_equal
        [ [ 9.; 3. ]; [ 3.; 0. ] ]
        (Matrix.map2
           (fun x y -> x +. y)
           test_matrix
           (Matrix.map (fun x -> x *. 2.) test_matrix)
        |> Matrix.to_list)
        ~printer:print_matrix );
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
      assert_equal state nstate );
  ]

let suite =
  "test suite for Clans"
  >::: List.flatten [ matrix_tests; controller_tests ]

let _ = run_test_tt_main suite
