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
let dim_matrix = Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ]

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
    ( "Get Matrix col size" >:: fun _ ->
      assert_equal 3 (dim_matrix |> Matrix.rows) );
    ( "Get Matrix row size" >:: fun _ ->
      assert_equal 2 (dim_matrix |> Matrix.cols) );
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
    ( "Attempt dot product on invalid dot matrices. Should fail by \
       assertion."
    >:: fun _ ->
      assert_equal
        [ [ 12.; 14.; 10. ]; [ 11.; 17.; 10. ]; [ 8.; 6.; 6. ] ]
        (Matrix.dot
           (Matrix.of_list [ [ 2. ]; [ 1. ]; [ 2. ] ])
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
    ( "Attempt map2 on unequal matrix dimensions. Should fail by \
       assertion."
    >:: fun _ ->
      assert_equal
        [ [ 12.; 14.; 10. ]; [ 11.; 17.; 10. ]; [ 8.; 6.; 6. ] ]
        (Matrix.map2
           (fun x y -> x +. y)
           (Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ])
           (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
        |> Matrix.to_list)
        ~printer:print_matrix );
    ( "transpose 2x3" >:: fun _ ->
      assert_equal
        [ [ 1.; 4. ]; [ 2.; 5. ]; [ 3.; 6. ] ]
        (Matrix.of_list [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]
        |> Matrix.transpose |> Matrix.to_list) );
    ( "Transpose empty Matrix for case coverage." >:: fun _ ->
      assert_equal []
        (Matrix.of_list [ [] ] |> Matrix.transpose |> Matrix.to_list) );
  ]

let model_tests =
  [
    ( "Test world size/dimensions" >:: fun _ ->
      assert_equal (25, 25) (Model.new_world 25 25 |> Model.get_size) );
    ( "Test world simulate/step when no cells are present. Should do \
       nothing."
    >:: fun _ ->
      assert_equal () (Model.new_world 25 25 |> Model.simulate) );
    ( "Place life and test if exists" >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         Model.get_cell world 5 5 != None) );
    ( "Attempt to place cell outside of world boundaries. Should \
       normalize coords and place @ 1,1"
    >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 26 26;
         Model.get_cell world 1 1 != None) );
    ( "Attempt to place life ontop of another life. Should error"
    >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         Model.random_life world 5 5;
         Model.get_cell world 5 5 != None) );
    ( "Attempt to clear a cell that has life on it." >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         if Model.get_cell world 5 5 != None then (
           Model.clear_cell world 5 5;
           Model.get_cell world 5 5 == None)
         else false) );
    ( "Attempt to clear a cell that doesn't have life on it. Used for \
       coverage of the latter case for clear_cell"
    >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.clear_cell world 5 5;
         Model.get_cell world 5 5 == None) );
    ( "Attempt to simulate with one life cell. Should not crash and \
       burn."
    >:: fun _ ->
      assert_equal ()
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         Model.simulate world) );
    ( "Populate world with random cells, then simulate a few times. \
       Should not crash and burn."
    >:: fun _ ->
      assert_equal ()
        (let world = Model.new_world 25 25 in
         Model.populate_random world 1.;
         Model.simulate world;
         Model.simulate world;
         Model.simulate world) );
    ( "Attempt to convert cell into json, then from json, then place \
       into world."
    >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         let cell = Model.get_cell world 5 5 in
         let to_json = Model.cell_to_json cell in
         let from_json_cell = Model.cell_from_json to_json in
         match from_json_cell with
         | None -> false
         | Some l ->
             Model.set_cell world 6 6 l;
             Model.get_cell world 6 6 != None) );
    ( "Attempt to convert world into json, then from json, then check \
       for proper dimensions and cell existence"
    >:: fun _ ->
      assert_equal true
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         let world_json = Model.to_json world in
         let from_json_world = Model.of_json world_json in
         if Model.get_cell world 5 5 == None then false
         else
           let size = Model.get_size from_json_world in
           match size with
           | x, y -> if x != 25 || y != 25 then false else true) );
    ( "Attempt to convert cell into json, then into a world json. \
       Should error."
    >:: fun _ ->
      assert_equal ()
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         let cell = Model.get_cell world 5 5 in
         let to_json = Model.cell_to_json cell in
         let _ = Model.of_json to_json in
         ()) );
    ( "Attempt to convert world into json, then into a life cell json. \
       Should error."
    >:: fun _ ->
      assert_equal ()
        (let world = Model.new_world 25 25 in
         Model.random_life world 5 5;
         let world_json = Model.to_json world in
         let _ = Model.cell_from_json world_json in
         ()) );
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
  >::: List.flatten [ matrix_tests; model_tests; controller_tests ]

let _ = run_test_tt_main suite
