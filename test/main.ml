open OUnit2
open Clans

let data_dir = "data/test/"

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. Adapted from Cornell CS 3110 A2
    Code *)
let print_list lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ string_of_float h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ string_of_float h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let rec print_matrix (m : float list list) : string =
  match m with
  | [] -> "[ [] ]"
  | h :: t -> "[" ^ print_list h ^ "]\n" ^ print_matrix t

let cmp_float_lists l1 l2 =
  let rec cmp a b =
    match (a, b) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> cmp_float h1 h2 && cmp t1 t2
    | _ -> failwith "lists must have equal length"
  in
  cmp l1 l2

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
    ( "Attempt dot product on invalid dot matrices." >:: fun _ ->
      assert_bool "didn't raise"
        (try
           ignore
             (Matrix.dot
                (Matrix.of_list [ [ 2. ]; [ 1. ]; [ 2. ] ])
                (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
             |> Matrix.to_list);
           false
         with
        | Assert_failure _ -> true) );
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
    ( "Attempt map2 on unequal matrix dimensions." >:: fun _ ->
      assert_bool "didn't raise"
        (try
           ignore
             (Matrix.map2
                (fun x y -> x +. y)
                (Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ])
                (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
             |> Matrix.to_list);
           false
         with
        | Assert_failure _ -> true) );
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
      assert_bool "didn't raise"
        (try
           let world = Model.new_world 25 25 in
           Model.random_life world 26 26;
           Model.get_cell world 1 1 != None
         with
        | Model.InvalidWorldOperation (5, 5) -> true) );
    ( "Attempt to place life ontop of another life." >:: fun _ ->
      assert_bool "didn't raise"
        (try
           let world = Model.new_world 25 25 in
           Model.random_life world 5 5;
           Model.random_life world 5 5;
           Model.get_cell world 5 5 != None
         with
        | Model.InvalidWorldOperation (5, 5) -> true) );
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
    ( "Attempt to convert cell into json, then into a world json."
    >:: fun _ ->
      assert_bool "didn't raise"
        (try
           let world = Model.new_world 25 25 in
           Model.random_life world 5 5;
           let cell = Model.get_cell world 5 5 in
           let to_json = Model.cell_to_json cell in
           let _ = Model.of_json to_json in
           false
         with
        | Not_found -> true) );
    ( "Attempt to convert world into json, then into a life cell json."
    >:: fun _ ->
      assert_bool "didn't raise"
        (try
           let world = Model.new_world 25 25 in
           Model.random_life world 5 5;
           let world_json = Model.to_json world in
           let _ = Model.cell_from_json world_json in
           false
         with
        | Yojson.Safe.Util.Type_error ("Expected string, got object", _)
          ->
            true) );
  ]

(* TODO: FIX! *)
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

let brain1 =
  "test/brain1.json" |> Yojson.Safe.from_file |> Brain.from_json

let brain2 =
  "test/brain2.json" |> Yojson.Safe.from_file |> Brain.from_json

let brain_tests =
  let open Brain in
  [
    ( "memory initially empty" >:: fun _ ->
      assert_equal [ 0.; 0.; 0.; 0.; 0. ] (mem brain1)
        ~printer:print_list );
    ( "output initially empty" >:: fun _ ->
      assert_equal [ 0.; 0.; 0. ] (out brain1) ~printer:print_list );
    ( "verify brain1 out after eval once " >:: fun _ ->
      assert_equal
        [ -0.89607323904; 0.937054971136; -0.323411523155 ]
        (out (eval brain1 (List.init 18 (fun _ -> 0.))))
        ~printer:print_list ~cmp:cmp_float_lists );
    ( "verify brain2 out after eval once " >:: fun _ ->
      assert_equal
        [ 0.448421709068; 0.0513718654537; 0.880462842882 ]
        (out (eval brain2 (List.init 18 (fun _ -> 0.))))
        ~printer:print_list ~cmp:cmp_float_lists );
    ( "verify brain1 mem after eval once " >:: fun _ ->
      assert_equal
        [
          -0.638279691527;
          0.978796493438;
          0.997659762324;
          -0.856745179127;
          -0.30613198672;
        ]
        (mem (eval brain1 (List.init 18 (fun _ -> 0.))))
        ~printer:print_list ~cmp:cmp_float_lists );
    ( "verify brain2 mem after eval once " >:: fun _ ->
      assert_equal
        [
          -0.932699314564;
          -0.397569800131;
          -0.972648328583;
          0.990850373388;
          -0.981339770252;
        ]
        (mem (eval brain2 (List.init 18 (fun _ -> 0.))))
        ~printer:print_list ~cmp:cmp_float_lists );
  ]

let suite =
  "test suite for Clans"
  >::: List.flatten [ matrix_tests; model_tests; brain_tests ]

let _ = run_test_tt_main suite
