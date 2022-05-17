open OUnit2
open Clans

let data_dir = "test/"

(** [print_list lst] pretty-prints float list [lst]. Adapted from
    Cornell CS 3110 A2 Code *)
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

(** [print_matrix m] pretty-prints float list list [m]. *)
let print_matrix (m : float list list) : string =
  let rec p m =
    match m with
    | [] -> ""
    | h :: t -> print_list h ^ ";\n" ^ p t
  in
  match m with
  | [ [] ] -> "[[]]"
  | x -> "[" ^ p x ^ "]"

(** [print_brain b] pretty-prints brain [b] as a string of a json*)
let print_brain b = b |> Brain.to_json |> Yojson.Safe.to_string

let cmp_float_lists l1 l2 =
  let rec cmp a b =
    match (a, b) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> cmp_float h1 h2 && cmp t1 t2
    | _ -> failwith "lists must have equal length"
  in
  cmp l1 l2

let cmp_float_matrices l1 l2 =
  let rec cmp a b =
    match (a, b) with
    | [], [] -> true
    | h1 :: t1, h2 :: t2 -> cmp_float_lists h1 h2 && cmp t1 t2
    | _ -> failwith "matrices must have equal sizes"
  in
  cmp l1 l2

let test_matrix = Matrix.of_list [ [ 3.; 1. ]; [ 1.; 0. ] ]
let dim_matrix = Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ]

let simple_big_matrix =
  Matrix.of_list
    [
      [ 3.; -1.; -10.; -2. ];
      [ -1.; 0.; 16.; -2. ];
      [ -7.; 8.; 14.; 0. ];
    ]

let big_matrix =
  Matrix.of_list
    [
      [ 9.; 8.8; 4.5; 0.1 ];
      [ -34.; -4.4; -2.; -0.1 ];
      [ 0.2; 6.; -77.; 0.9 ];
    ]

let matrix_tests =
  [
    ( "Test list-matrix-list conversion" >:: fun _ ->
      assert_equal
        [ [ 3.; 1. ]; [ 1.; 0. ] ]
        begin
          Matrix.of_list [ [ 3.; 1. ]; [ 1.; 0. ] ]
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Matrix creation test" >:: fun _ ->
      assert_equal
        [
          [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ]; [ 0.; 0.; 0. ];
        ]
        begin
          Matrix.create 4 3 |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Matrix dimensions" >:: fun _ ->
      assert_equal (4, 3)
        begin
          Matrix.create 4 3 |> Matrix.dims
        end );
    ( "Get Matrix row" >:: fun _ ->
      assert_equal [ 1.; 0. ]
        begin
          test_matrix |> Matrix.row 1
        end );
    ( "Get Matrix col" >:: fun _ ->
      assert_equal [ 3.; 1. ]
        begin
          test_matrix |> Matrix.col 0
        end );
    ( "Get Matrix col size" >:: fun _ ->
      assert_equal 3
        begin
          dim_matrix |> Matrix.rows
        end );
    ( "Get Matrix row size" >:: fun _ ->
      assert_equal 2
        begin
          dim_matrix |> Matrix.cols
        end );
    ( "Get Matrix test element" >:: fun _ ->
      assert_equal 1.
        begin
          test_matrix |> Matrix.get 0 1
        end );
    ( "Matrix dot product test" >:: fun _ ->
      assert_equal
        [ [ 12.; 14.; 10. ]; [ 11.; 17.; 10. ]; [ 8.; 6.; 6. ] ]
        begin
          Matrix.dot
            (Matrix.of_list [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ])
            (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Attempt dot product on invalid dot matrices." >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            ignore
              begin
                Matrix.dot
                  (Matrix.of_list [ [ 2. ]; [ 1. ]; [ 2. ] ])
                  (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
                |> Matrix.to_list
              end;
            false
          with
          | Assert_failure _ -> true
        end );
    ( "Matrix map test" >:: fun _ ->
      assert_equal
        [ [ 6.; 2. ]; [ 2.; 0. ] ]
        begin
          Matrix.map (fun x -> x *. 2.) test_matrix
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Big float arithmetic matrix map test" >:: fun _ ->
      assert_equal
        [
          [ 6.; 5.8; 1.5; -2.9 ];
          [ -37.; -7.4; -5.; -3.1 ];
          [ -2.8; 3.; -80.; -2.1 ];
        ]
        begin
          Matrix.map
            (fun x ->
              if cmp_float (x /. 3.) 0. then x *. x else x -. 3.)
            big_matrix
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Matrix map2 test" >:: fun _ ->
      assert_equal
        [ [ 9.; 3. ]; [ 3.; 0. ] ]
        begin
          Matrix.map2
            (fun x y -> x +. y)
            test_matrix
            (Matrix.map (fun x -> x *. 2.) test_matrix)
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Big matrix map and map2 equivalence test" >:: fun _ ->
      assert_equal
        begin
          Matrix.map (fun a -> 3. *. a) simple_big_matrix
          |> Matrix.to_list
        end
        begin
          Matrix.map2
            (fun a b -> (2. *. a) +. b)
            simple_big_matrix simple_big_matrix
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Big matrix map2 test" >:: fun _ ->
      assert_equal
        [
          [ 12.; -10.; -15.; -2. ];
          [ -35.; 4.; 18.; -2. ];
          [ -7.; 14.; 91.; -1. ];
        ]
        begin
          Matrix.map2
            (fun a b ->
              if a *. b > 0. then Float.round (a +. b)
              else Float.round (a -. b))
            simple_big_matrix big_matrix
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Attempt map2 on unequal matrix dimensions." >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            ignore
              begin
                Matrix.map2
                  (fun x y -> x +. y)
                  (Matrix.of_list
                     [ [ 2.; 3. ]; [ 1.; 4. ]; [ 2.; 1. ] ])
                  (Matrix.of_list [ [ 3.; 1.; 2. ]; [ 2.; 4.; 2. ] ])
                |> Matrix.to_list
              end;
            false
          with
          | Assert_failure _ -> true
        end );
    ( "transpose 2x3" >:: fun _ ->
      assert_equal
        [ [ 1.; 4. ]; [ 2.; 5. ]; [ 3.; 6. ] ]
        begin
          Matrix.of_list [ [ 1.; 2.; 3. ]; [ 4.; 5.; 6. ] ]
          |> Matrix.transpose |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Transpose empty Matrix for case coverage." >:: fun _ ->
      assert_equal []
        begin
          Matrix.of_list [ [] ]
          |> Matrix.transpose |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
    ( "Large matrix dot" >:: fun _ ->
      assert_equal
        [
          [ -85.; -40.2; 781.; -8.95 ];
          [ 124.5; 87.12; 44.25; 1.05 ];
          [ -99.74; -61.56; 826.1; -9.77 ];
        ]
        begin
          Matrix.dot
            (Matrix.of_list
               [
                 [ 4.; 3.5; -10. ];
                 [ 9.3; -1.2; 0. ];
                 [ 2.; 3.4; -10.7 ];
               ])
            big_matrix
          |> Matrix.to_list
        end
        ~printer:print_matrix ~cmp:cmp_float_matrices );
  ]

let model_tests =
  [
    ( "Test world size/dimensions" >:: fun _ ->
      assert_equal (25, 25)
        begin
          Model.new_world 25 25 |> Model.get_size
        end );
    ( "Test world simulate/step when no cells are present. Should do \
       nothing."
    >:: fun _ ->
      assert_equal ()
        begin
          Model.new_world 25 25 |> Model.simulate
        end );
    ( "Place life and test if exists" >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 25 25 in
          Model.random_life world 5 5;
          Model.get_cell world 5 5 <> None
        end );
    ( "Attempt to place cell outside of world boundaries. Should \
       normalize coords and place @ 1,1"
    >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            let world = Model.new_world 25 25 in
            Model.random_life world 26 26;
            Model.get_cell world 1 1 <> None
          with
          | Model.InvalidWorldOperation (5, 5) -> true
        end );
    ( "Attempt to place life ontop of another life." >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            let world = Model.new_world 25 25 in
            Model.random_life world 5 5;
            Model.random_life world 5 5;
            Model.get_cell world 5 5 <> None
          with
          | Model.InvalidWorldOperation (5, 5) -> true
        end );
    ( "Attempt to clear a cell that has life on it." >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 25 25 in
          Model.random_life world 5 5;
          if Model.get_cell world 5 5 <> None then begin
            Model.clear_cell world 5 5;
            Model.get_cell world 5 5 = None
          end
          else false
        end );
    ( "Attempt to clear a cell that doesn't have life on it. Used for \
       coverage of the latter case for clear_cell"
    >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 25 25 in
          Model.clear_cell world 5 5;
          Model.get_cell world 5 5 = None
        end );
    ( "Attempt to simulate with one life cell. Should not crash and \
       burn."
    >:: fun _ ->
      assert_equal ()
        begin
          let world = Model.new_world 25 25 in
          Model.random_life world 5 5;
          Model.simulate world
        end );
    ( "Populate world with random cells, then simulate a few times. \
       Should not crash and burn."
    >:: fun _ ->
      assert_equal ()
        begin
          let world = Model.new_world 25 25 in
          Model.populate_random world 1.;
          Model.simulate world;
          Model.simulate world;
          Model.simulate world
        end );
    ( "Attempt to convert cell into json, then from json, then place \
       into world."
    >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 25 25 in
          Model.random_life world 5 5;
          let cell = Model.get_cell world 5 5 in
          let to_json = Model.cell_to_json cell in
          let from_json_cell = Model.cell_from_json to_json in
          match from_json_cell with
          | None -> false
          | Some l ->
              Model.set_cell world 6 6 l;
              Model.get_cell world 6 6 <> None
        end );
    ( "Attempt to convert world into json, then from json, then check \
       for proper dimensions and cell existence"
    >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 25 25 in
          Model.random_life world 5 5;
          let world_json = Model.to_json world in
          let from_json_world = Model.of_json world_json in
          if Model.get_cell world 5 5 = None then false
          else
            let size = Model.get_size from_json_world in
            match size with
            | x, y -> x = 25 && y = 25
        end );
    ( "Attempt to convert cell into json, then into a world json."
    >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            let world = Model.new_world 25 25 in
            Model.random_life world 5 5;
            let cell = Model.get_cell world 5 5 in
            let to_json = Model.cell_to_json cell in
            let _ = Model.of_json to_json in
            false
          with
          | Not_found -> true
        end );
    ( "Attempt to convert world into json, then into a life cell json."
    >:: fun _ ->
      assert_bool "didn't raise"
        begin
          try
            let world = Model.new_world 25 25 in
            Model.random_life world 5 5;
            let world_json = Model.to_json world in
            let _ = Model.cell_from_json world_json in
            false
          with
          | Yojson.Safe.Util.Type_error
              ("Expected string, got object", _) ->
              true
        end );
  ]

let controller_tests =
  [
    ( "Basic world initiate test" >:: fun _ ->
      assert_equal true
        begin
          let _ = Controller.init (Model.new_world 10 10) in
          true
        end );
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
    ( "Update cell test" >:: fun _ ->
      assert_equal true
        begin
          let world = Model.new_world 10 10 in
          Model.random_life world 5 5;
          Model.random_life world 6 6;
          match Model.get_cell world 6 6 with
          | Some l ->
              let state = Controller.init world in
              Controller.update_cell state 5 5 l;
              true
          | None -> false
        end );
    ( "Populate state test" >:: fun _ ->
      assert_equal true
        begin
          let state = Controller.init (Model.new_world 10 10) in
          Controller.populate_world state 1.;
          Controller.save_to_file
            (data_dir ^ "populated10x10.json")
            state;
          true
        end );
    ( "Full test; Initialize state of 25x25, populate world, step a \
       few times, save to file, then load a 1x1 state, then save that \
       to json. Json should only have 1 cell, and it should be empty."
    >:: fun _ ->
      assert_equal true
        begin
          let state = Controller.init (Model.new_world 25 25) in
          Controller.populate_world state 1.;
          Controller.step state;
          Controller.step state;
          Controller.step state;
          Controller.save_to_file
            (data_dir ^ "fullworld25x25.json")
            state;
          Controller.load_from_file state (data_dir ^ "empty1x1.json");
          Controller.save_to_file (data_dir ^ "fullworld1x1.json") state;
          true
        end );
  ]

(* control randomness for testing purposes*)
let random_env = 539748

let brain_tests =
  let open Brain in
  let brain1 =
    (* uses deterministic nature of Random to always test same Brain *)
    let () = Random.init 1 in
    Brain.create 18 3 5 [ 10; 10; 5 ]
  in

  let brain2 =
    (* uses deterministic nature of Random to always test same Brain *)
    let () = Random.init 2 in
    Brain.create 18 3 5 [ 10; 10; 5 ]
  in

  let custom_nn_input =
    [
      -1.;
      1.;
      -1.;
      0.;
      1.;
      0.;
      -1.;
      0.;
      0.;
      1.;
      -1.;
      0.;
      -1.;
      1.;
      -1.;
      0.;
      1.;
      0.;
    ]
  in

  let basic_brains =
    [
      ( "test brain1 creation" >:: fun _ ->
        assert_equal
          begin
            "test/brain1.json" |> Yojson.Safe.from_file
          |> Brain.from_json
          end
          brain1 ~printer:print_brain );
      ( "test brain2 creation" >:: fun _ ->
        assert_equal
          begin
            "test/brain2.json" |> Yojson.Safe.from_file
          |> Brain.from_json
          end
          brain2 ~printer:print_brain );
      ( "memory initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0.; 0.; 0. ] (mem brain1)
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "output initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0. ] (out brain1) ~printer:print_list
          ~cmp:cmp_float_lists );
      ( "verify brain1 out after eval once " >:: fun _ ->
        assert_equal
          [ -0.176992260761; -0.62368718439; -0.247871067661 ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval brain1 |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify brain1 out after eval once with nonzero input"
      >:: fun _ ->
        assert_equal
          [ -0.819648121789; -0.302316725241; -0.982542297368 ]
          begin
            eval brain1 custom_nn_input
            |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify brain2 out after eval once " >:: fun _ ->
        assert_equal
          [ 0.980041660881; 0.982346676272; -0.999987950676 ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval brain2 |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify brain1 mem after eval once " >:: fun _ ->
        assert_equal
          [
            -0.536795060272;
            -0.0363876789966;
            0.980027277103;
            0.64740625151;
            0.997570951552;
          ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval brain1 |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify brain1 mem after eval once with nonzero input"
      >:: fun _ ->
        assert_equal
          [
            -0.998644873668;
            -0.592763477777;
            0.469106736655;
            0.892021834837;
            0.403483291783;
          ]
          begin
            eval brain1 custom_nn_input
            |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify brain2 mem after eval once " >:: fun _ ->
        assert_equal
          [
            -0.992130005065;
            -0.19658285145;
            -0.434309339932;
            -0.99930975995;
            0.988446020584;
          ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval brain2 |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
    ]
  in

  let mutated_tests =
    [
      ( "test mutated brain creation" >:: fun _ ->
        assert_equal
          begin
            "test/mutated1.json" |> Yojson.Safe.from_file
          |> Brain.from_json
          end
          begin
            Random.init random_env;
            mutate ~r:0. default_params brain1
          end
          ~printer:print_brain );
      ( "mutate memory initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0.; 0.; 0. ]
          begin
            mutate default_params brain1
            |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "mutate output initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0. ]
          begin
            mutate default_params brain1
            |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "eval brain1 mutated param=0 " >:: fun _ ->
        assert_equal
          [ 0.896572344798; 0.896572344798; 0.896572344798 ]
          begin
            let mutated = mutate ~r:0. ~g:0.25 default_params brain1 in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "eval brain1 mutated param=swap_chance " >:: fun _ ->
        assert_equal
          [ 0.999909913634; -0.462594099587; 0.995201312081 ]
          begin
            let mutated =
              mutate ~r:default_params.swap_chance ~g:0.25
                default_params brain1
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "brain1 mutated param=swap_chance equals param=a little above \
         swap_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            mutate
              ~r:(default_params.swap_chance +. 0.001)
              default_params brain1
          end
          begin
            Random.init random_env;
            mutate ~r:default_params.swap_chance default_params brain1
          end
          ~printer:print_brain );
      ( "eval brain1 mutated param=swap_chance equals param=a little \
         above swap_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            let mutated =
              mutate
                ~r:(default_params.swap_chance +. 0.001)
                default_params brain1
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          begin
            Random.init random_env;
            let mutated =
              mutate ~r:default_params.swap_chance default_params brain1
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "brain1 mutated param=1 equals param=1 - swap_chance - \
         mutate_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            mutate ~r:1. default_params brain1
          end
          begin
            Random.init random_env;
            mutate
              ~r:
                begin
                  1. -. default_params.swap_chance
                -. default_params.mutate_chance
                end
              default_params brain1
          end
          ~printer:print_brain );
      ( "eval brain1 mutated param=1 equals param=1 - swap_chance - \
         mutate_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            let mutated = mutate ~r:1. default_params brain1 in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          begin
            Random.init random_env;
            let mutated =
              mutate
                ~r:
                  begin
                    1. -. default_params.swap_chance
                  -. default_params.mutate_chance
                  end
                default_params brain1
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "brain1 mutated param=0 equals param=a little below swap_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            mutate
              ~r:(default_params.swap_chance -. 0.001)
              default_params brain1
          end
          begin
            Random.init random_env;
            mutate ~r:0. default_params brain1
          end
          ~printer:print_brain );
      ( "eval brain1 mutated param=0 equals param=a little below \
         swap_chance"
      >:: fun _ ->
        assert_equal
          begin
            Random.init random_env;
            let mutated =
              mutate
                ~r:(default_params.swap_chance -. 0.001)
                default_params brain1
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          begin
            Random.init random_env;
            let mutated = mutate ~r:0. default_params brain1 in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "eval out repeated mutation brain1 param=swap_chance, varying \
         gaussians"
      >:: (* param will cause gaussian value to matter *)
      fun _ ->
        assert_equal
          [ 0.999995490482; -0.498309769503; 0.999865293139 ]
          begin
            let mutated =
              List.fold_left
                (fun b g ->
                  mutate ~r:default_params.swap_chance ~g default_params
                    b)
                brain1
                [ -0.1; 0.1; 0.25; 0.2 ]
            in
            begin
              (fun _ -> 0.)
              |> List.init 18 |> eval mutated |> out
            end
          end
          ~printer:print_list ~cmp:cmp_float_lists );
    ]
  in

  let mated_tests =
    let mated = combine 0.5 brain1 brain2 in
    [
      ( "test mated weight=.5 brain creation" >:: fun _ ->
        assert_equal
          begin
            "test/mated_point5.json" |> Yojson.Safe.from_file
          |> Brain.from_json
          end
          mated ~printer:print_brain );
      ( "mated memory initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0.; 0.; 0. ] (mem mated)
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "mated output initially empty" >:: fun _ ->
        assert_equal [ 0.; 0.; 0. ] (out mated) ~printer:print_list
          ~cmp:cmp_float_lists );
      ( "verify mated out after eval once " >:: fun _ ->
        assert_equal
          [ 0.98683663704; -0.2757826989; -0.947449814467 ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval mated |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify mated out after eval once with nonzero input"
      >:: fun _ ->
        assert_equal
          [ 0.942906688766; 0.521738867995; 0.4997127204 ]
          begin
            eval mated custom_nn_input
            |> out
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify mated mem after eval once " >:: fun _ ->
        assert_equal
          [
            -0.237873293316;
            0.186278342113;
            0.101482806737;
            -0.992857458514;
            0.710970493196;
          ]
          begin
            (fun _ -> 0.)
            |> List.init 18 |> eval mated |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
      ( "verify mated mem after eval once with nonzero input"
      >:: fun _ ->
        assert_equal
          [
            -0.0874158832528;
            -0.460894622424;
            0.312202046404;
            -0.613391503121;
            0.623319530889;
          ]
          begin
            eval mated custom_nn_input
            |> mem
          end
          ~printer:print_list ~cmp:cmp_float_lists );
    ]
  in

  List.flatten [ basic_brains; mutated_tests; mated_tests ]

let suite =
  "test suite for Clans"
  >::: List.flatten
         [ matrix_tests; model_tests; brain_tests; controller_tests ]

let _ = run_test_tt_main suite
