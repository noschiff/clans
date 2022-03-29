(*
* @Author: UnsignedByte
* @Date:   2022-03-28 17:40:34
* @Last Modified by:   UnsignedByte
* @Last Modified time: 2022-03-28 17:43:41
*)
open OUnit2
open Clans

let data_dir = "data/test/"

let controller_tests = [
  "Empty 1x1 world test" >:: (fun _ -> 
    Controller.save_to_file (data_dir ^ "empty1x1.json") (Model.new_world 1 1));
]

let suite =
  "test suite for Clans"
  >::: List.flatten [ controller_tests; ]

let _ = run_test_tt_main suite
