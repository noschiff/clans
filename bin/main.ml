open Clans

let s, p =
  print_endline "Created stream and push function";
  Lwt_stream.create ()

let original_state =
  print_endline "Called controller.init";
  let x = Controller.init p in
  print_endline "Completed controller.init";
  x

let testing =
  print_endline "testing called";
  3

let rec post _ =
  print_endline "post called";
  Lwt.bind
    (Lwt.bind (Lwt_stream.last_new s) (fun json ->
         json |> Yojson.Safe.show |> print_endline |> Lwt.return)
       (*Controller.send_something*))
    (fun x -> post x)

let dummy =
  let _dummy2 = print_endline "main called" in
  let dummy3 =
    Lwt_main.run (Lwt.both (Server.init p |> Lwt.return) (post ()))
  in
  print_endline "main wrapup (should not be called while running)";
  dummy3
