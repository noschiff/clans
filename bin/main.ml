open Clans

let s, p = Lwt_stream.create ()
let original_state = Controller.init p
(* Currently gives errors because model/to_json isn't implimented *)

let rec post _ =
  Lwt.bind
    (Lwt.bind (Lwt_stream.last_new s) (fun json ->
         json |> Yojson.Safe.show |> print_endline |> Lwt.return)
       (*Controller.send_something*))
    (fun x -> post x)

let _ = Lwt_main.run (post ())