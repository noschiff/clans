open Clans

let s, p = Lwt_stream.create ()

let rec recieve _ =
  Lwt.bind
    (Lwt.bind (Lwt_stream.last_new s) Controller.send_something)
    (fun x -> recieve x)

let _ =
  Controller.init p;
  recieve ()
