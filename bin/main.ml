open Opium

let ( let* ) = Lwt.bind
let local_port = 3000

(*let request _request = let* messages = Server.recieve () in let json =
  [%to_yojson: Server.message list] messages in Lwt.return
  (Response.of_json json)

  let response request = let* input_json = Request.to_json_exn request
  in let input_message = match Server.message_of_yojson input_json with
  | Ok message -> message | Error error -> raise (Invalid_argument
  error) in let* () = Server.response input_message in Lwt.return
  (Response.make ~status:`OK ())

  To be implimented properly, but temprarily commented out so it
  compiles*)

(* https://github.com/EduardoRFS/youtube-channel/blob/master/05-making-crud-in-ocaml/code/crud.ml
   used as inspiration*)

let basic request =
  "look at this I can display text" |> Response.of_plain_text
  |> Lwt.return

let app : Opium.App.t =
  App.(
    empty |> cmd_name "#clans;; backend" |> port local_port
    (*|> get "/" (Server.recieve p))

      To be implimented properly, but temprarily commented out so it
      compiles*))

let _ =
  match App.run_command' app with
  | `Ok app ->
      Printf.sprintf "starting #clans;; server on port %i \n" local_port
      |> print_endline;
      Lwt_main.run app
  | `Error -> exit 1
  | `Not_running -> exit 0
(*https://github.com/ptwu/outbreak/blob/master/src/server.ml used as
  inspiration*)
