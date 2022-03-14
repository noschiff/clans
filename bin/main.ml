open Opium

let ( let* ) = Lwt.bind
(** [local_port] is the port on which the server will run. *)
let local_port = 3000

(* GET /messages *)
let request =
  App.get "/messages" (fun _request ->
      let* messages = Server.request () in
      let json = [%to_yojson: Server.message list] messages in
      Lwt.return (Response.of_json json))

(* POST /messages *)
let response =
  App.post "/messages" (fun request ->
      let* input_json = Request.to_json_exn request in
      let input_message =
        match Server.message_of_yojson input_json with
        | Ok message -> message
        | Error error -> raise (Invalid_argument error)
      in
      let* () = Server.response input_message in
      Lwt.return (Response.make ~status:`OK ()))

(* https://github.com/EduardoRFS/youtube-channel/blob/master/05-making-crud-in-ocaml/code/crud.ml
   used as inspiration*)

let app : Opium.App.t =
  App.(empty
  |> cmd_name "#clans;; backend"
  |> port local_port |> request |> response)

let _ =
  match App.run_command' app with
  | `Ok app ->
      Printf.sprintf "starting #clans;; server on port %i \n" local_port
      |> print_endline;
      Lwt_main.run app
  | `Error -> exit 1
  | `Not_running -> exit 0
(*https://github.com/ptwu/outbreak/blob/master/src/server.ml used as inspiration*)