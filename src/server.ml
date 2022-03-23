open Opium

type t = Yojson.Safe.t option -> unit

let get req =
  let content = Body.to_string req.Request.body in
  Lwt.bind content (fun x ->
      x |> Yojson.Safe.from_string |> Response.of_json |> Lwt.return)

let post (push : t) req =
  let json = req |> Request.to_json_exn in
  Lwt.bind json (fun x -> Some x |> push |> Lwt.return);
  (fun _json ->
    Lwt.return
      (Response.make ~body:(Body.of_string "Received response") ()))
    json

let init (push : Yojson.Safe.t option -> unit) : t =
  begin
    match
      App.empty |> App.port 3000 |> App.cmd_name "clans"
      |> App.get "/" get
      |> App.post "/" (post push)
      |> App.run_command'
    with
    | `Ok (app : unit Lwt.t) -> Lwt_main.run app
    | `Error -> exit 1
    | `Not_running -> exit 0
  end;
  push
