open Opium

type t = Yojson.Safe.t option -> unit

let post_cell req =
  let length = Body.length req.Request.body in
  let content = Body.to_stream req.Request.body in
  let body = content in
  Response.make ~body:(Body.of_stream ?length body) ()
  |> Response.add_header ("Access-Control-Allow-Origin", "*")
  |> Lwt.return

let get req =
  Response.of_json (`Assoc [ ("message", `String "Aite") ])
  |> Response.add_header ("Access-Control-Allow-Origin", "*")
  |> Lwt.return

let post (push : t) req =
  let json = req |> Request.to_json_exn in
  ignore @@ Lwt.bind json (fun x -> Some x |> push |> Lwt.return);
  (fun _json ->
    Lwt.return
      (Response.make ~body:(Body.of_string "Received response") ()))
    json

let init (push : Yojson.Safe.t option -> unit) =
  App.empty |> App.port 3000 |> App.get "/get" get
  |> App.post "/post_cell" post_cell
  |> App.run_command
