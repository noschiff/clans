open Opium

type t = Yojson.Safe.t option -> unit

(** Posts cell to stream to be dealt with later, and returns a message
    with whether the json was successfully parsed back to the client. *)
let post t push req =
  let content = Body.to_string req.Request.body in
  let content_stream = Body.to_stream req.Request.body in
  let _body = content_stream in
  match
    Lwt.bind content (fun x ->
      x |> Yojson.Safe.from_string
      |> (fun x -> Some (t, x))
      |> push 
      |> Lwt.return)
  with
  | exception _ ->
      Response.of_json (`Assoc [ ("status", `String "Failed") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return
  | _ ->
      Response.of_json (`Assoc [ ("status", `String "Successful") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return

let step push req =
  let content = Body.to_string req.Request.body in
  let content_stream = Body.to_stream req.Request.body in
  let _body = content_stream in
  match
    Lwt.bind content (fun x ->
      x |> Yojson.Safe.from_string
      |> (fun x -> Some ("step", x))
      |> push 
      |> Lwt.return)
  with
  | exception _ ->
      Response.of_json (`Assoc [ ("status", `String "Failed") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return
  | _ ->
      Response.of_json (`Assoc [ ("status", `String "Successful") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return

let init push =
  App.empty
  |> App.port 3000
  |> (fun x ->
    ["step"; "set_cell"]
    |> List.fold_left (fun acc x -> App.post ("/" ^ x) (post x push) acc) x
  )
  |> App.run_command
