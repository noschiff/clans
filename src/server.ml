open Opium

(** Posts cell to stream to be dealt with later, and returns a message
    with whether the json was successfully parsed back to the client. *)
let post t push req =
  let content = Body.to_string req.Request.body in
  let content_stream = Body.to_stream req.Request.body in
  let _body = content_stream in
  let respond x = x
    |> Response.of_json
    |> Response.add_header ("Access-Control-Allow-Origin", "*")
    |> Lwt.return
  in 
  match
    let (ret, res) = Lwt.task () in
    Lwt.bind content (fun x ->
      x |> Yojson.Safe.from_string
      |> (fun x -> Some (t, x, res))
      |> push;
      Lwt.bind ret respond
    )
  with
  | exception _ ->
    respond (`Assoc [ ("status", `String "Failed") ])
  | _ ->
    respond (`Assoc [ ("status", `String "Successful") ])

let init push =
  App.empty
  |> App.port 3000
  |> (fun x ->
    ["step"; "update_cell"]
    |> List.fold_left (fun acc x -> App.post ("/" ^ x) (post x push) acc) x
  )
  |> App.run_command
