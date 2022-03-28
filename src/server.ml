open Opium

type t = Yojson.Safe.t option -> unit

let post_cell push req =
  let content = Body.to_string req.Request.body in
  let content_stream = Body.to_stream req.Request.body in
  let _body = content_stream in
  match
    Lwt.bind content (fun x ->
        x |> Yojson.Safe.from_string
        |> (fun x -> Some x)
        |> push |> Lwt.return)
  with
  | exception _ ->
      Response.of_json (`Assoc [ ("status", `String "Uhhh ohhhh") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return
  | _ ->
      Response.of_json (`Assoc [ ("status", `String "Successful") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return

let parse_get req = Controller.get_json
(* Later this might parse different types of get requests, but for now
   it just gets the current json of the world. *)

let get req =
  Response.of_json (parse_get req)
  |> Response.add_header ("Access-Control-Allow-Origin", "*")
  |> Lwt.return

let init (push : Yojson.Safe.t option -> unit) =
  App.empty |> App.port 3000 |> App.get "/get" get
  |> App.post "/post_cell" (post_cell push)
  |> App.run_command
