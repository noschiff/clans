open Opium

type t = (string * Yojson.Safe.t) option -> unit

(** Posts cell to stream to be dealt with later, and returns a message
    with whether the json was successfully parsed back to the client. *)
let post t (push : t) req =
  let content = Body.to_string req.Request.body in
  let content_stream = Body.to_stream req.Request.body in
  let _body = content_stream in
  match
    Lwt.bind content (fun x ->
        x |> Yojson.Safe.from_string
        |> (fun x -> Some (t, x))
        |> push |> Lwt.return)
  with
  | exception _ ->
      Response.of_json (`Assoc [ ("status", `String "Failed") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return
  | _ ->
      Response.of_json (`Assoc [ ("status", `String "Successful") ])
      |> Response.add_header ("Access-Control-Allow-Origin", "*")
      |> Lwt.return



(*let step (push : t) req = let content = Body.to_string
  req.Request.body in let content_stream = Body.to_stream
  req.Request.body in let _body = content_stream in match Lwt.bind
  content (fun x -> x |> Yojson.Safe.from_string |> (fun x -> Some
  ("step", x)) |> push |> Lwt.return) with | exception _ ->
  Response.of_json (`Assoc [ ("status", `String "Failed") ]) |>
  Response.add_header ("Access-Control-Allow-Origin", "*") |> Lwt.return
  | _ -> Response.of_json (`Assoc [ ("status", `String "Successful") ])
  |> Response.add_header ("Access-Control-Allow-Origin", "*") |>
  Lwt.return
  
  
  Commented out because step should be a get request not post. *)
let step req =
  let step_count = Router.param req "step_count" |> int_of_string in
  let full_world = Router.param req "full_world" |> bool_of_string in
  let rec call_step (count : int) : unit =
    if count > 0 then begin
      Controller.step ();
      call_step (count - 1)
    end
    else ()
  in
  call_step step_count;
  full_world |> Controller.get_json |> Response.of_json |> Lwt.return

let init push =
  App.empty |> App.port 3000
  |> begin
       fun app ->
       [ "update_cell" ]
       |> List.fold_left
            (fun acc x -> App.post ("/" ^ x) (post x push) acc)
            app
     end
  |> App.get "/step/:step_count/:full_world" step
  |> App.run_command
