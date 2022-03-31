open Opium

type t = (string * Yojson.Safe.t * Yojson.Safe.t Lwt.u) option -> unit

let debug = false

(** Posts cell to stream to be dealt with later, and returns a message
    with whether the json was successfully parsed back to the client. *)
let post name (push : t) req =
  Printf.printf "Recieve post %s\n" name;
  let content = Body.to_string req.Request.body in
  let respond x =
    if debug then
      Printf.printf "Responding with %s\n" (Yojson.Safe.show x)
    else ();
    x |> Response.of_json
    |> Response.add_header ("Access-Control-Allow-Origin", "*")
    |> Lwt.return
  in
  let ret, res = Lwt.task () in
  match
    Lwt.bind content (fun x ->
        x |> Yojson.Safe.from_string
        |> (fun x -> Some (name, x, res))
        |> push |> Lwt.return)
  with
  | exception _ -> respond (`Assoc [ ("status", `String "Failed") ])
  | _ -> Lwt.bind ret respond

let init (push : t) =
  App.empty |> App.port 3000
  |> (fun x ->
       [ "update_cell"; "step"; "populate" ]
       |> List.fold_left
            (fun acc s -> App.post ("/" ^ s) (post s push) acc)
            x)
  |> (fun x ->
       if debug then print_endline "Server.init completed" else ();
       x)
  |> App.run_command
