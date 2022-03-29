open Clans

(** TODO: I (Ari) will move the handling of post requets to server.ml
    shortly. Nothing should be calling them besides what I write and the
    functionality should be the same, but that's why there's clutter in
    main. *)

let debug = false

let s, p =
  if debug then print_endline "Called create stream and push function"
  else ();
  let a = Lwt_stream.create () in
  if debug then print_endline "Created stream and push function" else ();
  a

let state =
  if debug then print_endline "Called controller.init" else ();
  let x = Controller.init @@ Model.new_world 100 100 in
  if debug then print_endline "Completed controller.init" else ();
  x

let do_something_with_post_request (req, json) : 'a Lwt.t =
  let open Yojson.Safe.Util in
  json |> to_assoc
  |> (fun x ->
       match req with
       | "update_cell" -> begin
           json |> Model.cell_from_json |> function
           | Some l ->
               let asc = to_assoc json in
               Controller.update_cell state
                 (List.assoc "x" asc |> to_int)
                 (List.assoc "y" asc |> to_int)
                 l
           | None -> ()
         end
       | _ -> failwith "unimplemented")
  |> Lwt.return
(* Function that recieves the json from a post request, as of right now
   just prints it to the console. The type signature should not be
   changed. The returned value is not used. *)

let rec post _ =
  if debug then print_endline "post called" else ();
  Lwt.bind
    (Lwt.bind (Lwt_stream.last_new s) do_something_with_post_request)
    post

let main =
  if debug then print_endline "main called" else ();
  let _ =
    Lwt_main.run (Lwt.both (Server.init p |> Lwt.return) (post ()))
  in
  if debug then
    print_endline "main wrapup (should not be called while running)"
  else ();
  ()
