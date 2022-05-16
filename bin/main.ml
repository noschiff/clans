open Clans

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

let handle_update_cell json =
  let open Yojson.Safe.Util in
  (json |> Model.cell_from_json |> function
   | Some l ->
       let asc = to_assoc json in
       if debug then
         asc
         |> (fun x -> List.fold_left (fun acc e -> acc ^ fst e) "" x)
         |> print_endline
       else ();
       Controller.update_cell state
         (List.assoc "x" asc |> to_int)
         (List.assoc "y" asc |> to_int)
         l
   | None -> ());
  Controller.get_json false state

let handle_step json =
  let open Yojson.Safe.Util in
  let assoc = json |> to_assoc in
  let full_world = List.assoc "full" assoc |> to_bool in
  let steps = List.assoc "steps" assoc |> to_int in
  let rec call_step n =
    if n > 0 then begin
      Controller.step state;
      call_step (n - 1)
    end
    else Controller.get_json full_world state
  in
  call_step steps

let handle_populate json =
  let open Yojson.Safe.Util in
  json |> to_assoc |> List.assoc "density" |> to_float
  |> Controller.populate_world state;
  Controller.get_json false state

let handle_load json =
  let open Yojson.Safe.Util in
  json |> to_assoc |> List.assoc "world" |> to_string
  |> Controller.load_from_string state;
  Controller.get_json false state

let handle_invalid_request json =
  let open Yojson.Safe.Util in
  if debug then print_endline "invalid request";
  Controller.get_json false state

let handle_exception e =
  if debug then begin
    Printf.printf "exception encountered: \n%s\nTRACE:\n%s\n"
      (Printexc.to_string e)
      (Printexc.get_backtrace ());
    Printexc.print_backtrace Stdlib.stdout
  end;
  Controller.get_json false state

let handle_post_request (req, json, callback) : 'a Lwt.t =
  if debug then
    Printf.printf "Recieved POST <%s>: %s\n" req (Yojson.Safe.show json);
  let open Yojson.Safe.Util in
  begin
    begin
      try
        match req with
        | "update_cell" -> handle_update_cell json
        | "step" -> handle_step json
        | "populate" -> handle_populate json
        | "load" -> handle_load json
        | _ -> handle_invalid_request json
      with
      | e -> handle_exception e
    end
    |> Lwt.wakeup_later callback
    |> Lwt.return
  end

let rec post _ =
  if debug then print_endline "post called" else ();
  Lwt.bind (Lwt.bind (Lwt_stream.last_new s) handle_post_request) post

let main =
  if debug then print_endline "main called" else ();
  let _ =
    Lwt.both (Server.init p |> Lwt.return) (post ()) |> Lwt_main.run
  in
  if debug then
    print_endline "main wrapup (should not be called while running)"
  else ()
