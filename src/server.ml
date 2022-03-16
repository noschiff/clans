let ( let* ) = Lwt.bind
let message_file = "index.html"
let recieve _ = Lwt.return None
(*Lwt_io.with_file ~mode:Input message_file (fun input_channel -> let*
  message_string = Lwt_io.read_lines input_channel |> Lwt_stream.to_list
  in let message_json = Yojson.Safe.from_string (String.concat "\n"
  message_string) in match [%of_yojson: message list] message_json with
  | Ok messages -> Lwt.return messages | Error error -> raise
  (Invalid_argument error))

  To be implimented properly, but temprarily commented out so it
  compiles*)

let respond _ = Yojson.Basic.from_string ""
(* let* messages = request () in let messages = m :: messages in
   Lwt_io.with_file ~mode:Output message_file (fun output_channel -> let
   messages_string = messages |> [%to_yojson: message list] |>
   Yojson.Safe.pretty_to_string in Lwt_io.write output_channel
   messages_string)

   To be implimented properly, but temprarily commented out so it
   compiles*)

type t = int
(* To be implimented properly, but temprarily commented exists so
   controller.ml compiles*)

let init () = 0
let render a = a