type message = {
  user_name : string;
  body : string;
}
[@@deriving yojson]

let ( let* ) = Lwt.bind
let message_file = "index.html"

let request () =
  Lwt_io.with_file ~mode:Input message_file (fun input_channel ->
      let* message_string =
        Lwt_io.read_lines input_channel |> Lwt_stream.to_list
      in
      let message_json =
        Yojson.Safe.from_string (String.concat "\n" message_string)
      in
      match [%of_yojson: message list] message_json with
      | Ok messages -> Lwt.return messages
      | Error error -> raise (Invalid_argument error))

let response m =
  let* messages = request () in
  let messages = m :: messages in
  Lwt_io.with_file ~mode:Output message_file (fun output_channel ->
      let messages_string =
        messages |> [%to_yojson: message list]
        |> Yojson.Safe.pretty_to_string
      in
      Lwt_io.write output_channel messages_string)
