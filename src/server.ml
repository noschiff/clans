open Opium

type t = Yojson.Safe.t option -> unit

let get req =
  let content = Body.to_string req.Request.body in
  Lwt.bind content (fun x ->
      x |> Yojson.Safe.from_string |> Response.of_json |> Lwt.return)

let post (push : t) req =
  let json = req |> Request.to_json_exn in
  ignore @@ Lwt.bind json (fun x -> Some x |> push |> Lwt.return);
  (fun _json ->
    Lwt.return
      (Response.make ~body:(Body.of_string "Received response") ()))
    json

let get_html _req =
  let my_page =
    let open Tyxml.Html in
    html
      (head (title (txt "Title")) [])
      (body [ h1 [ txt "Hello World!" ] ])
  in
  my_page |> Response.of_html |> Lwt.return

let init (push : Yojson.Safe.t option -> unit) : t =
  begin
    match
      App.empty |> App.port 3000 |> App.cmd_name "clans"
      |> App.get "/" get
      |> App.get "/index.html" get_html
      |> App.post "/" (post push)
      |> App.run_command'
    with
    | `Ok (app : unit Lwt.t) -> Lwt_main.run app
    | `Error -> exit 1
    | `Not_running -> exit 0
  end;
  push
