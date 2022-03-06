open Tyxml.Html

(* Write in the mycontent and mytitle tags for the content and title
   respectively using Tyxml formatting. index.html is the compiled
   version of normal html, and I am currently working on making it so
   that it can read normal html.*)

let mycontent =
  div
    ~a:[ a_class [ "content" ] ]
    [
      h1 [ txt "A not so fabulous title" ];
      txt "This is not fabulous content.";
    ]

let mytitle = title (txt "A Not Fabulous Web Page")
let mypage = html (head mytitle []) (body [ mycontent ])

let () =
  let file = open_out "index.html" in
  let fmt = Format.formatter_of_out_channel file in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) mypage;
  close_out file

open Opium

let no_subpages _req = Response.of_html mypage |> Lwt.return

let () =
  let open App in
  App.empty |> App.get "/" no_subpages |> App.run_command |> ignore
