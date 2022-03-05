open Bogue
module W = Widget
module L = Layout

let test_layout =
  (* let input = W.text_input ~max_size:200 ~prompt:"Enter your name" ()
     in *)
  let main_panel = W.label ~size:40 "CLANS Main Screen" in
  let side_panel =
    W.rich_text ~size:16
      [
        Text_display.para
          "Side Panel. This will have buttons and stuff.";
      ]
  in
  let layout =
    L.flat
      ~name:"CLANS: Cellular Life-like Automonomus Nation Simulator"
      [
        L.resident ~w:200 side_panel;
        L.resident ~w:400 ~h:400 main_panel;
      ]
  in

  (* let action ti l _ = let text = W.get_text ti in W.set_text l
     ("Hello " ^ text ^ "!") in let c = W.connect input label action
     Trigger.[ text_input; key_down ] in *)
  let board = Bogue.make [] [ layout ] in
  Bogue.run board

let main () = test_layout
