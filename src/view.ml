open Bogue
module W = Widget
module L = Layout

type t = {
  render_count : int;
  zoom : int;
}

type event = int
type event_handler = event -> unit

type interaction_manager = {
  getcell : event_handler;
  setcell : event_handler;
  play : event_handler;
  pause : event_handler;
  save : event_handler;
  load : event_handler;
}

let init interactionmanager = failwith "Unimplemented"
let render view = failwith "Unimplemented"
let set_zoom view dx dy zoom = failwith "Unimplemented"
let draw_cell view x y cell = failwith "Unimplemented"