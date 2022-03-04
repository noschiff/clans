type t = {
  initialized : bool;
  render_count : int;
  zoom : int;
}

let init = { initialized = true; render_count = 0; zoom = 0; }

let is_initalized t = t.initialized

let render t = let () = print_endline "Render View" in { initialized = t.initialized; render_count = t.render_count + 1; zoom = t.zoom; }

let get_cell t x y = if t.initialized then 
  print_endline ("Get the cell at " ^ (string_of_int x) ^ " and " ^ (string_of_int y)) 
  else print_endline "Not initialized"

let set_cell t x y data = let () = print_endline ("Set the cell at " ^ (string_of_int x) ^ " and " ^ (string_of_int y) ^ " with data " ^ data) in
  { initialized = t.initialized; render_count = t.render_count + 1; zoom = t.zoom }

let zoom_in t amt = print_endline ("Zoom into the view with " ^ (string_of_int amt) ^ " magnitude");
  { initialized = t.initialized; render_count = t.render_count + 1; zoom = t.zoom + 1; }

let zoom_out t amt = print_endline ("Zoom out from the view with " ^ (string_of_int amt) ^ " magnitude");
{ initialized = t.initialized; render_count = t.render_count + 1; zoom = t.zoom - 1; }