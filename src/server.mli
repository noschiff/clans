val recieve : Yojson.Basic.t Lwt_stream.t -> Yojson.Basic.t option Lwt.t
(** Run during a get request, will either be a
    [Yojson.Basic.t `Int steps], [Yojson.Basic.t `String load_filename],
    [Yojson.Basic.t `String save_filename],
    [Yojson.Basic.t `Assoc \[`Int x; `Int y\]],
    [Yojson.Basic.t `Assoc\[`Assoc \[`Int x; `Int y\]; REAL_DATA_TBD\]] *)

val respond : (Yojson.Basic.t option -> unit) -> Yojson.Basic.t
(** Run during a post request, to be interpreted by the js*)

type t
(** A type representing the server*)

val init : unit -> t
(* To be implimented properly, but temprarily commented exists so
   controller.ml compiles*)

val render : t -> t
(* To be implimented properly, but temprarily commented exists so
   controller.ml compiles*)