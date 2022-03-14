type message = {
  user_name : string;
  body : string;
}
[@@deriving yojson]

val request : unit -> message list Lwt.t
val response : message -> unit Lwt.t