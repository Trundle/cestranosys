(*
 * The Cetacean stranding notification system.
 *)


type event_obj = {
    status: string;
    id: string;
    from: string;
    time: int;
}

val handle_events: (Uri.t -> event_obj -> unit Lwt.t) -> Uri.t -> unit Lwt.t

val notify_room: string -> string -> Uri.t -> event_obj -> unit Lwt.t

val strip_tag: string -> string
