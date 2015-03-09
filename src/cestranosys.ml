(*
 * The Cetacean stranding notification system.
 *)

open Lwt
open Cohttp
open Cohttp_lwt_unix


type event_obj = {
    status: string;
    id: string;
    from: string;
    time: int;
} [@@deriving of_yojson, show]


type notification_obj = {
    message: string;
    message_format: string;
} [@@deriving to_yojson]


let docker_port = 2375

let notify_room token event =
  (* XXX hipchat notification would be here *)
  print_endline (show_event_obj event);
  flush stdout

let handle_event notify data =
  let json = Yojson.Safe.from_string data in
  match (event_obj_of_yojson json) with
  | `Ok event -> notify event
  | `Error error ->
     (* XXX log warning or something *)
     ()

let run docker_uris =
  let room_token =
    try Sys.getenv "HIPCHAT_ROOM_TOKEN" with
    Not_found ->
      print_endline "HIPCHAT_ROOM_TOKEN env var not set!";
      exit 1
  in
  let notify = notify_room room_token
    and events_uri = Uri.with_path (List.hd docker_uris) "/events"
  in
  Lwt_main.run (
      let%lwt (resp, body) = Client.get events_uri in
      let status = Response.status resp in
      let code = (Code.code_of_status status) in
      if (code != 200) then
	failwith(Format.sprintf "Unexpected HTTP status: %d" code);
      let stream = (Cohttp_lwt_body.to_stream body) in
      (* XXX We assume that every iteration is a complete event *)
      Lwt_stream.iter (handle_event notify) stream
  )


(* CLI follows *)

let info =
  Cmdliner.Term.info "cestranosys"

(* Converts into a Uri.t *)
let uri =
  (fun s ->
   let uri = Uri.of_string s in
   match Uri.host uri with
   | Some _ -> `Ok uri
   | None -> `Error "Please use an URI with scheme set"
  ),
  fun ppf u -> Format.fprintf ppf "%s" (Uri.to_string u)

let docker_uris =
  Cmdliner.Arg.(non_empty & (pos_all uri) [] & info [] ~docv:"DOCKER_URI")

let run_t =
  Cmdliner.Term.(pure run $ docker_uris)

let () =
  match Cmdliner.Term.eval (run_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
