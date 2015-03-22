(*
 * The Cetacean stranding notification system.
 *)

open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix


type event_obj = {
    status: string;
    id: string;
    from: string;
    time: int;
} [@@deriving of_yojson, show]


type notification = {
    message: string;
    (* XXX a variant would be nice here, but unfortunately, ppx_deriving_json
       represents variants as arrays *)
    message_format: string;
} [@@deriving to_yojson]


let notify_url room =
  Uri.make ~scheme:"https" ~host:"hipchat.com"
	   ~path:("/v2/room/" ^ room ^ "/notification") ()

let pp_event event =
  Printf.sprintf "Container %s (%s): %s"
		 (String.sub event.id 0 12) event.from event.status

let pp_uri_and_event uri event =
  let host = match Uri.host uri with
    | Some host -> host
    | None -> "(unknown)"
  in Printf.sprintf "%s: %s" host (pp_event event)

let strip_tag image =
  match String.rindex image ':' with
  | index -> String.sub image 0 index
  | exception Not_found -> image

let notify_room token room uri event =
  let json = notification_to_yojson { message = pp_uri_and_event uri event;
				      message_format = "text"; } in
  let body = Cohttp_lwt_body.of_string (Yojson.Safe.to_string json)
  and headers = Cohttp.Header.of_list [("Authorization", "Bearer " ^ token);
				       ("Content-Type", "application/json")]
  and uri = notify_url room in
  let%lwt (response, _) = Cohttp_lwt_unix.Client.post ~body ~headers uri in
  match response |> Response.status |> Code.code_of_status with
  | 204 -> Lwt_io.printl ("Sent notification for " ^ (show_event_obj event))
  | code -> Lwt_io.printlf "Unexpected status %d when sending %s"
			   code (show_event_obj event)

let handle_event notify data =
  let json = Yojson.Safe.from_string data in
  match event_obj_of_yojson json with
  | `Ok event -> notify event
  | `Error error ->
     Lwt_io.printlf "[ERR] Could not decode JSON: %s ('%s')" error data

let handle_events notify docker_uri =
  let events_uri = Uri.with_path docker_uri "/events" in
  let%lwt (resp, body) = Client.get events_uri in
  let code = resp |> Response.status |> Code.code_of_status in
  if code <> 200 then
    failwith(Format.sprintf "Unexpected HTTP status: %d" code);
  let stream = Cohttp_lwt_body.to_stream body in
  (* XXX We assume that every iteration is a complete event *)
  Lwt_stream.iter_s (handle_event (notify docker_uri)) stream


let run images_to_filter docker_uris room =
  let room_token =
    try Sys.getenv "HIPCHAT_ROOM_TOKEN" with
    Not_found ->
      print_endline "HIPCHAT_ROOM_TOKEN env var not set!";
      exit 1
  in
  let notify =
    (fun uri event ->
     let image = strip_tag event.from in
     if not (List.mem image images_to_filter) then
       notify_room room_token room uri event
     else Lwt.return ())
  in
  Lwt_main.run (Lwt_list.iter_p (handle_events notify) docker_uris)
