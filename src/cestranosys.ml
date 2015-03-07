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
} [@@deriving of_yojson]


let docker_port = 2375

let events_uri host =
  Uri.make ~scheme:"http" ~host:host ~port:docker_port ~path:"/events" ()

let info =
  Cmdliner.Term.info "cestranosys"

let notify event =
  Printf.printf "%s: %s\n" event.from event.status;
  flush stdout

let handle_event data =
  let json = Yojson.Safe.from_string data in
  match (event_obj_of_yojson json) with
  | `Ok event -> notify event
  | `Error error ->
     (* XXX log warning or something *)
     ()

let run () =
  print_endline "Beginning to look out for cetaceans...";

  Lwt_main.run (
      let%lwt (resp, body) = Client.get (events_uri "localhost") in
      let status = Response.status resp in
      let code = (Code.code_of_status status) in
      if (code != 200) then
	failwith(Format.sprintf "Unexpected HTTP status: %d" code);
      let stream = (Cohttp_lwt_body.to_stream body) in
      (* XXX We assume that every iteration is a complete event *)
      Lwt_stream.iter handle_event stream
  )

let run_t =
  Cmdliner.Term.(pure run $ pure ())

let () =
  match Cmdliner.Term.eval (run_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
