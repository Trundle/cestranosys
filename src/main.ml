let run images_to_filter docker_uris room =
  let room_token =
    try Sys.getenv "HIPCHAT_ROOM_TOKEN" with
    Not_found ->
      print_endline "HIPCHAT_ROOM_TOKEN env var not set!";
      exit 1
  in
  let notify =
    (fun uri event ->
     let image = Cestranosys.strip_tag event.Cestranosys.from in
     if not (List.mem image images_to_filter) then
       Cestranosys.notify_room room_token room uri event
     else Lwt.return ())
  in
  Lwt_main.run (Lwt_list.iter_p (Cestranosys.handle_events notify) docker_uris)

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

let filter =
  Cmdliner.Arg.(value & opt_all string [] &
		  info ["f"; "filter"] ~docv:"IMAGE")

let room =
  Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"ROOM")

let docker_uris =
  Cmdliner.Arg.(non_empty & (pos_right 0 uri) [] & info [] ~docv:"DOCKER_URI")

let run_t =
  Cmdliner.Term.(pure run $ filter $ docker_uris $ room)

let () =
  match Cmdliner.Term.eval (run_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
