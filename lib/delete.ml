module Delete = struct
  let exec ?(user_names = []) (module R : Repo.S) begin_date end_date =
    let ( let* ) = Api.bind in
    let ( let** ) = Lwt.bind in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let* users =
      match user_names with
      | [] -> Lwt.return_ok []
      | _ -> R.find_users ()
    in
    let some_user_ids, none_user_names =
      RU.ids_by_names (module User) users user_names
    in
    if [] == none_user_names
    then
      let* entries = R.find_timesheet begin_date end_date some_user_ids in
      let** errors =
        Lwt_list.filter_map_s
          (fun entry ->
            prerr_endline
            @@ Printf.sprintf "Deleting entry %d"
            @@ Entry.id entry;
            let** deletion = R.delete_timesheet @@ Entry.id entry in
            match deletion with
            | Ok _ -> Lwt.return_none
            | Error e ->
              Lwt.return_some
              @@ Printf.sprintf "Error deleting entry %d: %s" (Entry.id entry) e)
          entries
      in
      match errors with
      | [] -> Lwt.return_ok true
      | errs -> Lwt.return_error (String.concat ", " errs)
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Users do not exist: [%s]"
           (String.concat ", " none_user_names)
  ;;
end
