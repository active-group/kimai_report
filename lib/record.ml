module Record = struct
  let combine_errors_opt results =
    match
      List.filter_map
        (fun r ->
          match r with
          | Ok _ -> None
          | Error e -> Some e)
        results
    with
    | [] -> None
    | errs -> Some errs
  ;;

  let exec
    ?(user_name = None)
    (module R : Repo.S)
    begin_date_time
    end_date_time
    project_name
    activity_name
    description
    =
    let ( let* ) = Api.bind in
    let* users = R.find_users () in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Hash) in
    let user_id_result =
      match user_name with
      | Some user_name -> RU.id_by_name (module User) users user_name
      | None -> None
    in
    let user_id_error =
      match user_name with
      | None -> Result.ok 0
      | Some user_name ->
        Option.to_result
          user_id_result
          ~none:(Printf.sprintf "User %s does not exist" user_name)
    in
    let* projects = R.find_projects () in
    let* activities = R.find_activities () in
    let project_id_result =
      RU.id_by_name (module Project) projects project_name
      |> Option.to_result
           ~none:(Printf.sprintf "Project %s does not exist" project_name)
    in
    let activity_id_result =
      RU.id_by_name (module Activity) activities activity_name
      |> Option.to_result
           ~none:(Printf.sprintf "Activity %s does not exist" activity_name)
    in
    let maybe_errors =
      combine_errors_opt
        [ user_id_error; project_id_result; activity_id_result ]
    in
    match maybe_errors with
    | Some errors -> Lwt.return_error (String.concat ", " errors)
    | None ->
      (match project_id_result with
       | Ok project_id ->
         (match activity_id_result with
          | Ok activity_id ->
            let* is_success =
              R.add_timesheet
                ~user:user_id_result
                begin_date_time
                end_date_time
                project_id
                activity_id
                description
            in
            is_success |> Lwt.return_ok
          | Error e -> Lwt.return_error e)
       | Error e -> Lwt.return_error e)
  ;;
end
