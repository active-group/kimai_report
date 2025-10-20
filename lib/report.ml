module Timesheet = struct
  let projects_matches project_ids =
    if [] == project_ids
    then fun _ -> true
    else
      fun entry ->
      let project = Entry.project entry in
      match project with
      | None -> false
      | Some project_id -> List.mem project_id project_ids
  ;;

  let fill_description
    ?(prepend_project_name = false)
    activity_by_id
    project_by_id
    entry
    =
    let description_opt =
      match Entry.description entry, Entry.activity entry with
      | Some description, Some _ -> Some description
      | Some description, None -> Some description
      | None, Some activity_id -> activity_by_id activity_id
      | None, None -> None
    in
    let description_with_project_opt =
      if prepend_project_name
      then (
        let project_name_opt =
          match Entry.project entry with
          | Some project_id -> project_by_id project_id
          | None -> None
        in
        match project_name_opt, description_opt with
        | Some project_name, Some description ->
          Some (Printf.sprintf "%s: %s" project_name description)
        | None, description_opt -> description_opt
        | project_name_opt, None -> project_name_opt)
      else description_opt
    in
    Entry.with_description entry description_with_project_opt
  ;;

  let exec
    ?(project_names = [])
    ?(prepend_project_name = false)
    ?(user_names = [])
    ?(all_users = false)
    (module R : Repo.S)
    begin_date
    end_date
    =
    let ( let* ) = Api.bind in
    let* projects = R.find_projects () in
    let* activities = R.find_activities () in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let some_project_ids, none_project_names =
      RU.ids_by_names (module Project) projects project_names
    in
    if [] == none_project_names
    then
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
        let* timesheet =
          R.find_timesheet begin_date end_date some_user_ids all_users
        in
        timesheet
        |> List.filter (projects_matches some_project_ids)
        |> List.map
             (fill_description
                ~prepend_project_name
                (RU.name_by_id (module Activity) activities)
                (RU.name_by_id (module Project) projects))
        |> List.rev
        |> Lwt.return_ok
      else
        Lwt.return_error
        @@ Printf.sprintf
             "Users do not exist: [%s]"
             (String.concat ", " none_user_names)
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Projects do not exist: [%s]"
           (String.concat ", " none_project_names)
  ;;

  (* RFC 4180: escape double-quotes by doubling them *)
  let escape_double_quotes s = Str.global_replace (Str.regexp {|\"|}) "\"\"" s

  let print_csv emit_column_headers =
    if emit_column_headers
    then Printf.printf "\"Date\",\"Duration\",\"Description\"\n";
    List.iter (fun entry ->
      let description =
        Entry.description entry
        |> Option.map escape_double_quotes
        |> Option.value ~default:"no description"
      in
      Printf.printf
        "\"%s\",\"%.2f\",\"%s\"\n"
        (Entry.date_string entry)
        (Entry.duration entry)
        description)
  ;;

  let overall_duration timesheet =
    List.fold_left (fun acc entry -> acc +. Entry.duration entry) 0. timesheet
  ;;

  let print_overall_duration timesheet =
    timesheet |> overall_duration |> Printf.eprintf "Overall hours:\n%.2f"
  ;;
end

module Records = struct
  type t =
    { start_time : string
    ; end_time : string
    ; project : string
    ; activity : string
    ; description : string
    }

  let start_time { start_time; _ } = start_time
  let end_time { end_time; _ } = end_time
  let project { project; _ } = project
  let activity { activity; _ } = activity
  let description { description; _ } = description

  let projects_matches project_ids empty_result =
    if [] == project_ids
    then fun _ -> empty_result
    else
      fun entry ->
      let project = Entry.project entry in
      match project with
      | None -> false
      | Some project_id -> List.mem project_id project_ids
  ;;

  let exec
    ?(project_names = [])
    ?(exclude_project_names = [])
    ?(user_names = [])
    ?(all_users = false)
    (module R : Repo.S)
    begin_date
    end_date
    =
    let ( let* ) = Api.bind in
    let* projects = R.find_projects () in
    let* activities = R.find_activities () in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let project_name_by_id = RU.name_by_id (module Project) projects in
    let activity_name_by_id = RU.name_by_id (module Activity) activities in
    let some_project_ids, none_project_names =
      RU.ids_by_names (module Project) projects project_names
    in
    let some_excluded_project_ids, none_excluded_project_names =
      RU.ids_by_names (module Project) projects exclude_project_names
    in
    if [] == none_project_names && [] == none_excluded_project_names
    then
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
        let* timesheet =
          R.find_timesheet begin_date end_date some_user_ids all_users
        in
        timesheet
        |> List.filter (projects_matches some_project_ids true)
        |> List.filter (fun entry ->
          not (projects_matches some_excluded_project_ids false entry))
        |> List.filter (fun entry -> Option.is_some (Entry.end_string entry))
        |> List.map (fun entry ->
          { start_time = Entry.start_string entry
          ; end_time = Option.value (Entry.end_string entry) ~default:""
          ; project =
              (match Entry.project entry with
               | Some project_id ->
                 project_name_by_id project_id |> Option.value ~default:""
               | None -> "")
          ; activity =
              (match Entry.activity entry with
               | Some activity_id ->
                 activity_name_by_id activity_id |> Option.value ~default:""
               | None -> "")
          ; description = Entry.description entry |> Option.value ~default:""
          })
        |> Lwt.return_ok
      else
        Lwt.return_error
        @@ Printf.sprintf
             "Users do not exist: [%s]"
             (String.concat ", " none_user_names)
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Projects do not exist: [%s]"
           (String.concat
              ", "
              (List.append none_project_names none_excluded_project_names))
  ;;

  (* RFC 4180: escape double-quotes by doubling them *)
  let escape_double_quotes s = Str.global_replace (Str.regexp {|\"|}) "\"\"" s

  let print_csv emit_column_headers =
    if emit_column_headers
    then
      Printf.printf
        "\"Start\",\"End\",\"Project\",\"Activity\",\"Description\"\n";
    List.iter (fun entry ->
      Printf.printf
        "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
        (start_time entry)
        (end_time entry)
        (escape_double_quotes (project entry))
        (escape_double_quotes (activity entry))
        (escape_double_quotes (description entry)))
  ;;
end

module Percentage = struct
  module SM = Map.Make (String)

  let projects_matches project_ids =
    if [] == project_ids
    then fun _ -> false
    else
      fun entry ->
      let project = Entry.project entry in
      match project with
      | None -> false
      | Some project_id -> List.mem project_id project_ids
  ;;

  let project_labels (module R : Repo.S) projects =
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let project_by_id = RU.by_id (module Project) projects in
    let label entry =
      match Entry.project entry with
      | None -> "unknown"
      | Some project_id ->
        (match project_by_id project_id with
         | None -> "unknown"
         | Some p -> Project.name p)
    in
    label
  ;;

  let customer_labels (module R : Repo.S) projects customers =
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let project_by_id = RU.by_id (module Project) projects in
    let customer_by_id = RU.by_id (module Customer) customers in
    let label entry =
      match Entry.project entry with
      | None -> "unknown"
      | Some project_id ->
        (match project_by_id project_id with
         | None -> "unknown"
         | Some p ->
           (match customer_by_id @@ Project.customer p with
            | None -> "unknown"
            | Some c -> Customer.name c))
    in
    label
  ;;

  let durations_by_label time_entries by_label =
    List.fold_left
      (fun m t ->
        SM.update
          (by_label t)
          (function
            | None -> Some (Entry.duration t)
            | Some f -> Some (f +. Entry.duration t))
          m)
      SM.empty
      time_entries
  ;;

  let exec
    ?(by_customers = false)
    ?(exclude_project_names = [])
    ?(user_names = [])
    ?(all_users = false)
    (module R : Repo.S)
    begin_date
    end_date
    =
    let ( let* ) = Api.bind in
    let* projects = R.find_projects () in
    let* customers = R.find_customers () in
    let* users =
      match user_names with
      | [] -> Lwt.return_ok []
      | _ -> R.find_users ()
    in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let some_user_ids, none_user_names =
      RU.ids_by_names (module User) users user_names
    in
    if [] == none_user_names
    then
      let* timesheet =
        R.find_timesheet begin_date end_date some_user_ids all_users
      in
      let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
      let some_project_ids, _ =
        RU.ids_by_names (module Project) projects exclude_project_names
      in
      let filtered_timesheet =
        List.filter
          (fun entry -> not (projects_matches some_project_ids entry))
          timesheet
      in
      let durations =
        durations_by_label
          filtered_timesheet
          (if by_customers
           then customer_labels (module R) projects customers
           else project_labels (module R) projects)
      in
      let overall_duration =
        SM.bindings durations
        |> List.fold_left (fun acc kv -> acc +. snd kv) 0.0
      in
      let int_floor f = Float.round f |> int_of_float in
      durations
      |> SM.map (fun duration ->
        let percentage = duration /. overall_duration *. 100. in
        int_floor duration, percentage, int_floor percentage)
      |> SM.bindings
      |> List.sort
           (fun (_, (overall_hours_1, _, _)) (_, (overall_hours_2, _, _)) ->
              compare overall_hours_2 overall_hours_1)
      |> Lwt.return_ok
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Users do not exist: [%s]"
           (String.concat ", " none_user_names)
  ;;

  let print_csv entries =
    List.iter
      (fun (project_name, (overall_hours, percentage, percentage_rounded)) ->
        Printf.printf
          "%s,%ih,%.2f%%,%i%%\n"
          project_name
          overall_hours
          percentage
          percentage_rounded)
      entries
  ;;
end

module Working_time = struct
  type t =
    { date : string
    ; start_time : string
    ; end_time : string
    ; duration : float
    }

  let date { date; _ } = date
  let start_time { start_time; _ } = start_time
  let end_time { end_time; _ } = end_time
  let duration { duration; _ } = duration

  module SM = Map.Make (String)

  let earlier t1 t2 = if compare t1 t2 <= 0 then t1 else t2
  let later t1 t2 = if compare t2 t1 <= 0 then t1 else t2

  let exec
    ?(exclude_project_names = [])
    ?(user_names = [])
    ?(all_users = false)
    (module R : Repo.S)
    begin_date
    end_date
    =
    let ( let* ) = Api.bind in
    let* users =
      match user_names with
      | [] -> Lwt.return_ok []
      | _ -> R.find_users ()
    in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let some_user_ids, none_user_names =
      RU.ids_by_names (module User) users user_names
    in
    if [] == none_user_names
    then
      let* timesheet =
        R.find_timesheet begin_date end_date some_user_ids all_users
      in
      let* projects = R.find_projects () in
      let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
      let some_project_ids, _ =
        RU.ids_by_names (module Project) projects exclude_project_names
      in
      timesheet
      |> List.filter (fun entry ->
        not (Percentage.projects_matches some_project_ids entry))
      |> List.filter (fun entry -> Option.is_some (Entry.end_string entry))
      |> List.fold_left
           (fun mp entry ->
             SM.update
               (Entry.date_string entry)
               (fun workday_opt ->
                 match workday_opt with
                 | Some { start_time; end_time; duration; _ } ->
                   Some
                     { date = Entry.date_string entry
                     ; start_time =
                         earlier start_time @@ Entry.start_time_string entry
                     ; end_time = later end_time @@ Entry.end_time_string entry
                     ; duration = duration +. Entry.duration entry
                     }
                 | None ->
                   Some
                     { date = Entry.date_string entry
                     ; start_time = Entry.start_time_string entry
                     ; end_time = Entry.end_time_string entry
                     ; duration = Entry.duration entry
                     })
               mp)
           SM.empty
      |> SM.bindings
      |> List.map (fun (_, workday) -> workday)
      |> Lwt.return_ok
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Users do not exist: [%s]"
           (String.concat ", " none_user_names)
  ;;

  let print_csv emit_column_headers =
    if emit_column_headers
    then Printf.printf "\"Date\",\"Start\",\"End\",\"Duration\"\n";
    List.iter (fun workday ->
      Printf.printf
        "\"%s\",\"%s\",\"%s\",\"%.2f\"\n"
        (date workday)
        (start_time workday)
        (end_time workday)
        (duration workday))
  ;;

  let overall_duration working_time =
    List.fold_left (fun acc entry -> acc +. duration entry) 0. working_time
  ;;

  let print_overall_duration working_time =
    working_time |> overall_duration |> Printf.eprintf "Overall hours:\n%.2f"
  ;;
end

module Time_punch = struct
  type t =
    { start_time : string
    ; end_time : string
    }

  let start_time { start_time; _ } = start_time
  let end_time { end_time; _ } = end_time

  module SM = Map.Make (String)

  let exec
    ?(exclude_project_names = [])
    ?(user_names = [])
    ?(all_users = false)
    (module R : Repo.S)
    begin_date
    end_date
    =
    let ( let* ) = Api.bind in
    let* users =
      match user_names with
      | [] -> Lwt.return_ok []
      | _ -> R.find_users ()
    in
    let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
    let some_user_ids, none_user_names =
      RU.ids_by_names (module User) users user_names
    in
    if [] == none_user_names
    then
      let* timesheet =
        R.find_timesheet begin_date end_date some_user_ids all_users
      in
      let* projects = R.find_projects () in
      let module RU = Repo.Repo_utils (R) (Repo.Bi_lookup.Map) in
      let some_project_ids, _ =
        RU.ids_by_names (module Project) projects exclude_project_names
      in
      timesheet
      |> List.filter (fun entry ->
        not (Percentage.projects_matches some_project_ids entry))
      |> List.filter (fun entry -> Option.is_some (Entry.end_string entry))
      |> List.sort compare
      |> List.fold_left
           (fun mp entry ->
             let k_start = Entry.start_string entry in
             let k_end = Option.value (Entry.end_string entry) ~default:"" in
             match SM.find_opt k_start mp with
             | Some { start_time; _ } ->
               mp
               |> SM.remove k_start
               |> SM.add k_end { start_time; end_time = k_end }
             | None ->
               mp |> SM.add k_end { start_time = k_start; end_time = k_end })
           SM.empty
      |> SM.bindings
      |> List.map (fun (_, block) -> block)
      |> Lwt.return_ok
    else
      Lwt.return_error
      @@ Printf.sprintf
           "Users do not exist: [%s]"
           (String.concat ", " none_user_names)
  ;;

  let print_csv emit_column_headers =
    if emit_column_headers then Printf.printf "\"Start\",\"End\"\n";
    List.iter (fun block ->
      Printf.printf "\"%s\",\"%s\"\n" (start_time block) (end_time block))
  ;;
end
