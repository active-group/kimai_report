type t =
  { date : Ptime.t
  ; start_string : string
  ; end_string : string option
  ; duration : float
  ; activity : int option
  ; description : string option
  ; project : int option
  ; user : int option
  }

let date { date; _ } = date
let project { project; _ } = project
let duration { duration; _ } = duration
let description { description; _ } = description
let with_description t description = { t with description }
let activity { activity; _ } = activity
let user { user; _ } = user

let date_string_from_timestamp s =
  match Str.split (Str.regexp "T") s with
  | [] -> ""
  | x :: _ -> x
;;

let time_string_from_timestamp s =
  match Str.split (Str.regexp "T") s with
  | [] -> ""
  | _ :: [] -> ""
  | _ :: x :: _ -> String.sub x 0 8
;;

let date_string { date; _ } =
  Ptime.to_rfc3339 date |> date_string_from_timestamp
;;

let start_time_string { start_string; _ } =
  time_string_from_timestamp start_string
;;

let end_time_string { end_string; _ } =
  match end_string with
  | Some es -> time_string_from_timestamp es
  | None -> ""
;;

let start_string { start_string; _ } = start_string
let end_string { end_string; _ } = end_string

module D = Decoder.Yojson.Safe

let decoder =
  let open D.Syntax in
  let* date = D.field "begin" Api.timestamp_decoder in
  let* start_string = D.field "begin" D.string in
  let* end_string = D.optional @@ D.field "end" D.string in
  let* description = D.optional @@ D.field "description" D.string in
  let* activity = D.optional @@ D.field "activity" D.int in
  let* duration' = D.field "duration" D.int in
  let duration = float_of_int duration' /. 60. /. 60. in
  let* project = D.optional @@ D.field "project" D.int in
  let* user = D.optional @@ D.field "user" D.int in
  D.return
    { date
    ; start_string
    ; end_string
    ; duration
    ; activity
    ; description
    ; project
    ; user
    }
;;

let encoder
  ?(user = None)
  begin_date_time
  end_date_time
  project
  activity
  description
  =
  let entry =
    `Assoc
      [ "begin", `String begin_date_time
      ; "end", `String end_date_time
      ; "project", `Int project
      ; "activity", `Int activity
      ; "description", `String description
      ; ( "user"
        , match user with
          | Some u -> `Int u
          | None -> `Null )
      ]
  in
  Encoder.Yojson.Encoder.to_string entry
;;
