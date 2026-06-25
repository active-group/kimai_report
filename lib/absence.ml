type t =
  { id : int
  ; start_string : string
  ; end_string : string option
  ; half_day : bool
  ; kind : string
  ; comment : string option
  ; user : int
  }

let id { id; _ } = id
let start_string { start_string; _ } = start_string
let end_string { end_string; _ } = end_string
let half_day { half_day; _ } = half_day
let kind { kind; _ } = kind
let comment { comment; _ } = comment
let user { user; _ } = user

module D = Decoder.Yojson.Safe

let decoder =
  let open D.Syntax in
  let* id = D.field "id" D.int in
  let* start_string = D.field "date" D.string in
  let* end_string = D.optional @@ D.field "end" D.string in
  let* half_day = D.field "halfDay" D.boolean in
  let* comment = D.optional @@ D.field "comment" D.string in
  let* kind = D.field "type" D.string in
  let* user = D.field "user" (D.field "id" D.int) in
  D.return { id; start_string; end_string; half_day; kind; comment; user }
;;

let encoder
  ?(half_day = false)
  ?(end_date = None)
  ?(comment = None)
  user_id
  start_date
  kind
  =
  let absence =
    `Assoc
      ([ "user", `Int user_id
       ; "date", `String start_date
       ; "halfDay", `Bool half_day
       ; "type", `String kind
       ]
       @ (match end_date with
          | Some e -> [ "end", `String e ]
          | None -> [])
       @
       match comment with
       | Some c -> [ "comment", `String c ]
       | None -> [])
  in
  Encoder.Yojson.Encoder.to_string absence
;;
