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

let encoder ?(user = None) ?(half_day = false) start_date end_date kind comment =
  let absence =
    `Assoc
      (List.append
         [ "date", `String start_date
         ; "end", `String end_date
         ; "halfDay", `Bool half_day
         ; "type", `String kind
         ; "comment", `String comment
         ]
         (match user with
          | Some u -> [ "user", `Int u ]
          | None -> []))
  in
  Encoder.Yojson.Encoder.to_string absence
;;
