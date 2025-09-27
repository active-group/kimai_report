type t =
  { id : int
  ; name : string
  ; fullname : string option
  }

let id { id; _ } = id
let name { name; _ } = name
let fullname { fullname; _ } = fullname

module D = Decoder.Yojson.Safe

let ( let* ) = D.bind

let decoder =
  let* id = D.field "id" D.int in
  let* name = D.field "username" D.string in
  let* fullname = D.optional @@ D.field "alias" D.string in
  D.return { id; name; fullname }
;;

let encoder name fullname =
  let user = `Assoc [ "username", `String name; "alias", `String fullname ] in
  Encoder.Yojson.Encoder.to_string user
;;
