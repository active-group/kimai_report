type t

val id : t -> int
val name : t -> string
val fullname : t -> string option

(** [decoder yojson] decodes a {!Yojson.Safe.t} into a {!t}. *)
val decoder : t Decoder.Yojson.Safe.decoder

(** [encoder name fullname] encodes a customer into a JSON string. *)
val encoder : string -> string -> Encoder.Yojson.Encoder.encoder
