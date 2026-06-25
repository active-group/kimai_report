type t

val id : t -> int
val start_string : t -> string
val end_string : t -> string option
val half_day : t -> bool
val kind : t -> string
val comment : t -> string option
val user : t -> int
val decoder : t Decoder.Yojson.Safe.decoder

val encoder
  :  ?user:int option
  -> ?half_day:bool
  -> string
  -> string
  -> string
  -> string
  -> Encoder.Yojson.Encoder.encoder
