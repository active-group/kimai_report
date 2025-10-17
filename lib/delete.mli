module Delete : sig
  val exec
    :  ?user_names:string list
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> (bool, string) result Lwt.t
end
