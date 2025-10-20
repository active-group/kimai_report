module Delete : sig
  val exec
    :  ?user_names:string list
    -> ?all_users:bool
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> (bool, string) result Lwt.t
end
