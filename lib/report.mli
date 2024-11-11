module Timesheet : sig
  (** [exec ~project_name (module R) begin_date end_date] is a list of all
      timesheet entries between [begin_date] and [end_date] (inclusively) or a
      string error. *)
  val exec
    :  ?project_names:string list
    -> ?prepend_project_name:bool
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> Entry.t list Repo.or_error

  (** [print_csv pairs] prints all timesheet entries to stdout. *)
  val print_csv : bool -> Entry.t list -> unit

  (** [overall_duration pairs] calculates the overall duration of the calculated
      timesheet. *)
  val overall_duration : Entry.t list -> float

  (** [print_overall_duration pairs] prints the overall duration of the
      calculated timesheet to stderr. *)
  val print_overall_duration : Entry.t list -> unit
end

module Records : sig
  type t

  (** [exec ~project_names ~exclude_project_names (module R) begin_date end_date] is a list of all
      timesheet records between [begin_date] and [end_date] (inclusively) or a
      string error. *)
  val exec
    :  ?project_names:string list
    -> ?exclude_project_names:string list
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> t list Repo.or_error

  (** [print_csv pairs] prints all timesheet records to stdout. *)
  val print_csv : bool -> t list -> unit
end

module Percentage : sig
  (** [exec (module R) begin_date end_date] is a list of pairs, mapping project
      names to percentages. Includes everything between [begin_date] and
      [end_date] (inclusively) or a string error. *)
  val exec
    :  ?by_customers:bool
    -> ?exclude_project_names:string list
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> (string * (int * float * int)) list Repo.or_error

  (** [print_csv pairs] prints all percentages to stdout. *)
  val print_csv : (string * (int * float * int)) list -> unit
end

module Working_time : sig
  type t

  (** [exec ~project_name (module R) begin_date end_date] is a list of all
      working-day entries between [begin_date] and [end_date] (inclusively) or a
      string error. *)
  val exec
    :  ?exclude_project_names:string list
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> t list Repo.or_error

  (** [print_csv pairs] prints all working-time entries to stdout. *)
  val print_csv : bool -> t list -> unit

  (** [overall_duration pairs] calculates the overall duration of the calculated
      working time. *)
  val overall_duration : t list -> float

  (** [print_overall_duration pairs] prints the overall duration of the
      calculated working time to stderr. *)
  val print_overall_duration : t list -> unit
end

module Time_punch : sig
  type t

  (** [exec ~project_name (module R) begin_date end_date] is a list of all
      time-punch entries between [begin_date] and [end_date] (inclusively) or a
      string error. *)
  val exec
    :  ?exclude_project_names:string list
    -> (module Repo.S)
    -> Date.t
    -> Date.t
    -> t list Repo.or_error

  (** [print_csv pairs] prints all working-time entries to stdout. *)
  val print_csv : bool -> t list -> unit
end
