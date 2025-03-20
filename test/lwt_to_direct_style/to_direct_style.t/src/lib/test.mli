val f1 : 'a Lwt_condition.t -> 'a Lwt.t
val f2 : Lwt_mutex.t -> 'a Lwt_condition.t -> 'a Lwt.t
val f3 : Lwt_mutex.t option -> 'a Lwt_condition.t -> 'a Lwt.t
val x : unit Lwt.t
val f : unit -> unit Lwt.t
val g : unit Lwt.t -> unit
val h : (unit -> unit Lwt.t) -> unit Lwt.t
val i : (unit Lwt.t -> unit) -> unit
val test : unit -> unit Lwt.t
