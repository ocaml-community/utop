(*
 * uTop_history.mli
 * -------
 * Copyright : (c) 2017, Fabian Hemmer <copy@copy.sh>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)


(** Type of a history entry *)
type entry =
  | Input of string
  | Output of string
  | Error of string
  | Warnings of string
  | Bad_input of string

type t

val create : unit -> t
  (** Create a new, empty history *)

val contents : t -> entry list
  (** Get the contents of the given history *)

val add_input : t -> string -> unit
  (** Add an input *)

val add_output : t -> string -> unit
  (** Add an output *)

val add_error : t -> string -> unit
  (** Add an error *)

val add_warnings : t -> string -> unit
  (** Add a warning *)

val add_bad_input : t -> string -> unit
  (** Add an input that resulted in an error *)
