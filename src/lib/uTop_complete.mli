(*
 * uTop_complete.mli
 * -----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** OCaml completion. *)

val complete : phrase_terminator : string -> input : string -> int * (string * string) list
  (** [complete ~phrase_terminator ~input] returns the start
      of the completed word in [input] and the list of possible
      completions with their suffixes. *)

val reset : unit -> unit
  (** Reset global cache. It must be called before each interactive
      read line. *)
