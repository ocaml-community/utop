(*
 * uTop_main.mli
 * -------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

val main : unit -> unit
  (** Start utop. *)

exception Term of int
  (** Exception raised when a signal that should terminate the process
      is received. The argument is the signal number.

      utop raises this exception for SIGHUP and SIGTERM by default. *)

type value = V : string * _ -> value

val interact
  :  ?search_path:string list
  -> ?build_dir:string
  -> unit:string
  -> loc:(string * int * int * int)
  -> values:value list
  -> unit
  -> unit
