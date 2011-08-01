(*
 * uTop.mli
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** UTop configuration. *)

val count : int React.signal
  (** The number of commands already executed. *)

val keywords : Set.Make(String).t ref
  (** The set of OCaml keywords. *)

val add_keyword : string -> unit
  (** Add a new OCaml keyword. *)

(** {6 Console specific configuration} *)

type profile = Dark | Light
    (** Profile for terminal colors. *)

val profile : profile React.signal
  (** The profile of the terminal. It defaults to {!Dark}. This is
      used by the default prompt to choose colors. *)

val set_profile : profile -> unit
  (** Sets the profile of the terminal. *)

val size : LTerm_geom.size React.signal
  (** The current size of the terminal.  *)

val prompt : LTerm_text.t React.signal ref
  (** The current prompt.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2. *)

val prompt_continue : LTerm_text.t React.signal ref
  (** The prompt used to continue unterminated phrase.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2. *)

val prompt_comment : LTerm_text.t React.signal ref
  (** The prompt used to continue unterminated comments.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2. *)
