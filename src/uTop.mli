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

(** {6 Console specific configuration} *)

val size : LTerm_geom.size React.signal
  (** The current size of the terminal.  *)

val prompt : LTerm_text.t React.signal ref
  (** The current prompt. For compatibility with ocaml, it must ends
      with a line of length 2. *)

val prompt_continue : LTerm_text.t React.signal ref
  (** The prompt used to continue unterminated commands. For
      compatibility with ocaml, it must ends with a line of length
      2. *)
