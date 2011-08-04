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

val key_sequence : LTerm_key.t list React.signal
  (** The current key sequence entered by the user. *)

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

(** {6 Hooks} *)

val new_command_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions called before each new command. *)

val at_new_command : (unit -> unit) -> unit
  (** [at_new_command f] adds [f] to the hooks executed before each
      new commands. *)

val new_prompt_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions executed before each new prompt, including
      continuation prompts. *)

val at_new_prompt : (unit -> unit) -> unit
  (** [at_new_prompt f] adds [f] to the hooks executed before each new
      prompt. *)
