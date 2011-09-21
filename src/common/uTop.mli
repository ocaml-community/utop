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

type ui = Console | GTK | Emacs
    (** The user interface in use. *)

val get_ui : unit -> ui
  (** Returns the user interface in use. *)

(** {6 GTK specific utilities} *)

val exec_in_gui : (unit -> unit) -> unit
  (** [exec_in_gui f] executes [f] in the thread that handle the
      UI. The only use of this function is to call [window#show ()] on
      Windows:

      Since windows are attached to a thread on Windows and utop
      handle the UI in a separate thread, doing [window#show ()] in
      the toplevel UI will not work. *)

(** {6 Console/GTK specific configuration} *)

type profile = Dark | Light
    (** Profile for colors. *)

val profile : profile React.signal
  (** The color profile. It defaults to {!Dark}. This is used by the
      default prompt to choose colors. *)

val set_profile : profile -> unit
  (** Sets the color profile. *)

val smart_accept : bool ref
  (** If [true], then only lines terminated with ";;" will be sent to
      ocaml, otherwise the input will always be sent to ocaml when the
      user press Enter. It default to [true]. *)

val size : LTerm_geom.size React.signal
  (** The current size of the terminal. This is used only in the
      console UI. *)

val key_sequence : LTerm_key.t list React.signal
  (** The current key sequence entered by the user. This is used only
      in the console UI. *)

val time : float ref
  (** The time of the beginning of the current command. *)

val prompt : LTerm_text.t React.signal ref
  (** The current prompt.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2 in the console UI. *)

val prompt_continue : LTerm_text.t React.signal ref
  (** The prompt used to continue unterminated phrase.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2 in the console UI. *)

val prompt_comment : LTerm_text.t React.signal ref
  (** The prompt used to continue unterminated comments.

      For compatibility with ocaml error printing, it must ends with a
      line of length 2 in the console UI. *)

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
