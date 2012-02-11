(*
 * uTop.mli
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(** UTop configuration. *)

open React

val version : string
  (** Version of utop. *)

val count : int React.signal
  (** The number of commands already executed. *)

val keywords : Set.Make(String).t ref
  (** The set of OCaml keywords. *)

val add_keyword : string -> unit
  (** Add a new OCaml keyword. *)

type ui = Console | Emacs
    (** The user interface in use. *)

val get_ui : unit -> ui
  (** Returns the user interface in use. *)

val camlp4 : bool signal
  (** [true] if the lexer should recognize camlp4 quotations. This
      variable is automatically set to [true] when you type [#camlp4o]
      or [#camlp4r]. *)

val get_camlp4 : unit -> bool
  (** Returns the current value of {!camlp4}. *)

val set_camlp4 : bool -> unit
  (** Modifies {!camlp4}. *)

val phrase_terminator : string signal
  (** The phrase terminator. It is ";;" by default and ";" when you
      use revised syntax. *)

val get_phrase_terminator : unit -> string
  (** Returns the value of {!phrase_terminator}. *)

val set_phrase_terminator : string -> unit
  (** Modifies {!phrase_terminator}. *)

val auto_run_lwt : bool signal
  (** If [true] (the default) toplevel lwt expressions are
      automatically run with [Lwt_main.run]. i.e. if you type:

      {[
        Lwt_io.printl "Hello, world"
      ]}

      this will be replaced by:

      {[
        Lwt_main.run (Lwt_io.printl "Hello, world")
      ]}
  *)

val get_auto_run_lwt : unit -> bool
  (** Returns the value of {!auto_run_lwt}. *)

val set_auto_run_lwt : bool -> unit
  (** Modifies {!auto_run_lwt}. *)

(** {6 Console specific configuration} *)

type profile = Dark | Light
    (** Profile for colors. *)

val profile : profile React.signal
  (** The color profile. It defaults to {!Dark}. This is used by the
      default prompt to choose colors. *)

val set_profile : profile -> unit
  (** Sets the color profile. *)

val size : LTerm_geom.size React.signal
  (** The current size of the terminal. This is used only in the
      console UI. *)

val key_sequence : LTerm_key.t list React.signal
  (** The current key sequence entered by the user. This is used only
      in the console UI. *)

val time : float ref
  (** The time of the beginning of the current command. *)

val prompt : LTerm_text.t React.signal ref
  (** The prompt. *)

(** {6 Hooks} *)

val new_command_hooks : (unit -> unit) Lwt_sequence.t
  (** Functions called before each new command. *)

val at_new_command : (unit -> unit) -> unit
  (** [at_new_command f] adds [f] to the hooks executed before each
      new commands. *)

(** {6 Parsing} *)

type location = int * int
    (** Type of a string-location. It is composed of a start and stop
        offsets (in bytes). *)

(** Result of a function processing a programx. *)
type 'a result =
  | Value of 'a
      (** The function succeeded and returned this value. *)
  | Error of location list * string
      (** The function failed. Arguments are a list of locations to
          highlight in the source and an error message. *)

exception Need_more
  (** Exception raised by a parser when it need more data. *)

val parse_toplevel_phrase : (string -> bool -> Parsetree.toplevel_phrase result) ref
  (** [parse_toplevel_phrase] is the function used to parse a phrase
      typed in the toplevel.

      Its arguments are:
      - [input]: the string to parse
      - [eos_is_error]

      If [eos_is_error] is [true] and the parser reach the end of
      input, then {!Parse_failure} should be returned.

      If [eos_is_error] is [false] and the parser reach the end of
      input, the exception {!Need_more} must be thrown.

      Except for {!Need_more}, the function must not raise any
      exception. *)

val parse_toplevel_phrase_default : string -> bool -> Parsetree.toplevel_phrase result
  (** The default parser. It uses the standard ocaml parser. *)

val input_name : string
  (** The name you must use in location to let ocaml know that it is
      from the toplevel. *)

val lexbuf_of_string : bool ref -> string -> Lexing.lexbuf
  (** [lexbuf_of_string eof str] is the same as [Lexing.from_string
      str] except that if the lexer reach the end of [str] then [eof] is
      set to [true]. *)

(** {6 Helpers} *)

val get_message : (Format.formatter -> 'a -> unit) -> 'a -> string
  (** [get_message printer x] applies [printer] on [x] and
      returns everything it prints as a string. *)

val get_ocaml_error_message : exn -> location * string
  (** [get_ocaml_error_message exn] returns the location and error
      message for the exception [exn] which must be an exception from
      the compiler. *)

val check_phrase : Parsetree.toplevel_phrase -> (location list * string) option
  (** [check_phrase phrase] checks that [phrase] can be executed
      without typing or compilation errors. It returns [None] if
      [phrase] is OK and an error message otherwise.

      If the result is [None] it is guaranteed that
      [Toploop.execute_phrase] won't raise any exception. *)

(**/**)

(* These variables are not used and deprecated: *)

val prompt_continue : LTerm_text.t React.signal ref
val prompt_comment : LTerm_text.t React.signal ref
val smart_accept : bool ref
val new_prompt_hooks : (unit -> unit) Lwt_sequence.t
val at_new_prompt : (unit -> unit) -> unit
