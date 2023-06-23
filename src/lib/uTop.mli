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

val require : string list -> unit
  (** Load all the given findlib packages *)

type ui = Console | Emacs
    (** The user interface in use. *)

val get_ui : unit -> ui
  (** Returns the user interface in use. *)

val hide_reserved : bool signal
  (** If [true] (the default) identifiers starting with a '_' will be hidden from the
      output. i.e. the following phrase won't produces any output:

      {[
        let _x = 1
      ]}

      This is for hidding variables created by code generators for internal use. It can
      also be set/unset by the command line options [-hide-reserved] and [-show-reserved].
  *)

val get_hide_reserved : unit -> bool
  (** Returns the value of {!hide_reserved}. *)

val set_hide_reserved : bool -> unit
  (** Modifies {!hide_reserved}. *)

val create_implicits : bool signal
  (** If [true] (not the default) expressions entered in the toplevel are
      automatically bound, for example:

      {[
        # 3 + 4;;
        _0 : int = 7
        # _0 + 10;;
        _1 : int = 17
      ]}
  *)

val get_create_implicits : unit -> bool
  (** Returns the value of {!create_implicits}. *)

val set_create_implicits : bool -> unit
  (** Modifies {!create_implicits}. *)

val topfind_verbose : bool signal
  (** If [false] (the default) messages from findlib are hidden. This is only effective
      with findlib >= 1.4. *)

val get_topfind_verbose : unit -> bool
  (** Returns the value of {!topfind_verbose}. *)

val set_topfind_verbose : bool -> unit
  (** Modifies {!topfind_verbose}. *)

val topfind_log : string list signal
  (** List of messages logged by findlib since the beginning of the session. This
      requires findlib >= 1.4. *)

val show_box : bool signal
  (** If [true] (the default) the completion bar is displayed. *)

val get_show_box : unit -> bool
  (** Returns the value of {!show_box}. *)

val set_show_box : bool -> unit
  (** Modifies {!show_box}. *)

val set_margin_function : (LTerm_geom.size -> int option) -> unit
  (** Margin of the standard and error formatters as a function of the screen size.

      The default is:

      {[
        fun size -> Some (min 80 size.cols)
      ]}
  *)

val phrase_terminator : string signal
  (** The phrase terminator, ";;". *)

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

val auto_run_async : bool signal
  (** If [true] (the default) toplevel Async expressions are
      automatically run with in a separate thread with
      [Thread_safe.block_on_async_exn]. i.e. if you type:

      {[
        after (Time.Span.of_s 1.0)
      ]}

      this will be replaced by:

      {[
        Thread_safe.block_on_async_exn (fun () -> after (Time.Span.of_s 1.0))
      ]}
  *)

val get_auto_run_async : unit -> bool
  (** Returns the value of {!auto_run_async}. *)

val set_auto_run_async : bool -> unit
  (** Modifies {!auto_run_async}. *)

val end_and_accept_current_phrase : LTerm_read_line.action
 (** Action that add the phrase terminator at the end of the current phrase
     and accepts it. For instance to avoid typing [;;], add this to your
     ~/.config/utop/init.ml:

     {[
       #require "lambda-term";;
       LTerm_read_line.bind
         [ { control = false; meta = false; shift = false; code = Enter } ]
         [ UTop.end_and_accept_current_phrase ]
     ]}
 *)

(** External editor command. [None] for default. *)
val external_editor : string signal
val set_external_editor : string -> unit
val get_external_editor : unit -> string

(** {6 History} *)

val history : LTerm_history.t
  (** The history used by utop. You can configure limits using the
      [LTerm_history] module.

      For example if you want to limit the history to 1000 line, add
      these lines to your ~/.config/utop/init.ml file:

      {[
        #require "lambda-term";;
        LTerm_history.set_max_entries UTop.history 1000;;
      ]}
  *)

val history_file_name : string option ref
  (** Name of the history file. If [None], no history will be loaded
      or saved. *)

val history_file_max_size : int option ref
  (** Maximum size of the history file. If [None] (the default) the
      maximum size of [history] will be used. *)

val history_file_max_entries : int option ref
  (** Maximum entries to store in the history file. If [None] (the
      default) the maximum number of entries if [history] will be
      used. *)

val stashable_session_history : UTop_history.t
  (** A history consisting of inputs and resulting values or errors of the
      current session. Because stashing is supposed to produce a valid OCaml
      file which will behave roughly the same as the console, it is best if
      this history never gets truncated. While this will certainly lead to a
      slight memory leaking problem, UTop sessions are rarely long enough to
      make it a serious issue. *)

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

(** {6 Edit mode configuration} *)

val edit_mode : LTerm_editor.mode ref
  (** The edit mode. *)

(** {6 Hooks} *)

val new_command_hooks : (unit -> unit) LTerm_dlist.t
  (** Functions called before each new command. *)

val at_new_command : (unit -> unit) -> unit
  (** [at_new_command f] adds [f] to the hooks executed before each
      new commands. *)

(** {6 Parsing} *)

type location = int * int
    (** Type of a string-location. It is composed of a start and stop
        offsets (in bytes). *)

type lines = {
  start: int;
  stop:  int;
}
    (** Type for a range of lines in a buffer from start to stop. *)

(** Result of a function processing a programx. *)
type 'a result =
  | Value of 'a
      (** The function succeeded and returned this value. *)
  | Error of location list * string
      (** The function failed. Arguments are a list of locations to
          highlight in the source and an error message. *)

exception Need_more
  (** Exception raised by a parser when it need more data. *)

val parse_use_file : (string -> bool -> Parsetree.toplevel_phrase list result) ref

val parse_use_file_default : string -> bool -> Parsetree.toplevel_phrase list result
  (** The default parser for toplevel regions. It uses the standard
      ocaml parser. *)

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
  (** The default parser for toplevel phrases. It uses the standard
      ocaml parser. *)

val parse_default : (Lexing.lexbuf -> 'a) -> string -> bool -> 'a result
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

val get_ocaml_error_message : exn -> location * string * (lines option)
  (** [get_ocaml_error_message exn] returns the location and error
      message for the exception [exn] which must be an exception from
      the compiler. *)

val check_phrase : Parsetree.toplevel_phrase -> (location list * string * lines option list) option
  (** [check_phrase phrase] checks that [phrase] can be executed
      without typing or compilation errors. It returns [None] if
      [phrase] is OK and an error message otherwise.

      If the result is [None] it is guaranteed that
      [Toploop.execute_phrase] won't raise any exception. *)

val collect_formatters : Buffer.t -> Format.formatter list -> (unit -> 'a) -> 'a
  (** [collect_formatters buf pps f] executes [f] and redirect
      everything it prints on [pps] to [buf]. *)

val discard_formatters : Format.formatter list -> (unit -> 'a) -> 'a
  (** [discard_formatters pps f] executes [f], dropping everything it
      prints on [pps]. *)

val split_words : string -> string list

 (** {6 compiler-libs reexports} *)

val get_load_path : unit -> string list
val set_load_path : string list -> unit
  (** [get_load_path] and [set_load_path] manage the include directories.

      The internal variable contains the list of directories added by findlib-required packages
      and [#directory] directives. *)

(**/**)

module Private : sig
  val fix_string : string -> string
end
