(*
 * uTop_console.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* Main for console mode. *)

open CamomileLibraryDyn.Camomile
open Lwt
open Lwt_react
open LTerm_text
open LTerm_geom
open UTop_token
open UTop_styles

module String_set = Set.Make(String)

let () = UTop_private.set_ui UTop_private.Console

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = ref []

let init_history () =
  let hist_name = Filename.concat LTerm_resources.home ".utop-history" in
  (* Save history on exit. *)
  Lwt_main.at_exit (fun () -> LTerm_read_line.save_history hist_name !history);
  (* Load history. *)
  lwt h = LTerm_read_line.load_history hist_name in
  history := h;
  return ()

(* +-----------------------------------------------------------------+
   | The read-line class                                             |
   +-----------------------------------------------------------------+ *)

(* The pending line to add to the history. *)
let pending = ref None

let lparen = UChar.of_char '('
let rparen = UChar.of_char ')'
let lbrace = UChar.of_char '{'
let rbrace = UChar.of_char '}'
let lbracket = UChar.of_char '['
let rbracket = UChar.of_char ']'

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: l -> last l

class read_line ~term ~prompt =
  let pending =
    match !pending with
      | None -> ""
      | Some line -> line ^ "\n"
  in
  let pending_length = Zed_utf8.length pending in
object(self)
  inherit LTerm_read_line.read_line ~history:!history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term as super_term

  method exec = function
    | LTerm_read_line.Accept :: actions when !UTop.smart_accept && S.value self#mode = LTerm_read_line.Edition -> begin
        Zed_macro.add self#macro LTerm_read_line.Accept;
        let tokens = UTop_lexer.lex_string (pending ^ Zed_rope.to_string (Zed_edit.text self#edit)) in
        match last tokens with
          | Some (Symbol, _, _, ";;") ->
              return self#eval
          | _ ->
              self#insert (UChar.of_char '\n');
              self#exec actions
      end
    | actions ->
        super_term#exec actions

  method stylise last =
    let styled, position = super#stylise last in

    (* Syntax highlighting *)
    let stylise start stop token_style =
      for i = max 0 (start - pending_length) to stop - pending_length - 1 do
        let ch, style = styled.(i) in
        styled.(i) <- (ch, LTerm_style.merge token_style style)
      done
    in
    UTop_styles.stylise stylise (UTop_lexer.lex_string (pending ^ LTerm_text.to_string styled));

    (* Parenthesis matching. *)
    if not last then LTerm_text.stylise_parenthesis styled position styles.style_paren;

    (styled, position)

  method completion =
    let pos, words = UTop_complete.complete (pending ^ Zed_rope.to_string self#input_prev) in
    if pos < pending_length then self#set_completion 0 [] else self#set_completion (pos - pending_length) words

  initializer
    (* Set the source signal for the size of the terminal. *)
    UTop_private.set_size self#size;
    (* Set the source signal for the key sequence. *)
    UTop_private.set_key_sequence self#key_sequence;
    (* Set the prompt. *)
    self#set_prompt prompt;
    (* Call hooks. *)
    Lwt_sequence.iter_l (fun f -> f ()) UTop.new_prompt_hooks
end

(* +-----------------------------------------------------------------+
   | Toplevel integration                                            |
   +-----------------------------------------------------------------+ *)

(* The text typed by the user. *)
let input = ref ""

(* The position of the text already sent to ocaml in {!input}. *)
let pos = ref 0

(* Is it the first time [read_input] is called ? *)
let first_run = ref true

(* The read function given to ocaml. *)
let rec read_input term prompt buffer len =
  try
    if !pos = String.length !input then begin
      (* We need to get more input from the user. *)

      let prompt_to_display =
        match prompt with
          | "# " ->
              (* Reset completion. *)
              UTop_complete.reset ();

              (* increment the command counter. *)
              UTop_private.set_count (S.value UTop_private.count + 1);

              (* Add the previous line to the history. *)
              (match !pending with
                 | None ->
                     ()
                 | Some line ->
                     history := LTerm_read_line.add_entry line !history;
                     pending := None);

              (* Call hooks. *)
              Lwt_sequence.iter_l (fun f -> f ()) UTop.new_command_hooks;

              !UTop.prompt

          | "* " ->
              !UTop.prompt_comment

          | "  " ->
              !UTop.prompt_continue

          | _ ->
              Printf.ksprintf failwith "unknown prompt %S" prompt
      in

      (* Read interactively user input. *)
      let txt = Lwt_main.run (
        try_lwt
          lwt () =
            if !first_run then begin
              first_run := false;
              LTerm.fprint term "Type #utop_help for help about using utop.\n\n"
            end else
              return ()
          in
          (new read_line ~term ~prompt:prompt_to_display)#run
        finally
          LTerm.flush term
      ) in

      pending := Some (match !pending with
                         | None -> txt
                         | Some line -> line ^ "\n" ^ txt);

      (* Add a newline character at the end. *)
      input := txt ^ "\n";
      pos := 0;

      read_input term prompt buffer len
    end else begin
      (* There is still some pending input. *)
      let i = ref 0 in
      while !i < len && !pos < String.length !input do
        buffer.[!i] <- (!input).[!pos];
        incr i;
        incr pos
      done;
      (!i, false)
    end
  with LTerm_read_line.Interrupt ->
    (0, true)

let read_input_non_interactive prompt buffer len =
  let rec loop i =
    if i = len then
      return (i, false)
    else
      Lwt_io.read_char_opt Lwt_io.stdin >>= function
        | Some c ->
            buffer.[i] <- c;
            if c = '\n' then
              return (i + 1, false)
            else
              loop (i + 1)
        | None ->
            return (i, true)
  in
  Lwt_main.run (Lwt_io.write Lwt_io.stdout prompt >> loop 0)

let init_read_interactive_input () =
  (* Open the standard terminal. *)
  lwt term = Lazy.force LTerm.stdout in
  (* If standard channels are connected to a tty, use interactive
     read-line and display a welcome message: *)
  if LTerm.incoming_is_a_tty term && LTerm.outgoing_is_a_tty term then begin
    Toploop.read_interactive_input := (read_input term);

    (* Create a context to render the welcome message. *)
    let size = LTerm.size term in
    let size = { rows = 3; cols = size.cols } in
    let matrix = LTerm_draw.make_matrix size in
    let ctx = LTerm_draw.context matrix size in

    (* Draw the message in a box. *)

    let message = "Welcome to utop!" in

    LTerm_draw.fill_style ctx LTerm_style.({ none with foreground = Some lcyan });

    LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Light;
    LTerm_draw.draw_frame ctx {
      row1 = 0;
      row2 = 3;
      col1 = (size.cols - (String.length message + 4)) / 2;
      col2 = (size.cols + (String.length message + 4)) / 2;
    } LTerm_draw.Light;

    LTerm_draw.draw_styled ctx 1 ((size.cols - String.length message) / 2) (eval [B_fg LTerm_style.yellow; S message]);

    (* On Windows we must make sure we are not at the end of screen. *)
    lwt () =
      if LTerm.windows term then
        LTerm.fprint term "\n\n\n\n"
      else
        return ()
    in

    (* Render to the screen. *)
    lwt () = LTerm.print_box term ~delta:(if LTerm.windows term then -4 else 0) matrix in
    LTerm.flush term
  end else begin
    (* Otherwise fallback to classic non-interactive mode: *)
    Toploop.read_interactive_input := read_input_non_interactive;
    return ()
  end

(* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ *)

lwt () = join [
  init_history ();
  UTop_styles.load ();
  init_read_interactive_input ();
  LTerm_inputrc.load ();
]
