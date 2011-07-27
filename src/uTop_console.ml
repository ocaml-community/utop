(*
 * uTop_console.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* Main for console mode. *)

open Lwt
open Lwt_react
open LTerm_text
open LTerm_geom
open UTop_token

module String_set = Set.Make(String)

(* +-----------------------------------------------------------------+
   | Resources                                                       |
   +-----------------------------------------------------------------+ *)

type styles = {
  mutable style_keyword : LTerm_style.t;
  mutable style_symbol : LTerm_style.t;
  mutable style_ident : LTerm_style.t;
  mutable style_constant : LTerm_style.t;
  mutable style_char : LTerm_style.t;
  mutable style_string : LTerm_style.t;
  mutable style_quotation : LTerm_style.t;
  mutable style_comment : LTerm_style.t;
  mutable style_doc : LTerm_style.t;
  mutable style_blanks : LTerm_style.t;
  mutable style_error : LTerm_style.t;
}

let styles = {
  style_keyword = LTerm_style.none;
  style_symbol = LTerm_style.none;
  style_ident = LTerm_style.none;
  style_constant = LTerm_style.none;
  style_char = LTerm_style.none;
  style_string = LTerm_style.none;
  style_quotation = LTerm_style.none;
  style_comment = LTerm_style.none;
  style_doc = LTerm_style.none;
  style_blanks = LTerm_style.none;
  style_error = LTerm_style.none;
}

let init_resources () =
  try_lwt
    lwt res = LTerm_resources.load (Filename.concat (try Sys.getenv "HOME" with Not_found -> "") ".utoprc") in
    styles.style_keyword <- LTerm_resources.get_style "keyword" res;
    styles.style_symbol <- LTerm_resources.get_style "symbol" res;
    styles.style_ident <- LTerm_resources.get_style "identifier" res;
    styles.style_constant <- LTerm_resources.get_style "constant" res;
    styles.style_char <- LTerm_resources.get_style "char" res;
    styles.style_string <- LTerm_resources.get_style "string" res;
    styles.style_quotation <- LTerm_resources.get_style "quotation" res;
    styles.style_comment <- LTerm_resources.get_style "comment" res;
    styles.style_doc <- LTerm_resources.get_style "doc" res;
    styles.style_blanks <- LTerm_resources.get_style "blanks" res;
    styles.style_error <- LTerm_resources.get_style "error" res;
    return ()
  with Unix.Unix_error(Unix.ENOENT, _, _) ->
    return ()

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = ref []

let init_history () =
  let hist_name = Filename.concat (try Sys.getenv "HOME" with Not_found -> "") ".utop-history" in
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

class read_line ~term ~prompt =
  let pending =
    match !pending with
      | None -> ""
      | Some line -> line ^ "\n"
  in
  let pending_length = Zed_utf8.length pending in
object(self)
  inherit LTerm_read_line.read_line ~history:!history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method stylise =
    let styled, position = super#stylise in
    let tokens = UTop_lexer.lex_string (pending ^ LTerm_text.to_string styled) in
    let rec loop tokens =
      match tokens with
        | [] ->
            ()
        | (token, start, stop, src) :: rest ->
            let token_style =
              match token with
                | Symbol -> styles.style_symbol
                | Lident -> if String_set.mem src !UTop.keywords then styles.style_keyword else styles.style_ident
                | Uident -> styles.style_ident
                | Constant -> styles.style_constant
                | Char -> styles.style_char
                | String _ -> styles.style_string
                | Quotation _ -> styles.style_quotation
                | Comment _ -> styles.style_comment
                | Doc _ -> styles.style_doc
                | Blanks -> styles.style_blanks
                | Error -> styles.style_error
            in
            for i = start - pending_length to stop - pending_length - 1 do
              let ch, style = styled.(i) in
              styled.(i) <- (ch, LTerm_style.merge token_style style)
            done;
            loop rest
    in
    let rec skip tokens =
        match tokens with
          | [] ->
              ()
          | (token, start, stop, src) :: rest ->
              if stop = pending_length then
                loop rest
              else if stop > pending_length then
                loop ((token, pending_length, stop, Zed_utf8.sub src (pending_length - start) (stop - pending_length)) :: rest)
              else
                skip rest
    in
    if pending_length = 0 then loop tokens else skip tokens;
    (styled, position)

  method completion =
    let pos, words = UTop_complete.complete (pending ^ Zed_rope.to_string self#input_prev) in
    if pos < pending_length then self#set_completion 0 [] else self#set_completion (pos - pending_length) words

  initializer
    (* Set the source signal for the size of the terminal. *)
    UTop_private.set_size self#size;
    (* Set the prompt. *)
    self#set_prompt prompt
end

(* +-----------------------------------------------------------------+
   | Toplevel integration                                            |
   +-----------------------------------------------------------------+ *)

(* The text typed by the user. *)
let input = ref ""

(* The position of the text already sent to ocaml in {!input}. *)
let pos = ref 0

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
        lwt txt = (new read_line ~term ~prompt:prompt_to_display)#run in
        lwt () = LTerm.flush term in
        return txt
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
  (* If standard channels are connected to a tty, use interactive
     read-line and display a welcome message: *)
  if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then begin
    (* Open the standard terminal. *)
    lwt term = Lazy.force LTerm.stdout in

    Toploop.read_interactive_input := (read_input term);

    (* Create a context to render the welcome message. *)
    lwt size = LTerm.get_size term in
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

    (* Render to the screen. *)
    lwt () = LTerm.print_box term matrix in
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
  init_resources ();
  init_read_interactive_input ();
]
