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

(* +-----------------------------------------------------------------+
   | The read-line class                                             |
   +-----------------------------------------------------------------+ *)

class read_line ~term ~history ~prompt = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  initializer
    (* Set the source signal for the size of the terminal. *)
    UTop_private.set_size self#size;
    (* Set the prompt. *)
    self#set_prompt prompt
end

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let history = ref []

let () =
  let hist_name = Filename.concat (try Sys.getenv "HOME" with Not_found -> "") ".utop-history" in
  (* Save history on exit. *)
  Lwt_main.at_exit (fun () -> LTerm_read_line.save_history hist_name !history);
  (* Load history. *)
  history := Lwt_main.run (LTerm_read_line.load_history hist_name)

(* +-----------------------------------------------------------------+
   | Toplevel integration                                            |
   +-----------------------------------------------------------------+ *)

(* The text typed by the user. *)
let input = ref ""

(* The position of the text already sent to ocaml in {!input}. *)
let pos = ref 0

(* The pending line to add to the history. *)
let pending = ref ""

(* The read function given to ocaml. *)
let rec read_input term prompt buffer len =
  try
    if !pos = String.length !input then begin
      (* We need to get more input from the user. *)

      let prompt_to_display =
        if prompt = "# " then begin
          (* This is a new command. *)

          (* increment the command counter. *)
          UTop_private.set_count (S.value UTop_private.count + 1);

          (* Add the previous line to the history. *)
          history := LTerm_read_line.add_entry !pending !history;
          pending := "";

          !UTop.prompt
        end else
          !UTop.prompt_continue
      in

      (* Read interactively user input. *)
      let txt = Lwt_main.run (
        lwt txt = (new read_line ~term ~history:!history ~prompt:prompt_to_display)#run in
        lwt () = LTerm.flush term in
        return txt
      ) in

      pending := !pending ^ txt;

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

(* +-----------------------------------------------------------------+
   | Integration for when the input is not a terminal                |
   +-----------------------------------------------------------------+ *)

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

lwt () =
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
