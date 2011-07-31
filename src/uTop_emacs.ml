(*
 * uTop_emacs.ml
 * -------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

(* Main for emacs mode. *)

open Lwt

(* Copy standard output, which will be used to send commands. *)
let command_oc = Unix.out_channel_of_descr (Unix.dup Unix.stdout)

(* +-----------------------------------------------------------------+
   | Sending commands to Emacs                                       |
   +-----------------------------------------------------------------+ *)

(* Mutex used to send commands to Emacs. *)
let command_mutex = Mutex.create ()

let send command argument =
  Mutex.lock command_mutex;
  output_string command_oc command;
  output_char command_oc ':';
  output_string command_oc argument;
  output_char command_oc '\n';
  flush command_oc;
  Mutex.unlock command_mutex

(* +-----------------------------------------------------------------+
   | Standard outputs redirection                                    |
   +-----------------------------------------------------------------+ *)

(* The output of ocaml (stdout and stderr) is redirected so the emacs
   parts of celtop can recognize it. *)

(* Continuously copy the output of ocaml to Emacs. *)
let rec copy_output which ic =
  let line = input_line ic in
  send which line;
  copy_output which ic

(* Create a thread which redirect the given output: *)
let redirect which fd =
  let fdr, fdw = Unix.pipe () in
  Unix.dup2 fdw fd;
  Unix.close fdw;
  Thread.create (copy_output which) (Unix.in_channel_of_descr fdr)

(* Redirects stdout and stderr: *)
let _ = redirect "stdout" Unix.stdout
let _ = redirect "stderr" Unix.stderr

(* +-----------------------------------------------------------------+
   | Input                                                           |
   +-----------------------------------------------------------------+ *)

(* The text typed by the user. *)
let input = ref ""

(* The position of the text already sent to ocaml in {!input}. *)
let pos = ref 0

let read_command () =
  match Lwt_main.run (Lwt_io.read_line_opt Lwt_io.stdin) with
    | None ->
        None
    | Some line ->
        match try Some (String.index line ':') with Not_found -> None with
          | None ->
              send "stderr" "':' missing!";
              exit 1
          | Some idx ->
              Some (String.sub line 0 idx, String.sub line (idx + 1) (String.length line - (idx + 1)))

let read_data ?(final_newline = true) () =
  let buf = Buffer.create 1024 in
  let rec loop first =
    match read_command () with
      | None ->
          send "stderr" "'end' command missing!";
          exit 1
      | Some ("data", data) ->
          if not first then Buffer.add_char buf '\n';
          Buffer.add_string buf data;
          loop false
      | Some ("end", _) ->
          if final_newline then Buffer.add_char buf '\n';
          Buffer.contents buf
      | Some (command, argument) ->
          Printf.ksprintf (send "stderr") "'data' or 'end' command expected, got %S!" command;
          exit 1
  in
  loop true

let rec read_input prompt buffer length =
  if !pos = String.length !input then begin
    (match prompt with
       | "# " ->
           (* New phrase. *)

           (* Reset completion. *)
           UTop_complete.reset ();

           (* Increment the command counter. *)
           UTop_private.set_count (React.S.value UTop_private.count + 1);

           send "prompt" ""
       | "* " | "  " ->
           (* Continuation of the current phrase. *)
           send "continue" ""
       | _ ->
           Printf.ksprintf (send "stderr") "unrecognized prompt %S!" prompt;
           exit 1);
    loop prompt buffer length
  end else begin
    (* There is still some pending input. *)
    let i = ref 0 in
    while !i < length && !pos < String.length !input do
      buffer.[!i] <- (!input).[!pos];
      incr i;
      incr pos
    done;
    (!i, false)
  end

and process prompt buffer length command argument =
  match command with
    | "input" ->
        input := read_data ();
        pos := 0;
        read_input prompt buffer length
    | "complete" ->
        let input = read_data ~final_newline:false () in
        let start, words = UTop_complete.complete input in
        let words = List.map fst words in
        let prefix = LTerm_read_line.common_prefix words in
        let index = String.length input - start in
        let suffix =
          if index > 0 && index <= String.length prefix then
            String.sub prefix index (String.length prefix - index)
          else
            ""
        in
        if suffix = "" then begin
          send "completion-start" "";
          List.iter (fun word -> send "completion" word) words;
          send "completion-stop" "";
        end else
          send "completion-word" suffix;
        loop prompt buffer length
    | command ->
        Printf.ksprintf (send "stderr") "unrecognized command %S!" command;
        exit 1

and loop prompt buffer length =
  match read_command () with
    | None ->
        (0, true)
    | Some (command, argument) ->
        process prompt buffer length command argument

let () =
  Toploop.read_interactive_input := read_input
