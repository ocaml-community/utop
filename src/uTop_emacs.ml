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

let rec read_input prompt buffer length =
  if !pos = String.length !input then begin
    (match prompt with
       | "# " ->
           (* New phrase. *)
           send "prompt" ""
       | "* " | "  " ->
           (* Continuation of the current phrase. *)
           send "continue" ""
       | _ ->
           Printf.ksprintf (send "stderr") "unrecognized prompt %S!" prompt;
           exit 1);
    match read_command () with
      | None ->
          (0, true)
      | Some (command, argument) ->
          process prompt buffer length command argument
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
        let count =
          try
            int_of_string argument
          with _ ->
            send "stderr" "invalid number of line to read!";
            exit 1
        in
        input := "";
        pos := 0;
        for i = 1 to count do
          match read_command () with
            | None ->
                send "stderr" "wrong number of lines!";
                exit 1
            | Some ("data", data) ->
                input := !input ^ data ^ "\n"
            | Some (command, argument) ->
                Printf.ksprintf (send "stderr") "'data' command expected, got %S!" command;
                exit 1
        done;
        read_input prompt buffer length
    | command ->
        Printf.ksprintf (send "stderr") "unrecognized command %S!" command;
        exit 1

let () =
  Toploop.read_interactive_input := read_input
