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

let rec copy_input buffer offset length =
  if offset = length then
    return (offset, false)
  else
    Lwt_io.read_char_opt Lwt_io.stdin >>= function
      | Some '\n' ->
          buffer.[offset] <- '\n';
          return (offset + 1, false)
      | Some ch ->
          buffer.[offset] <- ch;
          copy_input buffer (offset + 1) length
      | None ->
          return (offset, true)

let read_input prompt buffer length =
  match prompt with
    | "# " ->
        (* New phrase. *)
        send "prompt" "";
        Lwt_main.run (copy_input buffer 0 length)
    | "* " | "  " ->
        (* Continuation of the current phrase. *)
        send "continue" "";
        Lwt_main.run (copy_input buffer 0 length)
    | _ ->
        send "stderr" "unrecognized prompt";
        exit 1

let () =
  Toploop.read_interactive_input := read_input
