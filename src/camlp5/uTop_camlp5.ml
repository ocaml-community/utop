(*
 * uTop_camlp5.ml
 * --------------
 * Copyright : (c) 2012, Wojciech Meyer <wojciech.meyer@gmail.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lexing

let print_camlp5_error pp exn =
  let save = Format.get_formatter_output_functions () in
  Format.set_formatter_output_functions 
    (fun str s e -> Format.pp_print_string pp (String.sub str s e))
    (fun () -> Format.pp_print_flush pp ());
  Format.printf "@[<0>%a@]@." (fun _ -> Pcaml.report_error) exn;
  Format.set_formatter_output_functions (fst save) (snd save)
    
let get_camlp5_error_message exn =
  let loc, exn =
    match exn with
      | Ploc.Exc (loc, exn) ->
          ((Ploc.first_pos loc, Ploc.last_pos loc), exn)
      | exn ->
          ((0, 0), exn)
  in
  let msg = UTop.get_message print_camlp5_error exn in
  loc, msg

let convert_camlp5_toplevel_phrase ast =
  try
    UTop.Value (Ast2pt.phrase ast)
  with exn ->
    let loc, msg = get_camlp5_error_message exn in
    UTop.Error ([loc], msg)

let parse_toplevel_phrase_camlp5 str eos_is_error =
  try
    let token_stream = Stream.of_string str in
    match Grammar.Entry.parse Pcaml.top_phrase token_stream with
      | Some ast ->
          UTop.Value ast
      | None ->
          raise UTop.Need_more
  with exn ->
    if not eos_is_error then
      raise UTop.Need_more
    else
      let loc, msg = get_camlp5_error_message exn in
      UTop.Error ([loc], msg)

let parse_toplevel_phrase str eos_is_error =
  match parse_toplevel_phrase_camlp5 str eos_is_error with
    | UTop.Value ast ->
        convert_camlp5_toplevel_phrase ast
    | UTop.Error (locs, msg) ->
        UTop.Error (locs, msg)

let () =
  UTop.parse_toplevel_phrase := parse_toplevel_phrase;
  (* Force camlp5 to display its welcome message. *)
  try
    ignore (!Toploop.parse_toplevel_phrase (Lexing.from_string ""))
  with _ ->
    ()
