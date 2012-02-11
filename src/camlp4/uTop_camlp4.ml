(*
 * uTop_camlp4.ml
 * --------------
 * Copyright : (c) 2012, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lexing
open Camlp4
open Camlp4.PreCast

module Ast2pt = Camlp4.Struct.Camlp4Ast2OCamlAst.Make(Ast)

external cast_toplevel_phrase : Camlp4_import.Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase = "%identity"

let print_camlp4_error pp exn =
  Format.fprintf pp "@[<0>%a@]" Camlp4.ErrorHandler.print exn;
  Format.pp_print_flush pp ()

let parse_toplevel_phrase_camlp4 str eos_is_error =
  let eof = ref false in
  try
    let token_stream = Gram.filter (Gram.lex_string (Loc.mk UTop.input_name) str) in
    let token_stream =
      Stream.from
        (fun _ ->
           match Stream.next token_stream with
             | (EOI, _) as x ->
                 eof := true;
                 Some x
             | x ->
                 Some x)
    in
    match Gram.parse_tokens_after_filter Syntax.top_phrase token_stream with
      | Some str_item ->
          let str_item = AstFilters.fold_topphrase_filters (fun t filter -> filter t) str_item in
          UTop.Value (cast_toplevel_phrase (Ast2pt.phrase str_item))
      | None ->
          raise UTop.Need_more
  with exn ->
    if !eof && not eos_is_error then
      raise UTop.Need_more
    else
      let locs, exn =
        match exn with
          | Loc.Exc_located (loc, exn) ->
              ([(Loc.start_off loc,Loc.stop_off loc)], exn)
          | exn ->
              ([], exn)
      in
      UTop.Error (locs, UTop.get_message print_camlp4_error exn)

let () =
  UTop.set_camlp4 true;
  UTop.parse_toplevel_phrase := parse_toplevel_phrase_camlp4;
  (* Force camlp4 to display its error message. *)
  try
    ignore (!Toploop.parse_toplevel_phrase (Lexing.from_string ""))
  with _ ->
    ()
