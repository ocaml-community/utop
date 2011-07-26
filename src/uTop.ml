(*
 * uTop.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open CamomileLibraryDyn.Camomile
open Lwt_react
open LTerm_text
open LTerm_geom
open LTerm_style

let size = UTop_private.size

let count = UTop_private.count

let make_prompt count size =
  let tm = Unix.localtime (Unix.time ()) in
  let txt =
    eval [
      B_bold true;
      B_fg lcyan;
      S "─( ";
      B_fg lmagenta; S(Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec); E_fg;
      S " )─< ";
      B_fg lyellow; S(Printf.sprintf "command %d" count); E_fg;
      S " >─";
    ] in
  Array.append (
    if Array.length txt > size.cols then
      Array.sub txt 0 size.cols
    else
      Array.append txt (Array.make (size.cols - Array.length txt) (UChar.of_int 0x2500, { none with foreground = Some lcyan }))
  ) [|(UChar.of_char '#', { none with foreground = Some lgreen }); (UChar.of_char ' ', none)|]

let prompt = ref (S.l2 make_prompt count size)

let prompt_continue = ref (S.const [|(UChar.of_char '>', { none with foreground = Some lgreen }); (UChar.of_char ' ', LTerm_style.none)|])

let () =
  (* Do not load packages linked with the toplevel. *)
  Topfind.don't_load_deeply ["utop"; "findlib"; "lambda-term"]
