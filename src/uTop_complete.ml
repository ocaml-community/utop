(*
 * uTop_complete.ml
 * ----------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open UTop_token

module String_set = Set.Make(String)

let set_of_list = List.fold_left (fun set x -> String_set.add x set) String_set.empty

(* +-----------------------------------------------------------------+
   | Directives                                                      |
   +-----------------------------------------------------------------+ *)

let get_directives () =
  Hashtbl.fold (fun k v set -> String_set.add k set) Toploop.directive_table String_set.empty

let directive_suffix dir =
  match Hashtbl.find Toploop.directive_table dir with
    | Toploop.Directive_none _ -> ";;"
    | Toploop.Directive_string _ -> " \""
    | Toploop.Directive_bool _  | Toploop.Directive_int _ | Toploop.Directive_ident _ -> " "

(* +-----------------------------------------------------------------+
   | Filtering                                                       |
   +-----------------------------------------------------------------+ *)

(* Filter blanks and comments except for the last token. *)
let rec filter tokens =
  match tokens with
    | [] -> []
    | [((Blanks | Comment | Doc), start, stop, src)] -> [(Blanks, start, stop, src)]
    | ((Blanks | Comment | Doc), _, _, _) :: rest -> filter rest
    | x :: rest -> x :: filter rest

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

let complete str =
  let tokens = UTop_lexer.lex_string str in
  (* Filter blanks and comments. *)
  let tokens = filter tokens in
  match tokens with

    (* +-------------------------------------------------------------+
       | Completion on directives                                    |
       +-------------------------------------------------------------+ *)

    (* Completion on directive names. *)
    | [(Symbol, _, stop, "#")]
    | [(Symbol, _, _, "#"); (Blanks, _, stop, _)] ->
        (stop, List.map (fun dir -> (dir, directive_suffix dir)) (String_set.elements (get_directives ())))
    | [(Symbol, _, _, "#"); ((Lident | Uident), start, _, src)] ->
        let prefix, words = LTerm_read_line.lookup src (String_set.elements (get_directives ())) in
        (start, List.map (fun dir -> (dir, directive_suffix dir)) words)

    (* Complete with ";;" when possible. *)
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, _); (String true, _, stop, _)]
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, _); (String true, _, _, _); (Blanks, _, stop, _)] ->
        (stop, [(";;", "")])

    (* Completion on packages. *)
    | [(Symbol, _, _, "#"); (Lident, _, _, "require"); (String false, start, stop, str)] ->
        let pkg = String.sub str 1 (String.length str - 1) in
        let prefix, pkgs = LTerm_read_line.lookup pkg (Fl_package_base.list_packages ()) in
        (start + 1, List.map (fun pkg -> (pkg, "\";;")) (List.sort compare pkgs))

    (* Generic completion. *)
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, dir); (Blanks, _, stop, _)] ->
        (stop,
         match try Some (Hashtbl.find Toploop.directive_table dir) with Not_found -> None with
           | Some (Toploop.Directive_none _) -> [(";;", "")]
           | Some (Toploop.Directive_string _) -> [(" \"", "")]
           | Some (Toploop.Directive_bool _) -> [("true", ";;"); ("false", ";;")]
           | Some (Toploop.Directive_int _) -> []
           | Some (Toploop.Directive_ident _) -> []
           | None -> [])
    | [(Symbol, _, _, "#"); ((Lident | Uident), _, _, dir); ((Lident | Uident), start, _, id)] ->
        (start,
         match try Some (Hashtbl.find Toploop.directive_table dir) with Not_found -> None with
           | Some (Toploop.Directive_none _) -> []
           | Some (Toploop.Directive_string _) -> []
           | Some (Toploop.Directive_bool _) ->
               let _, words = LTerm_read_line.lookup id ["true"; "false"] in
               List.map (fun w -> (w, ";;")) words
           | Some (Toploop.Directive_int _) -> []
           | Some (Toploop.Directive_ident _) -> []
           | None -> [])

    | _ ->
        (0, [])
