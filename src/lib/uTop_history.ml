(*
 * uTop_history.ml
 * -----------------
 * Copyright : (c) 2017, Fabian Hemmer <copy@copy.sh>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

type entry =
  | Input of string
  | Output of string
  | Error of string
  | Warnings of string
  | Bad_input of string
and t = entry list ref

let create () : t =
  ref []

let contents (t : t) =
  !t

let strip_colors s =
  let len = String.length s in
  let find_escape offset =
    try
      let i = String.index_from s offset '\027' in
      if i = len - 1 || s.[i + 1] <> '[' then
        None
      else
        Some i
    with
    | Not_found -> None
  in
  let find_color_escapes offset =
    let rec aux acc offset =
      match find_escape offset with
      | None -> (offset, len) :: acc
      | Some esc_offset ->
         try
           let i = String.index_from s esc_offset 'm' in
           aux ((offset, esc_offset) :: acc) (i + 1)
         with
         | Not_found -> (offset, len) :: acc
    in
    aux [] offset
  in
  find_color_escapes 0
    |> List.rev_map (fun (i, j) -> String.sub s i (j - i))
    |> String.concat ""

let add history v =
  history := v :: !history

let add_input history input =
  add history @@ Input (String.trim input)

let add_output history output =
  let output = String.trim output in
  if output <> "" then (* directives produce empty output *)
    add history @@ Output output

let add_error history error =
  add history @@ Error (strip_colors @@ String.trim error)

let add_bad_input history input =
  add history @@ Bad_input (String.trim input)

let add_warnings history warnings =
  let warnings = String.trim warnings in
  if warnings <> "" then
    add history @@ Warnings warnings
