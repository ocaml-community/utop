(*
 * uTop_private.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lwt_react

module Default_paths = struct
  let ( / ) = Filename.concat
  let xdg = Xdg.create ~env:Sys.getenv_opt ()

  let resolve ~legacy ~filename =
    if Sys.file_exists legacy then
      legacy
    else
      filename

  let history_file_name =
    resolve
      ~legacy:(LTerm_resources.home / ".utop-history")
      ~filename:(Xdg.state_dir xdg / "utop-history")

  let config_file_name =
    resolve
      ~legacy:(LTerm_resources.home / ".utoprc")
      ~filename:(Xdg.config_dir xdg / "utoprc")
end

let size, set_size =
  let ev, set_size = E.create () in
  let init = S.const { LTerm_geom.rows = 25; LTerm_geom.cols = 80 } in
  (S.switch (S.hold ~eq:( == ) init ev), set_size)

let key_sequence, set_key_sequence =
  let ev, set_key_sequence = E.create () in
  let init = (S.const ([] : LTerm_key.t list)) in
  (S.switch (S.hold ~eq:( == ) init ev), set_key_sequence)

let count, set_count = S.create (-1)

type ui = Console | Emacs

let ui, set_ui = S.create Console

let error_style = ref LTerm_style.none

(* Config from $XDG_CONFIG_HOME/utop/utoprc *)
let autoload = ref true

let margin_function, set_margin_function =
  S.create ~eq:( == ) (fun (size : LTerm_geom.size) -> Some (min 80 size.cols))

let margin = S.app margin_function size

let set_margin pp =
  match S.value margin with
  | None   -> ()
  | Some n -> if Format.pp_get_margin pp () <> n then Format.pp_set_margin pp n
