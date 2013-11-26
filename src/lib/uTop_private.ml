(*
 * uTop_private.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of utop.
 *)

open Lwt_react

let size, set_size =
  let ev, set_size = E.create () in
  (S.switch (S.const { LTerm_geom.rows = 25; LTerm_geom.cols = 80 }) ev, set_size)

let key_sequence, set_key_sequence =
  let ev, set_key_sequence = E.create () in
  (S.switch (S.const ([] : LTerm_key.t list)) ev, set_key_sequence)

let count, set_count = S.create (-1)

type ui = Console | Emacs

let ui, set_ui = S.create Console

let error_style = ref LTerm_style.none

let autoload = ref true
