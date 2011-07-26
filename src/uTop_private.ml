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
  (S.switch (S.const { LTerm_geom.rows = 0; LTerm_geom.cols = 0 }) ev, set_size)

let count, set_count = S.create(-1)
