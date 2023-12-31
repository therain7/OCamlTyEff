(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

type t = (Var.t, Var.comparator_witness) Set.t

let pp ppf set =
  let open Stdlib.Format in
  let vars = Set.to_list set in
  pp_print_list ~pp_sep:(fun out () -> fprintf out " ") Var.pp ppf vars

let compare = Set.compare_direct
let sexp_of_t = Set.sexp_of_m__t (module Var)

let empty = Set.empty (module Var)
let singleton = Set.singleton (module Var)
let of_list = Set.of_list (module Var)

let is_empty = Set.is_empty
let mem = Set.mem

let add = Set.add
let union = Set.union
let union_list = Set.union_list (module Var)
let inter = Set.inter
let diff = Set.diff

let fold = Set.fold
let fold_right = Set.fold_right
