open! Base
open Ast

type t = (Ident.t, Scheme.t, Ident.comparator_witness) Map.t

let empty = Map.empty (module Ident)

let singleton = Map.singleton (module Ident)

let of_alist_exn = Map.of_alist_exn (module Ident)

let find = Map.find

let find_exn = Map.find_exn

let set = Map.set
