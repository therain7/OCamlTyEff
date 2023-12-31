(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Expr
open Pattern

(**
  [let P1 = E1 and P2 = E2 and ...]
  [let rec ValName1 PArg1 = E1 and P1 = E2 and ...]
*)
let parse_str_let =
  parse_let_binding parse_expression parse_pattern
  >>| fun (rec_flag, bindings) -> Str_value (rec_flag, bindings)

let parse_structure : structure t =
  let parse_structure_item =
    (*
       XXX: parsing of let structure items seems to require 2 passes.
       first it tries to parse let expression, fails when doesn't find `in`,
       then starts all over again trying to parse let structure item.

       we probably should use lookahead to check for `in`
    *)
    parse_expression >>| (fun e -> Str_eval e) <|> parse_str_let
  in
  sep_by ws parse_structure_item <* ws
