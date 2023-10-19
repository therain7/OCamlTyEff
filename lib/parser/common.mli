open! Base
open Angstrom
open Ast

val ws : unit t
(** skip whitespaces *)

val parse_value_name : string t
(**
  value_name ::= (a..z | _) \{ A..Z | a..z | 0..9 | _ | ' \}
  must not be keyword
*)

val parse_const : constant t

val parse_let_binding :
  expression t -> pattern t -> (rec_flag * value_binding list) t
(**
  [let P1 = E1 and P2 = E2 and ... and Pn = En]
  [let rec P1 PArg1 = E1 and P2 = E2 and ... and Pn = En]
*)
