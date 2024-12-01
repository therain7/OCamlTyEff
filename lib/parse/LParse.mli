(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

val parse : string -> LAst.structure option
(** Tries to parse [string]. Returns [None] if parsing fails *)

val parse_ty_exn : string -> LTypes.Ty.t
(** Tries to parse [string]. Raises [Failure] exception if parsing fails *)
