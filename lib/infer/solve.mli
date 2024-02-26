(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints

val weaken : Ty.t -> Ty.t
(** Convert all variables in a type to weak ones *)

val generalize : VarSet.t -> Ty.t -> Scheme.t
(** Quantify all variables except weak ones
    and those present in passed varset *)

val solve : rec_types:bool -> ConstrSet.t -> (Sub.t, TyError.t) result
