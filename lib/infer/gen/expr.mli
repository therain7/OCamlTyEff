(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open LTypes
open LAst

open Common

val gen : expression -> (Assumptions.t * Ty.t * Eff.t) GenMonad.t
