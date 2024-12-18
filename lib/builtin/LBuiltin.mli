(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

val ty_env : LTypes.Env.t
(** Type environment containing bindings for built-ins *)

val eval_env : LEval.Env.t
(** Eval environment containing bindings for built-ins *)

val prelude : string
