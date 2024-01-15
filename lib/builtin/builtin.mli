(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

module Prelude : module type of Prelude

val ty_env : Env.t