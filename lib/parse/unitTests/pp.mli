(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom

val pp : (Format.formatter -> 'a -> unit) -> 'a t -> string -> unit
(**
  Run parser on string and pretty print the output using printer.
  Used for inline expect tests
*)
