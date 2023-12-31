(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types

let run code =
  let open Stdlib.Format in
  match Parse.parse code with
  | Some [str_item] -> (
    match Infer.infer_structure_item StdEnv.env str_item with
    | Ok (env, bounds, sc) ->
        printf "%a\n" Scheme.pp sc ;

        List.iter bounds ~f:(fun bound_var ->
            let sc = Env.find_exn env bound_var in
            let (Ident name) = bound_var in
            printf "%s: %a\n" name Scheme.pp sc )
    | Error err ->
        Infer.TyError.pp Stdlib.Format.std_formatter err )
  | None ->
      Stdlib.print_endline "syntax error"
  | _ ->
      Stdlib.print_endline "invalid test"

let%expect_test _ =
  run {|
    let id1 = fun x -> x in
    id1 42; id1 "hello"
  |} ;
  [%expect {| string |}]

let%expect_test _ =
  run {|
    let f id = id 42; id "hello" in
    f (fun x -> x)
  |} ;
  [%expect {| (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {| 'a 'b. ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun x f -> f x |} ; [%expect {| 'a 'b. 'a -> ('a -> 'b) -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {| 'a 'b. ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (Ident "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect {| 'a. (bool -> 'a) -> 'a |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect {| (OccursIn ('gen1, 'gen1 -> 'gen5)) |}]

let%expect_test _ =
  run {| fun x y (a, _) -> (x + y - a) = 1 |} ;
  [%expect {| 'a. int -> int -> (int * 'a) -> bool |}]

let%expect_test _ =
  run {|
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x |} ;
  [%expect {| int |}]

let%expect_test _ =
  run {| Some (1, "hi") |} ; [%expect {| (int * string) option |}]

let%expect_test _ = run {| None |} ; [%expect {| 'a. 'a option |}]

let%expect_test _ =
  run {| Some |} ; [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| None 42 |} ; [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| None None |} ;
  [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| let Some = Some 1 in 0 |} ;
  [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| let x, Some x = 1, Some 2 in x |} ;
  [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| fun x x -> x |} ; [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| let a, _ = 1, 2, 3 in a |} ;
  [%expect {| UnificationMismatch |}]

let%expect_test _ =
  run {| let a = 1, (fun (a, _) -> a), 2 in a|} ;
  [%expect {| 'a 'b. int * (('a * 'b) -> 'a) * int |}]

let%expect_test _ =
  run
    {|
    match Some id with
      | Some x -> x "hi"; x 5
      | None -> 1
    |} ;
  [%expect {| int |}]

let%expect_test _ =
  run
    {|
    fun x ->
      match x with
        | Some v -> Some (v + 1)
        | None -> None
    |} ;
  [%expect {| int option -> int option |}]

let%expect_test _ =
  run {| function Some x -> x | None -> 0 |} ;
  [%expect {| int option -> int |}]

let%expect_test _ =
  run {| function Some id -> id "hi"; id 5 | None -> 1 |} ;
  [%expect {| (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| fun arg -> match arg with Some x -> let y = x in y |} ;
  [%expect {| 'a. 'a option -> 'a |}]

let%expect_test _ =
  run {| function [x] -> let y = x in y |} ;
  [%expect {| 'a. 'a list -> 'a |}]

let%expect_test _ =
  run {| function 42 -> true | _ -> false |} ;
  [%expect {| int -> bool |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1) in fact |} ;
  [%expect {| int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true in fact |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1)  |} ;
  [%expect {|
    int -> int
    fact: int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true  |} ;
  [%expect {| (UnificationFail (bool, int)) |}]

let%expect_test _ =
  run {| let rec f x = f 5 in f |} ;
  [%expect {| 'a. int -> 'a |}]

let%expect_test _ =
  run {| let rec _ = id in 1 |} ;
  [%expect {| NotVarLHSRec |}]

let%expect_test _ = run {| let rec _ = id |} ; [%expect {| NotVarLHSRec |}]

let%expect_test _ =
  run {| let rec Some x = Some 1 in x |} ;
  [%expect {| NotVarLHSRec |}]

let%expect_test _ =
  run {| let f x = x |} ; [%expect {|
    'a. 'a -> 'a
    f: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let id1, id2 = id, id |} ;
  [%expect
    {|
    'a 'b. ('a -> 'a) * ('b -> 'b)
    id1: 'a. 'a -> 'a
    id2: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let Some a = (<) |} ;
  [%expect {| (UnificationFail ('solve0 option, int -> int -> bool)) |}]

let%expect_test _ =
  run {| let Some x = Some id |} ;
  [%expect {|
    'a. ('a -> 'a) option
    x: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let () = id |} ;
  [%expect {| (UnificationFail (unit, 'solve0 -> 'solve0)) |}]

let%expect_test _ =
  run {| let [a; b] = [(1,2); (3,4)] |} ;
  [%expect {|
    (int * int) list
    a: int * int
    b: int * int |}]

let%expect_test _ =
  run {| let [Some(a, b)] = [Some(1,2)] |} ;
  [%expect {|
    (int * int) option list
    a: int
    b: int |}]
