(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Run

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
  [%expect {| (UnificationFailTy (string, int)) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {| 'a 'b 'e. ('a -'e-> 'b) -> 'a -'e-> 'b |}]

let%expect_test _ =
  run {| fun x f -> f x |} ;
  [%expect {| 'a 'b 'e. 'a -> ('a -'e-> 'b) -'e-> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {| 'a 'b 'e. ('a -'e-> 'b) -> 'a -'e-> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (Ident "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect {| 'a 'e. (bool -'e-> 'a) -'e-> 'a |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect {| (UnificationFailTy (int, bool)) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect {| (UnificationFailTy (bool, int)) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect {| (OccursInTy ('gen6, 'gen6 -'gen20-> 'gen18)) |}]

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
  [%expect {| (UnificationFailTy (string, int)) |}]

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
  [%expect {| (UnificationFailTy (bool, int)) |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1)  |} ;
  [%expect {|
    fact: int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true  |} ;
  [%expect {| (UnificationFailTy (bool, int)) |}]

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

let%expect_test _ = run {| let f x = x |} ; [%expect {|
    f: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let id1, id2 = id, id |} ;
  [%expect {|
    id1: 'a. 'a -> 'a
    id2: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let Some a = (<) |} ;
  [%expect
    {| (UnificationFailTy ('solve1 option, int -'solve3-> int -'solve2-> bool)) |}]

let%expect_test _ =
  run {| let Some x = Some id |} ;
  [%expect {|
    x: 'a. 'a -> 'a |}]

let%expect_test _ =
  run {| let () = id |} ;
  [%expect {| (UnificationFailTy (unit, 'solve1 -'solve0-> 'solve1)) |}]

let%expect_test _ =
  run {| let [a; b] = [(1,2); (3,4)] |} ;
  [%expect {|
    a: int * int
    b: int * int |}]

let%expect_test _ =
  run {| let [Some(a, b)] = [Some(1,2)] |} ;
  [%expect {|
    a: int
    b: int |}]

let%expect_test _ =
  run {| let rec x = x + 1 |} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
          (Exp_constant (Const_integer 1))))) |}]

let%expect_test _ =
  run {| let rec x = x + 1 in x |} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
          (Exp_constant (Const_integer 1))))) |}]

let%expect_test _ =
  run {| let rec y = 1 in let rec x = y in x |} ;
  [%expect {| int |}]

let%expect_test _ =
  run
    {|
      type 'a list = Nil | Cons of 'a * 'a list
      type unit = ()
      type foo = Foo

      exception My_exc of string
    |} ;
  [%expect
    {|
    Cons: 'a. ('a * 'a list) -> 'a list
    Nil: 'a. 'a list
    (): unit
    Foo: foo
    My_exc: string -> _My_exc exception |}]

let%expect_test _ =
  run {| type foo = Foo of 'a list |} ;
  [%expect {| (UnboundTypeVariable "a") |}]

let%expect_test _ =
  run {| type foo = Foo of list |} ;
  [%expect {| (TypeArityMismatch (Ident "list")) |}]

let%expect_test _ =
  run {| type foo = Foo of bar |} ;
  [%expect {| (UnboundType (Ident "bar")) |}]

let%expect_test _ =
  run {| type foo = Foo
         type 'a foo = Foo of foo |} ;
  [%expect {|
    Foo: foo
    (TypeArityMismatch (Ident "foo")) |}]

let%expect_test _ =
  run {| function Some x | Some y -> 0 |} ;
  [%expect {| (VarsMismatchOrPattern (Ident "x")) |}]

let%expect_test _ =
  run {| function Some x | Some (Some x) -> 1 |} ;
  [%expect {| (OccursInTy ('solve5, 'solve5 option)) |}]

let%expect_test _ =
  run {| function Some 1 | Some "42" -> 0 |} ;
  [%expect {| (UnificationFailTy (int, string)) |}]

let%expect_test _ =
  run {| function 1 | 2 -> 0 |} ;
  [%expect {| int -> int |}]
