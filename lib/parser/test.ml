open! Base
open Ast

(** Parse string and pretty print the output *)
let pp str = Stdlib.Format.printf "%a" pp_structure (Parser.parse_exn str)

let%expect_test "parse_let_rec_without_whitespaces" =
  pp "letrec f n = n + 1" ;
  [%expect
    {|
    [(Str_eval
        (Exp_apply (
           (Exp_apply ((Exp_ident (Ident "=")),
              (Exp_apply (
                 (Exp_apply ((Exp_ident (Ident "letrec")),
                    (Exp_ident (Ident "f")))),
                 (Exp_ident (Ident "n"))))
              )),
           (Exp_apply (
              (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "n")))),
              (Exp_constant (Const_integer 1))))
           )))
      ] |}]

let%expect_test "parse_fact" =
  pp "let rec fact n = if n < 2 then 1 else n * fact (n - 1)" ;
  [%expect
    {|
    [(Str_value (Recursive,
        [{ pat = (Pat_var "fact");
           expr =
           (Exp_function ([(Pat_var "n")],
              (Function_body
                 (Exp_ifthenelse (
                    (Exp_apply (
                       (Exp_apply ((Exp_ident (Ident "<")),
                          (Exp_ident (Ident "n")))),
                       (Exp_constant (Const_integer 2)))),
                    (Exp_constant (Const_integer 1)),
                    (Some (Exp_apply (
                             (Exp_apply ((Exp_ident (Ident "*")),
                                (Exp_ident (Ident "n")))),
                             (Exp_apply ((Exp_ident (Ident "fact")),
                                (Exp_apply (
                                   (Exp_apply ((Exp_ident (Ident "-")),
                                      (Exp_ident (Ident "n")))),
                                   (Exp_constant (Const_integer 1))))
                                ))
                             )))
                    )))
              ))
           }
          ]
        ))
      ] |}]
