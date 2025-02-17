open Util

type t = Let of Name.t * Expr.t

let eval ~(env : Term.Env.t) : t -> (Term.Env.t, Error.t) result
  = function
  | Let (name, expr) ->
    let* term = Expr.eval ~env expr in
    Ok (Term.Env.update_or_add name term env)
;;

open Ansifmt

let tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | Let (name, expr) ->
    block
      [ simple
          [ Formatting.Token_type.Keyword, "let"
          ; Formatting.Token.space
          ]
      ; Name.tokenize name
      ; simple
          [ Formatting.Token.space
          ; Formatting.Token_type.Operator_stmt, "="
          ; Formatting.Token.space
          ]
      ; Expr.tokenize expr
      ]
;;
