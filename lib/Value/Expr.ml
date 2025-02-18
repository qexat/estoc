open Util

type t =
  | App of (t * t)
  | Fun of (param * t)
  | Term of Term.t

and param = Param of Name.t

let is_atomic : t -> bool = function
  | Term _ -> true
  | _ -> false
;;

let rec eval ~(env : Term.Env.t) : t -> (Term.t, Error.t) result = function
  | App (Fun (Param name, body), arg) ->
    let* arg_term = eval ~env arg in
    eval ~env:(Term.Env.update_or_add name arg_term env) body
  | App (_, _) -> Error Error.Illegal_application
  | Fun (Param name, body) ->
    let* body_term = eval ~env body in
    Ok (Term.Forall (name, body_term))
  | Term term -> Term.eval ~env term
;;

open Ansifmt

let rec tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | App (func, arg) ->
    block
      [ parenthesize_if (Fun.negate is_atomic) tokenize func
      ; simple [ Formatting.Token.space ]
      ; parenthesize_if (Fun.negate is_atomic) tokenize arg
      ]
  | Fun (Param name, value) ->
    block
      [ simple
          [ Formatting.Token_type.Keyword, "fun"
          ; Formatting.Token.space
          ; Formatting.Token_type.Identifier, Prelude.format name ~using:(module Name)
          ; Formatting.Token.space
          ; Formatting.Token_type.Operator_expr, "->"
          ; Formatting.Token.space
          ]
      ; parenthesize_if (Fun.negate is_atomic) tokenize value
      ]
  | Term term -> Term.tokenize term
;;
