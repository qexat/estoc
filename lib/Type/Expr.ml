open Util

type t =
  | App of (t * t)
  | Fun of (param * t)
  | Term of Term.t

and param = Param of Index.t

let rec eval ~(env : Term.Env.t) : t -> (Term.t, Error.t) result
  = function
  | App (Fun (Param index, ty), arg) ->
    let* arg_term = eval ~env arg in
    eval ~env:(Term.Env.update_or_add index arg_term env) ty
  | App (_, _) -> Error Error.Illegal_application
  | Fun (Param index, ty) ->
    let* type_term = eval ~env ty in
    Ok (Term.Forall (index, type_term))
  | Term term -> Term.eval ~env term
;;

let rec of_value_expr : Value.Expr.t -> t = function
  | App (func, arg) -> App (of_value_expr func, of_value_expr arg)
  | Fun (Param name, value) ->
    let index = Term.instantiate_variable name in
    Fun (Param index, of_value_expr value)
  | Term term -> Term (Term.of_value_term term)
;;
