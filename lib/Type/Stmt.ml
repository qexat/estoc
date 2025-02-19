open Util

type t = Let of Index.t * Expr.t

let eval ~(env : Term.Env.t) : t -> (Term.Env.t, Error.t) result = function
  | Let (name, body) ->
    let* term = Expr.eval ~env body in
    (match Term.Env.get name env with
     | None -> Ok (Term.Env.add name term env)
     | Some _ -> Ok (Term.Env.update_lenient name term env))
;;

let of_value_stmt : Value.Stmt.t -> t = function
  | Let (name, body) ->
    let index = Term.instantiate_variable name in
    Let (index, Expr.of_value_expr body)
;;
