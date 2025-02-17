open Util

type t = Let of Index.t * Expr.t

let eval ~(env : Term.Env.t) : t -> (Term.Env.t, Error.t) result
  = function
  | Let (name, expr) ->
    let* term = Expr.eval ~env expr in
    (match Term.Env.get name env with
     | None -> Ok ((name, term) :: env)
     | Some _ ->
       (* TODO: implement unification algorithm *)
       Ok (Term.Env.update_or_add name term env))
;;

let of_value_stmt : Value.Stmt.t -> t = function
  | Let (name, expr) ->
    let index = Term.instantiate_variable name in
    Let (index, Expr.of_value_expr expr)
;;
