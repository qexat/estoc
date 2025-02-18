(* stmt shorthands *)
let let' name body = Stmt.Let (name, body)

(* expr shorthands *)
let app func arg = Expr.App (func, arg)
let fun' param_name body = Expr.Fun (Param param_name, body)

(* term-expr shorthands *)
let forall name func = Expr.Term (Term.Forall (name, func (Term.Var name)))
let int (value : int) = Expr.Term (Term.Int value)
let unit = Expr.Term Term.Unit
let var name = Expr.Term (Term.Var name)
