open Util

type t = Stmt.t list

let rec eval ~(env : Term.Env.t)
  : t -> (Term.Env.t, Error.t) result
  = function
  | [] -> Ok env
  | first :: rest ->
    let* env = Stmt.eval ~env first in
    eval ~env rest
;;

let of_value_source : Value.Source.t -> t =
  List.map Stmt.of_value_stmt
;;
