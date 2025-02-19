open Ansifmt.Prelude
open Util

type t = Stmt.t list

let rec eval ~(env : Term.Env.t) : t -> (Term.Env.t, Error.t) result = function
  | [] -> Ok env
  | first :: rest ->
    let* env = Stmt.eval ~env first in
    eval ~env rest
;;

let eval_checked ~(env : Term.Env.t) (source : t) : (Term.Env.t, Error.t) result =
  let* env = eval ~env source in
  (* TODO: do the check instead of just printing *)
  print_formatted ~line_end:"" env ~using:(module Term.Env);
  Ok env
;;

let run ~(env : Term.Env.t ref) (source : t) : (unit, Error.t) result =
  let* new_env = eval ~env:!env source in
  env := new_env;
  print_formatted ~line_end:"" !env ~using:(module Term.Env);
  Ok ()
;;

let of_value_source : Value.Source.t -> t = List.map Stmt.of_value_stmt
