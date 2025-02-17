open Util

type t =
  | Illegal_application (* of what? *)
  | Undefined_name of Name.t
  | Unification_failure of Name.t * Name.t

let repr : t -> string = function
  | Illegal_application -> "illegal application"
  | Undefined_name name -> "type " ^ name ^ " is not defined"
  | Unification_failure (left, right) ->
    "failed to unify type " ^ left ^ " with type " ^ right
;;
