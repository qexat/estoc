open Util
include Term_base
module Env = Mapping.Make (Name) (Term_base)

let eval ~(env : Env.t) : t -> (t, Error.t) result = function
  | Var name -> Env.get name env |> Option.to_result ~none:(Error.Undefined_name name)
  | term -> Ok term
;;
