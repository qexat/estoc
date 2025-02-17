open Ansifmt.Prelude
open Util

type t =
  | Illegal_application (* of what? *)
  | Undefined_name of Name.t

let repr : t -> string = function
  | Illegal_application ->
    "illegal application of non-functional expression"
  | Undefined_name name ->
    "name "
    ^ format name ~using:(module Name)
    ^ " is not defined"
;;
