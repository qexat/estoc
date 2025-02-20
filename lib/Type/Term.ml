open Ansifmt.Prelude
open Util
include Term_base

let indices_to_names : Name.t list ref = ref []
let get_index () = List.length !indices_to_names

let print_indices_to_names () =
  !indices_to_names
  |> List.mapi (fun index name ->
    Printf.sprintf
      "%s -> %s"
      (format index ~using:(module Index))
      (format name ~using:(module Name)))
  |> String.concat "\n"
  |> Printf.printf "%s\n"
;;

let instantiate_variable (name : Name.t) : Index.t =
  let index = get_index () in
  indices_to_names := List.push name !indices_to_names;
  index
;;

let rec of_value_term : Value.Term.t -> t = function
  | Forall (name, body) -> Forall (instantiate_variable name, of_value_term body)
  | Int _ -> Int
  | Unit -> Unit
  | Var name -> Var (instantiate_variable name)
;;

module Env = struct
  include Mapping.Make (Index) (Term_base)
end

let eval ~(env : Env.t) : t -> (t, Error.t) result = function
  | Var index ->
    Env.get index env
    |> Option.to_result ~none:(Error.Undefined_name (List.nth !indices_to_names index))
  | term -> Ok term
;;
