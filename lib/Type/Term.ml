open Ansifmt.Prelude
open Util
include Term_base

module Name_indices = struct
  include
    Mapping.Make
      (Name)
      (struct
        include Index

        let relation_token = Ansifmt.Formatting.Token_type.Punctuation_strong, "->"
      end)
end

let name_indices = ref Name_indices.empty
let make_index = Index.generator ~start:0

let print_name_indices () =
  let open Ansifmt.Formatting in
  Tree.format
    (Tree.simple [ Token_type.Documentation, "de Bruijn indices:"; Token.line_break ])
  ^ format !name_indices ~using:(module Name_indices)
  |> Printf.printf "%s\n"
;;

let instantiate_variable (name : Name.t) : Index.t =
  let index = make_index () in
  name_indices := Name_indices.add name index !name_indices;
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

  let tokenize_base = tokenize

  open Ansifmt

  let tokenize (env : t) : Formatting.Tree.t =
    let open Formatting in
    Tree.block
      [ Tree.simple [ Token_type.Documentation, "Environment:"; Token.line_break ]
      ; tokenize_base env
      ]
  ;;
end

let eval ~(env : Env.t) : t -> (t, Error.t) result = function
  | Var index ->
    Env.get index env
    |> Option.to_result
         ~none:
           (Error.Undefined_name (Name_indices.flip !name_indices |> List.assoc index))
  | term -> Ok term
;;
