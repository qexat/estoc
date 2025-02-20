open Util
include Term_base

module Env = struct
  include Mapping.Make (Name) (Term_base)

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
  | Var name -> Env.get name env |> Option.to_result ~none:(Error.Undefined_name name)
  | term -> Ok term
;;
