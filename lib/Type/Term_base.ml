open Util

type t =
  | Forall of (Index.t * t)
  | Int
  | Unit
  | Var of Index.t

open Ansifmt

let is_atomic : t -> bool = function
  | Int | Unit | Var _ -> true
  | _ -> false
;;

let rec tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | Forall (index, body) ->
    block
      [ simple [ Formatting.Token_type.Keyword, "forall"; Formatting.Token.space ]
      ; Index.tokenize index
      ; simple [ Formatting.Token.comma; Formatting.Token.space ]
      ; parenthesize_if (Fun.negate is_atomic) tokenize body
      ]
  | Int -> simple [ Formatting.Token_type.Type, "int" ]
  | Unit -> simple [ Formatting.Token_type.Type, "unit" ]
  | Var index -> Index.tokenize index
;;

let relation_token = Formatting.Token.colon
