open Util

type t =
  | Forall of Name.t * t
  | Int of int
  | Unit
  | Var of Name.t

open Ansifmt

let is_atomic : t -> bool = function
  | Int _ | Unit | Var _ -> true
  | _ -> false
;;

let rec tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | Forall (name, value) ->
    block
      [ simple
          [ Formatting.Token_type.Keyword, "forall"
          ; Formatting.Token.space
          ; ( Formatting.Token_type.Parameter
            , Prelude.format name ~using:(module Name) )
          ; Formatting.Token.comma
          ; Formatting.Token.space
          ]
      ; parenthesize_if (Fun.negate is_atomic) tokenize value
      ]
  | Int value ->
    simple
      [ ( Formatting.Token_type.Literal_constant
        , Int.to_string value )
      ]
  | Unit ->
    simple [ Formatting.Token_type.Literal_constant, "()" ]
  | Var name -> Name.tokenize name
;;

let relation_token = Formatting.Token_type.Operator_stmt, "="
