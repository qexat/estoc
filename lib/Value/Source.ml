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

open Ansifmt

let rec tokenize : t -> Formatting.Tree.t =
  let open Formatting.Tree in
  function
  | [] -> simple [ Formatting.Token_type.Comment, "(empty)" ]
  | last :: [] -> Stmt.tokenize last
  | first :: rest ->
    block
      [ Stmt.tokenize first
      ; simple [ Formatting.Token.line_break ]
      ; tokenize rest
      ]
;;
