module type TOKENIZABLE_WITH_RELATION = sig
  include Ansifmt.Formatting.TOKENIZABLE

  val relation_token : Ansifmt.Formatting.Token.t
end

module type ENV = sig
  type key
  type term
  type t = (key * term) list

  val empty : t
  val get : key -> t -> term option
  val update_or_add : key -> term -> t -> t
  val map : 'term2. (term -> 'term2) -> t -> (key * 'term2) list
  val tokenize : t -> Ansifmt.Formatting.Tree.t
end

module Make
    (Key : Ansifmt.Formatting.TOKENIZABLE)
    (Term : TOKENIZABLE_WITH_RELATION) :
  ENV with type key = Key.t with type term = Term.t = struct
  type key = Key.t
  type term = Term.t
  type t = (key * term) list

  let empty : t = []
  let get : key -> t -> term option = List.assoc_opt

  let update_or_add : key -> term -> t -> t =
    fun key term env ->
    match get key env with
    | None -> (key, term) :: env
    | Some _ -> (key, term) :: List.remove_assoc key env
  ;;

  let map (type term2) (func : term -> term2)
    : t -> (key * term2) list
    = function
    | [] -> []
    | env -> List.map (fun (key, term) -> key, func term) env
  ;;

  open Ansifmt

  let tokenize_entry : key * term -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun (key, term) ->
      block
        [ Key.tokenize key
        ; simple
            [ Formatting.Token.space
            ; Term.relation_token
            ; Formatting.Token.space
            ]
        ; Term.tokenize term
        ; simple [ Formatting.Token.line_break ]
        ]
  ;;

  let header_tokens =
    Formatting.Tree.simple
      [ Formatting.Token_type.Documentation, "Environment:"
      ; Formatting.Token.line_break
      ]
  ;;

  let tokenize : t -> Formatting.Tree.t =
    let open Formatting.Tree in
    function
    | [] -> simple [ Formatting.Token_type.Comment, "(empty)" ]
    | env -> block (header_tokens :: List.map tokenize_entry env)
  ;;
end
