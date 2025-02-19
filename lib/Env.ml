open Util

module type TOKENIZABLE_WITH_RELATION = sig
  include Ansifmt.Formatting.TOKENIZABLE

  val relation_token : Ansifmt.Formatting.Token.t
end

module type ENV = sig
  type key
  type term
  type t = private (key * term) list

  val empty : t
  val keys : t -> key list
  val terms : t -> term list
  val rows : t -> (key * term list) list
  val contains : key -> t -> bool
  val get : key -> t -> term option
  val get_all : key -> t -> term list
  val add : key -> term -> t -> t
  val remove : key -> t -> t option
  val remove_lenient : key -> t -> t
  val update : key -> term -> t -> t option
  val update_lenient : key -> term -> t -> t
  val deduplicate : conflict_handler:(term list -> term) -> t -> t
  val map_terms : 'term2. (term -> 'term2) -> t -> (key * 'term2) list
  val tokenize : t -> Ansifmt.Formatting.Tree.t
end

module Make (Key : Ansifmt.Formatting.TOKENIZABLE) (Term : TOKENIZABLE_WITH_RELATION) :
  ENV with type key = Key.t with type term = Term.t = struct
  type key = Key.t
  type term = Term.t
  type t = (key * term) list

  let empty : t = []
  let keys : t -> key list = fun env -> env |> List.map fst |> List.deduplicate
  let terms : t -> term list = List.map snd
  let contains : key -> t -> bool = List.mem_assoc
  let get : key -> t -> term option = List.assoc_opt

  let get_all : key -> t -> term list =
    fun key env -> env |> List.find_all (fun (key', _) -> compare key key' = 0) |> terms
  ;;

  let rows : t -> (key * term list) list =
    fun env -> List.map (fun key -> key, get_all key env) (keys env)
  ;;

  let add : key -> term -> t -> t = fun key term env -> (key, term) :: env

  let remove_lenient : key -> t -> t =
    let rec tailrec key env acc =
      match env with
      | [] -> acc
      | (key', term') :: rest ->
        tailrec key rest (if compare key key' = 0 then acc else add key' term' acc)
    in
    fun key env -> tailrec key env empty
  ;;

  let remove : key -> t -> t option =
    fun key env ->
    match contains key env with
    | false -> None
    | true -> Some (remove_lenient key env)
  ;;

  let update_lenient : key -> term -> t -> t =
    fun key term env -> add key term (remove_lenient key env)
  ;;

  let update : key -> term -> t -> t option =
    fun key term env -> remove key env |> Option.map (add key term)
  ;;

  let deduplicate : conflict_handler:(term list -> term) -> t -> t =
    fun ~conflict_handler env ->
    rows env |> List.map (fun (key, terms) -> key, conflict_handler terms)
  ;;

  let map_terms (type term2) (func : term -> term2) : t -> (key * term2) list = function
    | [] -> []
    | env -> List.map (fun (key, term) -> key, func term) env
  ;;

  open Ansifmt

  let tokenize_entry : key * term -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun (key, term) ->
      block
        [ Key.tokenize key
        ; simple [ Formatting.Token.space; Term.relation_token; Formatting.Token.space ]
        ; Term.tokenize term
        ; simple [ Formatting.Token.line_break ]
        ]
  ;;

  let header_tokens =
    Formatting.Tree.simple
      [ Formatting.Token_type.Documentation, "Environment:"; Formatting.Token.line_break ]
  ;;

  let tokenize : t -> Formatting.Tree.t =
    let open Formatting.Tree in
    function
    | [] -> simple [ Formatting.Token_type.Comment, "(empty)" ]
    | env -> block (header_tokens :: List.map tokenize_entry env)
  ;;
end
