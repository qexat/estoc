open Util

module type TOKENIZABLE_WITH_RELATION = sig
  include Ansifmt.Formatting.TOKENIZABLE

  val relation_token : Ansifmt.Formatting.Token.t
end

module type MAPPING = sig
  type key
  type value
  type t = private (key * value) list

  val empty : t
  val keys : t -> key list
  val values : t -> value list
  val rows : t -> (key * value list) list
  val contains : key -> t -> bool
  val get : key -> t -> value option
  val get_all : key -> t -> value list
  val add : key -> value -> t -> t
  val remove : key -> t -> t option
  val remove_lenient : key -> t -> t
  val update : key -> value -> t -> t option
  val update_lenient : key -> value -> t -> t
  val deduplicate : conflict_handler:(value list -> value) -> t -> t
  val flip : t -> (value * key) list
  val map_values : 'value2. (value -> 'value2) -> t -> (key * 'value2) list
  val iter : (key -> value -> unit) -> t -> unit
  val tokenize : t -> Ansifmt.Formatting.Tree.t
end

module Make (Key : Ansifmt.Formatting.TOKENIZABLE) (Value : TOKENIZABLE_WITH_RELATION) :
  MAPPING with type key = Key.t with type value = Value.t = struct
  type key = Key.t
  type value = Value.t
  type t = (key * value) list

  let empty : t = []
  let keys : t -> key list = fun env -> env |> List.map fst |> List.deduplicate
  let values : t -> value list = List.map snd
  let contains : key -> t -> bool = List.mem_assoc
  let get : key -> t -> value option = List.assoc_opt

  let get_all : key -> t -> value list =
    fun key env -> env |> List.find_all (fun (key', _) -> compare key key' = 0) |> values
  ;;

  let rows : t -> (key * value list) list =
    fun env -> List.map (fun key -> key, get_all key env) (keys env)
  ;;

  let add : key -> value -> t -> t = fun key value env -> (key, value) :: env

  let remove_lenient : key -> t -> t =
    let rec tailrec key env acc =
      match env with
      | [] -> acc
      | (key', value') :: rest ->
        tailrec key rest (if compare key key' = 0 then acc else add key' value' acc)
    in
    fun key env -> tailrec key env empty
  ;;

  let remove : key -> t -> t option =
    fun key env ->
    match contains key env with
    | false -> None
    | true -> Some (remove_lenient key env)
  ;;

  let update_lenient : key -> value -> t -> t =
    fun key value env -> add key value (remove_lenient key env)
  ;;

  let update : key -> value -> t -> t option =
    fun key value env -> remove key env |> Option.map (add key value)
  ;;

  let deduplicate : conflict_handler:(value list -> value) -> t -> t =
    fun ~conflict_handler env ->
    rows env |> List.map (fun (key, values) -> key, conflict_handler values)
  ;;

  let flip : t -> (value * key) list = function
    | [] -> []
    | env -> List.combine (values env) (keys env)
  ;;

  let map_values (type value2) (func : value -> value2) : t -> (key * value2) list
    = function
    | [] -> []
    | env -> List.map (fun (key, value) -> key, func value) env
  ;;

  let rec iter (func : key -> value -> unit) : t -> unit = function
    | [] -> ()
    | (key, value) :: rest ->
      func key value;
      iter func rest
  ;;

  open Ansifmt

  let tokenize_row : key * value list -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun (key, values) ->
      block
        [ Key.tokenize key
        ; simple [ Formatting.Token.space; Value.relation_token; Formatting.Token.space ]
        ; block
            (List.intersperse
               (simple [ Formatting.Token.comma; Formatting.Token.space ])
               (List.map Value.tokenize values))
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
    | env ->
      rows (List.rev env)
      |> List.map tokenize_row
      |> List.intersperse (simple [ Formatting.Token.line_break ])
      |> List.cons header_tokens
      |> block
  ;;
end
