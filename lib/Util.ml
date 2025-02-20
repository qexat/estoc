let ( let* ) = Result.bind

module Name = struct
  type t = string

  open Ansifmt

  let tokenize : t -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun name -> simple [ Formatting.Token_type.Identifier, name ]
  ;;
end

module Index = struct
  type t = int

  let generator : start:t -> unit -> t =
    fun ~start ->
    let current = ref start in
    fun () ->
      let value = !current in
      incr current;
      value
  ;;

  open Ansifmt

  let tokenize : t -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun index ->
      simple
        [ Formatting.Token_type.Variable_type, "?"
        ; Formatting.Token_type.Variable_type, Int.to_string index
        ]
  ;;
end

module List = struct
  include List

  let push : type item_t. item_t -> item_t t -> item_t t =
    let rec tailrec item list acc =
      match list with
      | [] -> rev (item :: acc)
      | first :: rest -> (tailrec [@tailrec]) item rest (first :: acc)
    in
    fun item list -> tailrec item list []
  ;;

  let deduplicate : type item_t. item_t t -> item_t t =
    let rec tailrec list acc =
      match list with
      | [] -> rev acc
      | first :: rest ->
        (tailrec [@tailrec]) rest (if mem first acc then acc else first :: acc)
    in
    fun list -> tailrec list []
  ;;

  let intersperse : type item_t. item_t -> item_t t -> item_t t =
    let rec tailrec item list acc =
      match list with
      | [] -> List.rev acc
      | last :: [] -> tailrec item [] (last :: acc)
      | first :: rest -> tailrec item rest (item :: first :: acc)
    in
    fun item list -> tailrec item list []
  ;;
end
