let ( let* ) = Result.bind

module Name = struct
  type t = string

  let repr : t -> string = fun name -> name

  open Ansifmt

  let tokenize : t -> Formatting.Tree.t =
    let open Formatting.Tree in
    fun name -> simple [ Formatting.Token_type.Identifier, name ]
  ;;
end

module Index = struct
  type t = int

  let repr : t -> string = Printf.sprintf "?%d"

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

  let rec push : type item_t. item_t -> item_t t -> item_t t =
    fun item -> function
    | [] -> item :: []
    | first :: rest -> first :: push item rest
  ;;
end
