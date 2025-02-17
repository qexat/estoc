type t =
  | Static_error of Type.Error.t
  | Runtime_error of Value.Error.t

let repr : t -> string = function
  | Static_error error -> "type error: " ^ Type.Error.repr error
  | Runtime_error error ->
    "runtime error: " ^ Value.Error.repr error
;;

let of_t_error : Type.Error.t -> t =
  fun error -> Static_error error
;;

let of_v_error : Value.Error.t -> t =
  fun error -> Runtime_error error
;;
