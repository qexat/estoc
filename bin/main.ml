open Ansifmt.Prelude
open Estoc
open Util

let v_env : Value.Term.Env.t = []
let t_env : Type.Term.Env.t = []

(* sources *)
let v_source : Value.Source.t =
  [ Value.Stmt.Let
      ("a", Term (Value.Term.Forall ("x", Value.Term.Var "x")))
  ; Value.Stmt.Let ("b", Term (Int 9))
  ]
;;

let t_source = Type.Source.of_value_source v_source

let t_eval () : (unit, Type.Error.t) result =
  let* t_env = Type.Source.eval ~env:t_env t_source in
  print_formatted
    ~line_end:""
    t_env
    ~using:(module Type.Term.Env);
  Ok ()
;;

let v_eval () : (unit, Value.Error.t) result =
  let* v_env = Value.Source.eval ~env:v_env v_source in
  print_formatted
    ~line_end:""
    v_env
    ~using:(module Value.Term.Env);
  Ok ()
;;

let main () : (unit, Any_error.t) result =
  print_formatted v_source ~using:(module Value.Source);
  (* comment the line below to disable type-checking *)
  let* () = t_eval () |> Result.map_error Any_error.of_t_error in
  let* () = v_eval () |> Result.map_error Any_error.of_v_error in
  (* TODO: remove ; this is for debugging only *)
  Type.Term.print_indices_to_names ();
  Ok ()
;;

let () =
  match main () with
  | Ok () -> ()
  | Error error -> Printf.eprintf "%s\n" (Any_error.repr error)
;;
