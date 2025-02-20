open Ansifmt.Prelude
open Estoc
open Util

let value_env = ref Value.Term.Env.empty
let type_env = ref Type.Term.Env.empty

(* sources *)
let v_source : Value.Source.t =
  let open Value.Base in
  [ let' "a" (forall "x" (fun x -> x)); let' "b" (int 9) ]
;;

let t_source = Type.Source.of_value_source v_source

module Core = struct
  let type_exec () =
    Type.Source.run ~env:type_env t_source |> Result.map_error Any_error.of_t_error
  ;;

  let value_exec () =
    Value.Source.run ~env:value_env v_source |> Result.map_error Any_error.of_v_error
  ;;
end

let main () : (unit, Any_error.t) result =
  print_formatted v_source ~using:(module Value.Source);
  (* comment the line below to disable type-checking *)
  let* () = Core.type_exec () in
  (* comment the line below to disable execution *)
  let* () = Core.value_exec () in
  (* TODO: remove ; this is for debugging only *)
  Type.Term.print_name_indices ();
  Ok ()
;;

let () =
  match main () with
  | Ok () -> ()
  | Error error -> Printf.eprintf "%s\n" (Any_error.repr error)
;;
