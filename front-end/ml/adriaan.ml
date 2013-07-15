(*
 * File :: adriaan.ml
 *)
open Modules
open MiniML
module Core = ML
open ML

    let rec printexpr :ML.term -> unit  = function
        Constant i -> (print_int i)
            | Longident z -> (MLPrint.print_path z) 
            | Function(id, body) -> ((printexpr (Longident (Pident id)));(print_string "->");(printexpr body))
            | Apply(t1, t2) -> ((print_string "app: "); (printexpr t1);(print_string "->");(printexpr t2))
            | Let(id, t1, t2) -> ((printexpr (Longident (Pident id)));(print_string ",");(printexpr t1);(printexpr t2))
            | _ -> (raise (Failure "Not supported by printexpr"))

        let rec parse = function
            | [] -> ((print_string "DONE"); Format.print_newline())
            | l :: ls -> ((printexpr l); Format.print_newline(); (parse ls))
