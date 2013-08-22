(*
 * =====================================================================================
 *
 *       Filename:  prettyprinter.ml
 *
 *    Description:  Pretty print the types
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 *)

open Ruse
open Typechecker

module PrettyPrinter : sig

    val print_type : RuseML.simple_type -> string

end =
struct
    
    open RuseML

    (* Exceptions *) 
    exception Cannot_print of string
    
    (* print a type *)
    let print_type ty = raise Cannot_print "Function not implemented"

end

