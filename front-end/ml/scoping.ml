(*
 * =====================================================================================
 *
 *       Filename:  scoping.ml
 *
 *    Description:  Scoping for Ruse Ml
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 *)

 (* Requires RUSE  and Leroy Modular Modules *)
open Ruse
open Modules


(*-----------------------------------------------------------------------------
 *  Ruse ML scoping
 *-----------------------------------------------------------------------------*)
module RuseMLScoping =
struct

    module Core = RuseML
    open RuseML (* TODO *)

    (* 
     * ===  FUNCTION  ======================================================================
     *         Name:    scope term
     *  Description:    Allow the modular module system scoper to figure out the id's 
     * =====================================================================================
     *)
    let rec scope_term sc = function
        Constant _  as el -> el
        | Boolean _ as el -> el
        | Longident path -> Longident(Scope.value_path path sc)
        | Function(id, body) ->
            Function(id, scope_term (Scope.enter_value id sc) body)
        | Apply(t1, t2) -> Apply(scope_term sc t1, scope_term sc t2)
        | Let(id, t1, t2) ->
            Let(id, scope_term sc t1, scope_term (Scope.enter_value id sc) t2)
        | If (t1,t2,t3) -> If(scope_term sc t1,scope_term sc t2,scope_term sc t3)
        | Prim (c,ls) -> Prim(c,(List.map  (fun x -> (scope_term sc x)) ls))
        | SI(ty,t1) -> SI (ty,(scope_term sc t1))
        | IS(ty,t1) -> IS (ty,(scope_term sc t1))

    let rec scope_simple_type sc = function
        Var v -> Var v
      | Typeconstr(path, args) ->
          Typeconstr(Scope.type_path path sc,
                     List.map (scope_simple_type sc) args)

    let scope_valtype sc vty =
      { quantif = vty.quantif; body = scope_simple_type sc vty.body }

    let scope_deftype sc def =
      { params = def.params; defbody = scope_simple_type sc def.defbody }

    let scope_kind sc kind = kind

end

(*-----------------------------------------------------------------------------
 *  Apply the Modular Module system to the typing
 *-----------------------------------------------------------------------------*)
module RuseMLModScoping = ModScoping(RuseMLMod)(RuseMLScoping)


