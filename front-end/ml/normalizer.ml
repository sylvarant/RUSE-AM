(*
 * =====================================================================================
 *
 *       Filename:  normalizer.ml
 *
 *    Description:  Normalize to ANF
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 *)

 open Ruse
 open Modules


(*-----------------------------------------------------------------------------
 *  The normalizer for Ruse ml
 *-----------------------------------------------------------------------------*)
module Gensym : sig 

    val next : unit -> string 

end =
struct

    (* Counter *)
    let counter = ref 0


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    rand_chr
     *  Description:    produce a random character
     * =====================================================================================
     *)
    let rand_chr = fun () -> (Char.chr (97 + (Random.int 26)))        
    

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    rand_str
     *  Description:    produce a random string of random length
     * =====================================================================================
     *)
    let rand_str = 
        let rec produce = function  
            | 0 -> ""
            | x -> (Char.escaped (rand_chr())) ^ (produce (x - 1))
        in
        fun ()  -> (produce (Random.int 10))

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    next
     *  Description:    produce an new variable
     * =====================================================================================
     *)
     let next = fun () -> incr counter ; 
        let newstr = rand_str() in newstr ^ (string_of_int !counter)

end



(*-----------------------------------------------------------------------------
 *  The normalizer for Ruse ml
 *-----------------------------------------------------------------------------*)
module ANFNormalizer : sig

    val normalize : RuseML.term list -> RuseML.term list

end =
struct
    open RuseML

    (* Exceptions *)
    exception Cannot_normalize of string

    (* The initial normalization context *)    
    let init_context = (fun x -> x) 

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    is_atomic
     *  Description:    all expressions not be normalized
     * =====================================================================================
     *)
    let is_atomic = function
        | Constant _
        | Boolean _  
        | Longident _ -> true
        | _ -> false

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    is_enclosed
     *  Description:    all expressions not to be lifted
     * =====================================================================================
     *)
    let is_enclosed = function
        | Function _ 
        | Prim _
        | IS _
        | SI _ -> true
        | x when (is_atomic x) -> true
        | _ -> false 

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    lift
     *  Description:    Lift a subterm if necessary
     * =====================================================================================
     *)
    let rec lift expr cont = 
        (normalize_term expr (fun atomic -> 
            if (is_enclosed atomic) 
            then (cont atomic)
            else 
                let newsym = Ident.create (Gensym.next()) in
                let newbody = (cont (Longident (Pident newsym))) in

                Let (newsym,atomic,newbody)))

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    liftls
     *  Description:    Lift a list of subterms if necessary
     * =====================================================================================
     *)
    and lift_list exprls cont = match exprls with
        | [] -> (cont [])
        | l::ls -> (lift l (fun x -> (lift_list ls (fun y -> (cont (x :: y))))))

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    normalize_term
     *  Description:    normalize_term a term by either building up a context or applying the 
     *                  term to the context
     * =====================================================================================
     *)
    and normalize_term expr cont = match expr with
        | Function (var,body) -> 
            let newbody = (normalize_term body init_context) in (cont (Function(var,newbody))) 
        | IS (ty,term) -> 
            let newterm = (normalize_term term init_context) in (cont (IS(ty,newterm))) 
        | SI (ty,term) -> 
            let newterm = (normalize_term term init_context) in (cont (SI(ty,newterm)))
        | Let (var,value,body) -> 
            let newbody = (normalize_term body cont) in Let(var,value,newbody)  
        | If (t1,t2,t3) -> 
            let newt2 = (normalize_term t2 init_context) 
            and newt3 = (normalize_term t3 init_context) in
            (lift t1 (fun x -> (cont (If (x,newt2,newt3)))))
        | Prim (op,ls) -> 
            (lift_list ls (fun x -> (cont (Prim(op,x)))))
        | Apply (proc,arg) -> 
            (lift proc (fun x -> lift arg (fun y -> (cont (Apply(x,y)))))) 
        | x when (is_atomic x) -> (cont expr)

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    normalize
     *  Description:    normalize a list of terms 
     * =====================================================================================
     *)
    let normalize terms = List.map (fun x -> normalize_term x init_context) terms
    
end

