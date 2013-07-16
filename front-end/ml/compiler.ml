(*
 * =====================================================================================
 *
 *       Filename:  compiler.ml
 *
 *    Description:  Compile the AST into bytecode
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 *)

(* Requires the Ruse specification and Leroy's path definition in modules, Typechecker is needed by IS/SI *)
open  Ruse
open  Modules
open Typechecker

(*-----------------------------------------------------------------------------
 *  The bytecode compiler for Ruse ml
 *-----------------------------------------------------------------------------*)
module ByteCompiler : sig

    val parse : RuseML.term list -> string

end = 
struct
    open RuseML 


    (* Exceptions *) 
    exception Cannot_compile of string

    (* Tokens for terms  type to clean up the hashtable *)
    type term_token = 
          BConstant
        | BBoolean
        | BLongindent 
        | BFunction
        | BApply
        | BLet
        | BIS
        | BSI

    (* Tokens for types type to clean up the hashtable *)
    type type_token =
          BTArrow
        | BTInt
        | BTBool

    (* The byte code table for terms of Ruse ML *)
    let term_table = (Hashtbl.create 17 : (term_token,int) Hashtbl.t)
    let _ = List.iter (fun (str,tok) -> Hashtbl.add term_table str tok)
    [
        BConstant,      3;
        BBoolean,       4;
        BFunction,      10;
        BLongindent,    12;
        BApply,         13;
        BLet,           16;
        BIS,            26;
        BSI,            26
    ]

    (* The byte code table for types of Ruse ML *)
    let type_table = (Hashtbl.create 17 : (type_token,int) Hashtbl.t)
    let _ = List.iter (fun (str,tok) -> Hashtbl.add type_table str tok)
    [
        BTArrow,    1;
        BTInt,      2;
        BTBool,     3
    ]

    (* ported from Leroy *)
    let variable_names = ref ([] : (type_variable * string) list)


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    emit
     *  Description:    add the seperator to all integers 
     * =====================================================================================
     *)
     let emit num = 
        let its i = (Printf.sprintf "%d" i) in
        ((its num) ^ "\n") 


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    term_bc
     *  Description:    get the byte code of the term
     * =====================================================================================
     *)
     let term_bc token = 
        try
            Hashtbl.find term_table token
        with Not_found -> 
            raise (Cannot_compile "Unsupported term token")

    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    type_bc
     *  Description:    get the byte code of the type
     * =====================================================================================
     *)
     let type_bc token = 
        try
            Hashtbl.find type_table token
        with Not_found -> 
            raise (Cannot_compile "Unsupported token")


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    path_str
     *  Description:    convert path to string
     * =====================================================================================
     *)
     let rec path_str = function
          Pident id -> (Ident.name id) 
        | Pdot(root, field) -> ((path_str root) ^ "." ^ field)


    
    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    type_byte
     *  Description:    convert a type to a sequnce of bytes 
     * =====================================================================================
     *)
    let rec type_byte ty = match RuseMLTyping.typerepr ty with
        Var v ->
          let name =
            try
              List.assq v !variable_names
            with Not_found ->
              let n = List.length !variable_names + 1 in
              let s = String.make 1 (Char.chr(97 + n)) in
              variable_names := (v, s) :: !variable_names;
              s in
              (name ^ "\n")
        | Typeconstr(path, [t1;t2]) when path = RuseMLTyping.path_arrow ->
            ((emit (type_bc BTArrow)) ^ (type_byte t1) ^ (type_byte t2))  
        | Typeconstr(path, [t1;t2]) when path = RuseMLTyping.path_star ->
            (raise (Cannot_compile "Star not supported yet !"))
        | Typeconstr(path, []) ->
            ((path_str path) ^ "\n")
        | Typeconstr(path, [t]) ->
            (raise (Cannot_compile "List not supported yet !"))
        | Typeconstr(path, t1::tl) ->
            (raise (Cannot_compile "List not supported yet !"))
        | BooleanType -> (emit (type_bc BTBool)) 
        | IntType -> (emit (type_bc BTInt)) 
        | _ -> raise (Cannot_compile "Cannot convert type")


    (*
     * ===  FUNCTION  ======================================================================
     *         Name:    term_byte
     *  Description:    convert to a sequnce of bytes
     * =====================================================================================
     *)
    let rec term_byte = function
        Constant i -> ((emit (term_bc BConstant)) ^ (emit i))
        | Boolean b -> ( 
            let bti = function
               | true -> 1
               | false -> 0
            in
            ((emit (term_bc BBoolean)) ^ (emit (bti b))))
        | Longident p -> 
            ((emit (term_bc BLongindent))^(path_str p)^"\n") 
        | Function(id, body) -> 
            ((emit (term_bc BFunction)) ^ (term_byte (Longident (Pident id))) ^ (term_byte body))
        | Apply(t1, t2) -> 
            ((emit (term_bc BApply)) ^ (term_byte t1)^(term_byte t2))
        | Let(id, t1, t2) -> 
            ((emit (term_bc BLet)) ^ (term_byte (Longident (Pident id)))^(term_byte t1)^(term_byte t2))
        | IS(ty,t1) -> ((emit (term_bc BIS)) ^ (type_byte ty) ^ (term_byte t1))
        | SI(ty,t1) -> ((emit (term_bc BSI)) ^ (type_byte ty) ^ (term_byte t1))
        | _ -> (raise (Cannot_compile "Not supported by term_byte"))


    (* 
    * ===  FUNCTION  ======================================================================
    *         Name:    parse
    *  Description:    parse a list of expressions into a string of bytes
    * =====================================================================================
    *)
    let parse terms = 
        let lst = (List.map term_byte terms) in
        let bst = 
            [   "==@@==PARSER==@@==\n";     (* first header *)
                (emit 1) ;                  (* language == ML *)
                (emit (List.length lst))    (* number of expressions *)
            ]@
                lst @                       (* insecure code *)
            [   
                "==@@==PARSER==@@==\n";     (* second header *)
                "0\n"; 
                "0\n" 
            ] 
        in
        (String.concat "" bst)

end
