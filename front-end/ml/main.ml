(*
 * =====================================================================================
 *
 *       Filename:  main.ml
 *
 *    Description:  Compile the AST into bytecode
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 *)

(* Requires Module system, Ruse Ast, Ruse Typechecker, Ruse scoping and Compiler *)
open Modules
open Ruse
open Typechecker
open Scoping
open Compiler


let init_scope = ref Scope.empty
let init_env = ref RuseMLEnv.empty

let enter_type id decl =
  init_scope := Scope.enter_type id !init_scope;
  init_env := RuseMLEnv.add_type id decl !init_env

let enter_val name ty =
  let id = Ident.create name in
  init_scope := Scope.enter_value id !init_scope;
  init_env := RuseMLEnv.add_value id ty !init_env

let _ =
  let ident_bool = Ident.create "bool" in
  let path_bool = Pident ident_bool in
  let bool_type = RuseML.Typeconstr(path_bool, []) in
  enter_type RuseMLTyping.ident_arrow {RuseMLMod.kind = {RuseML.arity = 2}; RuseMLMod.manifest = None};
  enter_type RuseMLTyping.ident_star {RuseMLMod.kind = {RuseML.arity = 2}; RuseMLMod.manifest = None};
  enter_type RuseMLTyping.ident_int {RuseMLMod.kind = {RuseML.arity = 0}; RuseMLMod.manifest = None};
  enter_type RuseMLTyping.ident_bool {RuseMLMod.kind = {RuseML.arity = 0}; RuseMLMod.manifest = None};
  List.iter
    (fun name ->
        enter_val name
          { RuseML.quantif = [];
            RuseML.body = RuseMLTyping.arrow_type RuseMLTyping.int_type 
                (RuseMLTyping.arrow_type RuseMLTyping.int_type RuseMLTyping.bool_type)})
    ["+"; "-"; "*"; "/"; "=="; "<>"; "<"; "<="; ">"; ">="];
  let alpha = RuseMLTyping.newvar() and beta = RuseMLTyping.newvar() in
  let talpha = RuseML.Var alpha and tbeta = RuseML.Var beta in
  enter_val ","
    { RuseML.quantif = [alpha;beta];
      RuseML.body = RuseMLTyping.arrow_type talpha (RuseMLTyping.arrow_type tbeta
                  (RuseML.Typeconstr(RuseMLTyping.path_star, [talpha; tbeta]))) };
  enter_val "fst"
    { RuseML.quantif = [alpha;beta];
      RuseML.body = RuseMLTyping.arrow_type
                  (RuseML.Typeconstr(RuseMLTyping.path_star, [talpha; tbeta])) talpha };
  enter_val "snd"
    { RuseML.quantif = [alpha;beta];
      RuseML.body = RuseMLTyping.arrow_type
                  (RuseML.Typeconstr(RuseMLTyping.path_star, [talpha; tbeta])) tbeta };
  enter_val "conditional"
    { RuseML.quantif = [alpha];
      RuseML.body =RuseMLTyping.arrow_type bool_type
                          (RuseMLTyping.arrow_type talpha (RuseMLTyping.arrow_type talpha talpha)) }


(*
 * ===  FUNCTION  ======================================================================
 *         Name:    main
 *  Description:    feeds the lexer from stdin and then parses and compiles it
 *          TODO    compile modules
 * =====================================================================================
 *)
let main() =
  let lexbuf = Lexing.from_channel stdin in
  try

(*  Modular modules TODO
    let prog = MiniMLparser.implementation MiniMLlexer.token lexbuf in
    let scoped_prog = RuseMLModScoping.scope_module !init_scope prog in
    let mty = RuseMLModTyping.type_module !init_env scoped_prog in
    MLPrint.print_modtype mty; *)

    let prog = Parser.adriaan Lexer.token lexbuf in
    (print_string (ByteCompiler.parse prog));
    Format.print_newline();
    exit 0
  with
    Error s ->
      prerr_string "Error: "; prerr_string s; prerr_newline(); exit 2
  | Parsing.Parse_error ->
      prerr_string "Syntax error at char ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 2
  | Lexer.Lexical_error msg ->
      prerr_string "Lexical error: "; prerr_string msg;
      prerr_string ", around character ";
      prerr_int (Lexing.lexeme_start lexbuf);
      prerr_newline();
      exit 2

let _ = main()
