/*
 * =====================================================================================
 *
 *       Filename:  parser.mly
 *
 *    Description:  Parser for Ruse ML based on Leroy's parser for miniML
 *
 *         Author:  Adriaan Larmuseau, ajhl
 *        Company:  Distrinet, Kuleuven
 *
 * =====================================================================================
 */

%{
open Modules
open Ruse
open Typechecker

let variables = ref ([] : (string * RuseML.type_variable) list)

let reset_type_variables () =
  variables := []

let find_type_variable name =
  try
    List.assoc name !variables
  with Not_found ->
    let v = RuseMLTyping.newvar() in
    variables := (name, v) :: !variables;
    v

(* TODO fix *)
let binop op arg1 arg2 =
  RuseML.Apply(RuseML.Apply(RuseML.Longident(Pident(Ident.create op)), arg1), arg2)
let ternop op arg1 arg2 arg3 =
  RuseML.Apply(RuseML.Apply(RuseML.Apply(RuseML.Longident(Pident(Ident.create op)), arg1), arg2), arg3)

%}

%token <string> IDENT
%token <int> INT

%token TRUE
%token FALSE
%token IS
%token SI
%token ARROW
%token COLON
%token COMMA
%token DOT
%token ELSE
%token END
%token EOF
%token EQUAL
%token EQUALEQUAL
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATEREQUAL
%token IF
%token IN
%token LESS
%token LESSEQUAL
%token LESSGREATER
%token LET
%token LIDENT
%token LPAREN
%token MINUS
%token MODULE
%token PLUS
%token QUOTE
%token RPAREN
%token SEMICOLON
%token SEMISEMI
%token SIG
%token SLASH
%token STAR
%token STRUCT
%token THEN
%token TYPE
%token VALUE

%right ARROW
%right COMMA
%right LESSGREATER LESS LESSEQUAL GREATER GREATEREQUAL
%right PLUS MINUS
%right STAR SLASH

/* Adriaan */
%start adriaan 
%type <Ruse.RuseML.term list> adriaan 

/*% IGNORED
%start implementation
%type <MiniML.RuseMLMod.mod_term> implementation
*/

/*% IGNORED
start phrase
%type <MiniML.RuseMLMod.definition> phrase
%start code
%type <MiniML.RuseMLMod.Core.term> code */
%%

/* Paths */

path:
    IDENT           { Pident(Ident.create $1) }
  | path DOT IDENT  { Pdot($1, $3) }
;

/* Value expressions for the core language */

valexpr:
    valexpr1                          { $1 }
  | valexpr COMMA valexpr             { binop "," $1 $3 }
  | valexpr PLUS valexpr              { binop "+" $1 $3 }
  | valexpr MINUS valexpr             { binop "-" $1 $3 }
  | valexpr STAR valexpr              { binop "*" $1 $3 }
  | valexpr SLASH valexpr             { binop "/" $1 $3 }
  | valexpr EQUALEQUAL valexpr        { binop "==" $1 $3 }
  | valexpr LESSGREATER valexpr       { binop "<>" $1 $3 }
  | valexpr LESS valexpr              { binop "<" $1 $3 }
  | valexpr LESSEQUAL valexpr         { binop "<=" $1 $3 }
  | valexpr GREATER valexpr           { binop ">" $1 $3 }
  | valexpr GREATEREQUAL valexpr      { binop ">=" $1 $3 }
  | FUNCTION IDENT ARROW valexpr      { RuseML.Function(Ident.create $2, $4) }
  | LET IDENT valbind IN valexpr      { RuseML.Let(Ident.create $2, $3, $5) }
  | IF valexpr THEN valexpr ELSE valexpr { ternop "conditional" $2 $4 $6 }
  | IS simpletype COLON valexpr      { RuseML.IS($2,$4)}
  | SI simpletype COLON valexpr      { RuseML.SI($2,$4)}
;
valexpr1:
    valexpr0 { $1 }
  | valexpr1 valexpr0 { RuseML.Apply($1, $2) }
;
valexpr0:
    path { RuseML.Longident($1) }
  | INT  { RuseML.Constant $1 }
  | TRUE { RuseML.Boolean true }
  | FALSE { RuseML.Boolean false }
  | LPAREN valexpr RPAREN { $2 }
;


valbind:
    EQUAL valexpr     { $2 }
  | IDENT valbind     { RuseML.Function(Ident.create $1, $2) }
;

/* Type expressions for the core language */

simpletype:
    QUOTE IDENT             { RuseML.Var(find_type_variable $2) }
  | simpletype ARROW simpletype { RuseML.Typeconstr(RuseMLTyping.path_arrow, [$1; $3]) }
  | simpletype STAR simpletype  { RuseML.Typeconstr(RuseMLTyping.path_star, [$1; $3]) }
  | path                    { RuseML.Typeconstr($1, []) }
  | simpletype path         { RuseML.Typeconstr($2, [$1]) }
  | LPAREN simpletypelist RPAREN path { RuseML.Typeconstr($4, List.rev $2) }
;
simpletypelist:
    simpletype { [$1] }
  | simpletypelist COMMA simpletype { $3::$1 }
;

valuedecl:
    colon_begin_scheme simpletype
            { reset_type_variables(); RuseMLTyping.end_def(); RuseMLTyping.generalize $2 }
;
colon_begin_scheme: /* Hack to perform side effects before reading the type */
    COLON   { RuseMLTyping.begin_def(); reset_type_variables() }
;

/* Type definitions and declarations */

typedecl:
    typeparams IDENT        { ($2, {RuseML.arity = List.length $1}) }
;
typedef:
    typeparams IDENT EQUAL simpletype
      { reset_type_variables();
        ($2, {RuseML.arity = List.length $1}, {RuseML.params = $1; RuseML.defbody = $4}) }
;
typeparams:
    /* nothing */               { [] }
  | typeparam                   { [$1] }
  | LPAREN typeparamlist RPAREN { List.rev $2 }
;
typeparamlist:
    typeparam                       { [$1] }
  | typeparamlist COMMA typeparam   { $3 :: $1 }
;
typeparam:
    QUOTE IDENT { find_type_variable $2 }
;
typeinfo:
    typedef   { let (id, kind, def) = $1 in
                (id, {RuseMLMod.kind = kind; RuseMLMod.manifest = Some def})}
  | typedecl  { let (id, kind) = $1 in
                (id, {RuseMLMod.kind = kind; RuseMLMod.manifest = None}) }
;

/* Value expressions for the module language */

modulexpr:
    path                              { RuseMLMod.Longident $1 }
  | STRUCT structure END              { RuseMLMod.Structure(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN modulexpr
                                      { RuseMLMod.Functor(Ident.create $3, $5, $7) }
  | modulexpr LPAREN modulexpr RPAREN { RuseMLMod.Apply($1, $3) }
  | LPAREN modulexpr RPAREN           { $2 }
  | modulexpr COLON moduletype        { RuseMLMod.Constraint($1, $3) }
/*  | valexpr                           { RuseMLMod.Expression $1 }  Adriaan */
;
structure:
    /*nothing*/                       { [] }
  | structure structure_item opt_semi { $2 :: $1 }
;
structure_item:
    VALUE IDENT valbind           { RuseMLMod.Value_str(Ident.create $2, $3) }
  | TYPE typedef                  { let (id, kind, def) = $2 in
                                    RuseMLMod.Type_str(Ident.create id, kind, def) }
  | MODULE IDENT COLON moduletype EQUAL modulexpr
                     { RuseMLMod.Module_str(Ident.create $2, RuseMLMod.Constraint($6, $4)) }
  | MODULE IDENT EQUAL modulexpr   { RuseMLMod.Module_str(Ident.create $2, $4) }
;
opt_semi:
    /* nothing */ { () }
  | SEMICOLON { () }
;

/* Type expressions for the module language */

moduletype:
    SIG signature END               { RuseMLMod.Signature(List.rev $2) }
  | FUNCTOR LPAREN IDENT COLON moduletype RPAREN moduletype
                                    { RuseMLMod.Functor_type(Ident.create $3, $5, $7) }
  | LPAREN moduletype RPAREN        { $2 }
;
signature:
    /*nothing*/                       { [] }
  | signature signature_item opt_semi { $2 :: $1 }
;
signature_item:
    VALUE IDENT valuedecl             { RuseMLMod.Value_sig(Ident.create $2, $3) }
  | TYPE typeinfo    { let (id, def) = $2 in RuseMLMod.Type_sig(Ident.create id, def) }
  | MODULE IDENT COLON moduletype     { RuseMLMod.Module_sig(Ident.create $2, $4) }
;

/* Toplevel entry point */

implementation:
   | modulexpr EOF                     { $1 }

adriaan:
    | valexpr EOF { [$1] }
    | valexpr SEMISEMI adriaan { $1 :: $3 } 
    | EOF { [] }


