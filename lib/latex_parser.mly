%{
open Ast

let prime = Op "\xe2\x80\xb2"
%}

%token <string> NUM IDENT OP LARGEOP XARROW SPACE
%token <string * string> MATHFONT
%token FRAC SQRT STACKREL INFER
%token CARET UNDERSCORE
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token BAR PRIME
%token EOF

%start <Ast.math> main

%%

main:
  | m = math EOF { m }

math:
  | atoms = atom* { match atoms with [ x ] -> x | xs -> Row xs }

atom:
  | b = postfixed { b }
  | b = postfixed CARET sup = arg sub = sub_opt
      { match sub with None -> Sup (b, sup) | Some s -> SubSup (b, s, sup) }
  | b = postfixed UNDERSCORE sub = arg sup = sup_opt
      { match sup with None -> Sub (b, sub) | Some s -> SubSup (b, sub, s) }

sub_opt:
  | { None }
  | UNDERSCORE a = arg { Some a }

sup_opt:
  | { None }
  | CARET a = arg { Some a }

postfixed:
  | b = base { b }
  | p = postfixed PRIME { Sup (p, prime) }

base:
  | n = NUM { Num n }
  | i = IDENT { Ident i }
  | f = MATHFONT { Font (fst f, snd f) }
  | o = OP { Op o }
  | o = LARGEOP { LargeOp o }
  | BAR { Op "|" }
  | LBRACE m = math RBRACE { m }
  | LPAREN m = math RPAREN { Fenced ("(", m, ")") }
  | LBRACK m = math RBRACK { Fenced ("[", m, "]") }
  | FRAC a = arg b = arg { Frac (a, b) }
  | INFER a = arg b = arg { Infer (a, b) }
  | SQRT a = arg { Sqrt a }
  | w = SPACE { Space w }
  | g = XARROW a = arg { Xarrow (g, a) }
  | STACKREL over = arg base = arg { Over (base, over) }

arg:
  | b = base { b }
