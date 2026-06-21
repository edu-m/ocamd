%{
open Ast

let prime = Op "\xe2\x80\xb2"

let with_primes b ps = List.fold_left (fun acc _ -> Sup (acc, prime)) b ps
%}

%token <string> NUM IDENT OP LARGEOP XARROW ACCENT SPACE MTEXT
%token <string * string> MATHFONT
%token FRAC SQRT STACKREL INFER OVERLINE MATHREL
%token BEGINCASES ENDCASES ROWSEP
%token CARET UNDERSCORE
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token BAR PRIME
%token EOF

%start <Ast.math> main

%%

main:
  | m = math EOF { m }

math:
  | lead = lead_script* atoms = atom*
      { match lead @ atoms with [ x ] -> x | xs -> Row xs }

lead_script:
  | CARET a = arg { Sup (Row [], a) }
  | UNDERSCORE a = arg { Sub (Row [], a) }

atom:
  | b = postfixed { b }
  | b = postfixed CARET sup = arg sub = sub_opt ps = list(PRIME)
      { with_primes (match sub with None -> Sup (b, sup) | Some s -> SubSup (b, s, sup)) ps }
  | b = postfixed UNDERSCORE sub = arg sup = sup_opt ps = list(PRIME)
      { with_primes (match sup with None -> Sub (b, sub) | Some s -> SubSup (b, sub, s)) ps }

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
  | OVERLINE a = arg { Overline a }
  | g = ACCENT a = arg { Accent (g, a) }
  | s = MTEXT { Mtext s }
  | MATHREL a = arg { Row [ Space "0.278em"; a; Space "0.278em" ] }
  | BEGINCASES rows = separated_nonempty_list(ROWSEP, math) ENDCASES { Cases rows }

arg:
  | b = base { b }
