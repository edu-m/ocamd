{
open Latex_parser

let sym name =
  let kind, s = Symbols.lookup name in
  match kind with
  | Symbols.Mi -> IDENT s
  | Symbols.Mo -> OP s
  | Symbols.Mn -> NUM s
  | Symbols.LargeOp -> LARGEOP s
}

let digit = ['0'-'9']
let num = digit+ ('.' digit+)?
let letter = ['a'-'z' 'A'-'Z']
let sp = [' ' '\t' '\n' '\r']

rule token = parse
  | sp+ { token lexbuf }
  | '\\' ' ' { SPACE "0.25em" }
  | "\\," { SPACE "0.167em" }
  | "\\:" { SPACE "0.222em" }
  | "\\>" { SPACE "0.222em" }
  | "\\;" { SPACE "0.278em" }
  | "\\!" { SPACE "-0.167em" }
  | "\\quad" { SPACE "1em" }
  | "\\qquad" { SPACE "2em" }
  | "\\begin{cases}" { BEGINCASES }
  | "\\end{cases}" { ENDCASES }
  | "\\\\" { ROWSEP }
  | "\\frac" { FRAC }
  | "\\inferrule" { INFER }
  | "\\overline" { OVERLINE }
  | "\\hat" { ACCENT "^" }
  | "\\sqrt" { SQRT }
  | "\\left" { token lexbuf }
  | "\\right" { token lexbuf }
  | "\\mathbb" sp* '{' sp* (letter as c) sp* '}'
      { match Symbols.blackboard (String.make 1 c) with
        | Some s -> IDENT s
        | None -> IDENT (String.make 1 c) }
  | "\\mathrm" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("normal", String.trim s) }
  | "\\mathit" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("italic", String.trim s) }
  | "\\mathcal" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("script", String.trim s) }
  | "\\mathbf" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("bold", String.trim s) }
  | "\\text" sp* '{' ([^ '{' '}']* as s) '}' { MTEXT s }
  | "\\mathrel" { MATHREL }
  | "\\xrightarrow" { XARROW "\xe2\x86\x92" }
  | "\\xleftarrow" { XARROW "\xe2\x86\x90" }
  | "\\stackrel" { STACKREL }
  | "\\{" { OP "{" }
  | "\\}" { OP "}" }
  | "\\|" { OP "\xe2\x80\x96" }
  | "\\#" { OP "#" }
  | '\\' (letter+ as name) { sym name }
  | num as n { NUM n }
  | letter as c { IDENT (String.make 1 c) }
  | '^' { CARET }
  | '_' { UNDERSCORE }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '|' { BAR }
  | '\'' { PRIME }
  | ['+' '-' '*' '/' '=' '<' '>' ',' '.' ':' ';' '!' '?'] as c
      { OP (String.make 1 c) }
  | eof { EOF }
  | _ as c { OP (String.make 1 c) }
