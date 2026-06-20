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
  | '\\' [' ' ',' ';' ':' '!' '>'] { token lexbuf }
  | "\\frac" { FRAC }
  | "\\sqrt" { SQRT }
  | "\\left" { token lexbuf }
  | "\\right" { token lexbuf }
  | "\\mathbb" sp* '{' sp* (letter as c) sp* '}'
      { match Symbols.blackboard (String.make 1 c) with
        | Some s -> IDENT s
        | None -> IDENT (String.make 1 c) }
  | "\\mathrm" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("normal", String.trim s) }
  | "\\mathit" sp* '{' ([^ '{' '}']* as s) '}' { MATHFONT ("italic", String.trim s) }
  | "\\mathrel" sp* '{' ([^ '{' '}']* as s) '}' { OP (String.trim s) }
  | "\\xrightarrow" { XARROW "\xe2\x86\x92" }
  | "\\xleftarrow" { XARROW "\xe2\x86\x90" }
  | "\\{" { OP "{" }
  | "\\}" { OP "}" }
  | "\\|" { OP "\xe2\x80\x96" }
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
