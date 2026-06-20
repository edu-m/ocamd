(* Inline MD tokenizer *)

{
type tok =
  | TEXT of string
  | STAR
  | DSTAR
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | ESCAPED of char
  | MATH of string
}

rule token = parse
  | "**" { DSTAR }
  | '*' { STAR }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '\\' (_ as c) { ESCAPED c }
  | '\\' { TEXT "\\" }
  | '$' { MATH (Buffer.contents (math (Buffer.create 32) lexbuf)) }
  | [^ '*' '[' ']' '(' ')' '\\' '$' ]+ as s { TEXT s }
  | eof { raise End_of_file }

and math buf = parse
  | "\\$" { Buffer.add_char buf '$'; math buf lexbuf }
  | '$' { buf }
  | eof { buf }
  | _ as c { Buffer.add_char buf c; math buf lexbuf }

{
let tokens (s : string) : tok list =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match token lexbuf with
    | t -> loop (t :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []
}
