let parse (src : string) : Ast.math =
  let lexbuf = Lexing.from_string src in
  try Latex_parser.main Latex_lexer.token lexbuf
  with Latex_parser.Error | Failure _ -> Ast.Row [ Ast.Op src ]
