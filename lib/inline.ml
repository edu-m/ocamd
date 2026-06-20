open Ast
open Md_lexer

let raw = function
  | TEXT s -> s
  | ESCAPED c -> String.make 1 c
  | STAR -> "*"
  | DSTAR -> "**"
  | LBRACK -> "["
  | RBRACK -> "]"
  | LPAREN -> "("
  | RPAREN -> ")"
  | MATH s -> "$" ^ s ^ "$"

let is_blank s = String.trim s = ""

let rec run stop toks =
  match toks with
  | [] -> ([], [], false)
  | t :: rest when Some t = stop -> ([], rest, true)
  | t :: rest ->
      let node, rest' =
        match t with
        | TEXT s -> (Some (Text s), rest)
        | ESCAPED c -> (Some (Text (String.make 1 c)), rest)
        | MATH s -> (Some (Math (Latex.parse s)), rest)
        | DSTAR -> emphasis (fun xs -> Strong xs) DSTAR rest
        | STAR -> emphasis (fun xs -> Emph xs) STAR rest
        | LBRACK -> link rest
        | (RBRACK | LPAREN | RPAREN) as tk -> (Some (Text (raw tk)), rest)
      in
      let more, rest'', closed = run stop rest' in
      let nodes = match node with Some n -> n :: more | None -> more in
      (nodes, rest'', closed)

and emphasis wrap opener toks =
  match run (Some opener) toks with
  | inner, rest, true -> (Some (wrap inner), rest)
  | _ -> (Some (Text (raw opener)), toks)

and link toks =
  match run (Some RBRACK) toks with
  | inner, rest, true -> (
      match skip_blank rest with
      | LPAREN :: after -> (
          match url after with
          | Some (u, rest') -> (Some (Link (inner, u)), rest')
          | None -> (Some (Text "["), toks))
      | _ -> (Some (Text "["), toks))
  | _ -> (Some (Text "["), toks)

and skip_blank = function
  | TEXT s :: rest when is_blank s -> rest
  | toks -> toks

and url toks =
  let buf = Buffer.create 32 in
  let rec go = function
    | RPAREN :: rest -> Some (Buffer.contents buf, rest)
    | [] -> None
    | t :: rest ->
        Buffer.add_string buf (raw t);
        go rest
  in
  go toks

let parse (s : string) : inline list =
  let inlines, _, _ = run None (Md_lexer.tokens s) in
  inlines
