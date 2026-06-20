(* LaTeX -> MathML *)

open Ast

let escape buf s =
  String.iter
    (fun c ->
      match c with
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | _ -> Buffer.add_char buf c)
    s

let element buf tag content =
  Buffer.add_char buf '<';
  Buffer.add_string buf tag;
  Buffer.add_char buf '>';
  escape buf content;
  Buffer.add_string buf "</";
  Buffer.add_string buf tag;
  Buffer.add_char buf '>'

let rec node buf = function
  | Num n -> element buf "mn" n
  | Ident i -> element buf "mi" i
  | Upright s ->
      Buffer.add_string buf "<mi mathvariant=\"normal\">";
      escape buf s;
      Buffer.add_string buf "</mi>"
  | Op o -> element buf "mo" o
  | Row xs ->
      Buffer.add_string buf "<mrow>";
      List.iter (node buf) xs;
      Buffer.add_string buf "</mrow>"
  | Sup (b, s) -> wrap2 buf "msup" b s
  | Sub (b, s) -> wrap2 buf "msub" b s
  | SubSup (b, sub, sup) ->
      Buffer.add_string buf "<msubsup>";
      node buf b;
      node buf sub;
      node buf sup;
      Buffer.add_string buf "</msubsup>"
  | Frac (a, b) -> wrap2 buf "mfrac" a b
  | Sqrt a ->
      Buffer.add_string buf "<msqrt>";
      node buf a;
      Buffer.add_string buf "</msqrt>"
  | Fenced (op, body, cl) ->
      Buffer.add_string buf "<mrow>";
      element buf "mo" op;
      node buf body;
      element buf "mo" cl;
      Buffer.add_string buf "</mrow>"

and wrap2 buf tag a b =
  Buffer.add_char buf '<';
  Buffer.add_string buf tag;
  Buffer.add_char buf '>';
  node buf a;
  node buf b;
  Buffer.add_string buf "</";
  Buffer.add_string buf tag;
  Buffer.add_char buf '>'

let render (m : math) : string =
  let buf = Buffer.create 128 in
  Buffer.add_string buf
    "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
  node buf m;
  Buffer.add_string buf "</math>";
  Buffer.contents buf
