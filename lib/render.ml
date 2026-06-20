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

let rec inline buf = function
  | Text s -> escape buf s
  | Emph xs ->
      Buffer.add_string buf "<em>";
      inlines buf xs;
      Buffer.add_string buf "</em>"
  | Strong xs ->
      Buffer.add_string buf "<strong>";
      inlines buf xs;
      Buffer.add_string buf "</strong>"
  | Link (xs, url) ->
      Buffer.add_string buf "<a href=\"";
      escape buf url;
      Buffer.add_string buf "\">";
      inlines buf xs;
      Buffer.add_string buf "</a>"
  | Math m -> Buffer.add_string buf (Mathml.render m)

and inlines buf xs = List.iter (inline buf) xs

let block buf = function
  | Heading (lvl, xs) ->
      Printf.bprintf buf "<h%d>" lvl;
      inlines buf xs;
      Printf.bprintf buf "</h%d>\n" lvl
  | Paragraph xs ->
      Buffer.add_string buf "<p>";
      inlines buf xs;
      Buffer.add_string buf "</p>\n"
  | Blank -> Buffer.add_char buf '\n'

let document (doc : block list) : string =
  let buf = Buffer.create 1024 in
  List.iter (block buf) doc;
  Buffer.contents buf
