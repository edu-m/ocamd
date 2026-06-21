(* A markdown document is a list of [block]s; each block carries [inline]
   content. Inline LaTeX (written [$...$]) is parsed into the [math] AST, which
   the MathML renderer turns into <math> markup.

   The point of having these types is that the tree can be inspected, optimized before rendering (see transform.ml). *)

type math =
  | Row of math list
  | Num of string
  | Ident of string
  | Font of string * string
  | Op of string
  | LargeOp of string
  | Sup of math * math
  | Sub of math * math
  | SubSup of math * math * math
  | Frac of math * math
  | Sqrt of math
  | Fenced of string * math * string
  | Xarrow of string * math
  | Over of math * math
  | Infer of math * math
  | Space of string
  | Overline of math
  | Mtext of string
  | Cases of math list

type inline =
  | Text of string
  | Emph of inline list
  | Strong of inline list
  | Link of inline list * string
  | Math of math

type block =
  | Heading of int * inline list
  | Paragraph of inline list
  | Blank
