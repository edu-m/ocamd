open Ast

(* adjacent text block coalescing (optimization) *)

let rec coalesce_inlines = function
  | Text a :: Text b :: rest -> coalesce_inlines (Text (a ^ b) :: rest)
  | Emph xs :: rest -> Emph (coalesce_inlines xs) :: coalesce_inlines rest
  | Strong xs :: rest -> Strong (coalesce_inlines xs) :: coalesce_inlines rest
  | Link (xs, u) :: rest ->
      Link (coalesce_inlines xs, u) :: coalesce_inlines rest
  | x :: rest -> x :: coalesce_inlines rest
  | [] -> []

(* empty emphasis pruner *)

let rec prune_inlines xs =
  List.filter_map
    (fun n ->
      match n with
      | (Emph [] | Strong []) -> None
      | Emph ys -> Some (Emph (prune_inlines ys))
      | Strong ys -> Some (Strong (prune_inlines ys))
      | Link (ys, u) -> Some (Link (prune_inlines ys, u))
      | other -> Some other)
    xs

let on_inlines f = function
  | Heading (l, xs) -> Heading (l, f xs)
  | Paragraph xs -> Paragraph (f xs)
  | Blank -> Blank

let passes : (block list -> block list) list =
  [
    List.map (on_inlines coalesce_inlines);
    List.map (on_inlines prune_inlines);
  ]

let run (doc : block list) : block list =
  List.fold_left (fun d pass -> pass d) doc passes
