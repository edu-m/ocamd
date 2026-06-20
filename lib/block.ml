open Ast

let count_hashes line =
  let n = String.length line in
  let rec go i = if i < n && line.[i] = '#' then go (i + 1) else i in
  go 0

let chomp line =
  let n = String.length line in
  if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line

let block_of_line line =
  let line = chomp line in
  if line = "" then Blank
  else if line.[0] = '#' then (
    let lvl = count_hashes line in
    let i = ref lvl in
    let n = String.length line in
    while !i < n && line.[!i] = ' ' do
      incr i
    done;
    Heading (lvl, Inline.parse (String.sub line !i (n - !i))))
  else Paragraph (Inline.parse line)

let parse (inc : in_channel) : block list =
  let rec loop acc =
    match input_line inc with
    | line -> loop (block_of_line line :: acc)
    | exception End_of_file -> List.rev acc
  in
  loop []
