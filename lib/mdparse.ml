(* Markdown -> HTML converter
    stdin   -> stdout
*)

let document (inc : in_channel) : string =
  inc |> Block.parse |> Transform.run |> Render.document

let markdown_to_html (inc : in_channel) (out : out_channel) : unit =
  output_string out (document inc)
