(* Markdown -> HTML converter
    stdin   -> stdout
*)

let document (inc : in_channel) : string =
  inc |> Block.parse |> Transform.run |> Render.document

let markdown_to_html (inc : in_channel) (out : out_channel) : unit =
  let buf = Buffer.create 256 in
  let rec loop () =
    match input_line inc with
    | line ->
        let blocks = Transform.run [ Block.block_of_line line ] in
        Buffer.clear buf;
        List.iter (Render.block buf) blocks;
        output_string out (Buffer.contents buf);
        flush out;
        loop ()
    | exception End_of_file -> ()
  in
  loop ()
