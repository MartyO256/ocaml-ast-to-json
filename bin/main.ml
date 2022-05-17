let main_stdout input_file =
  let input_channel = open_in input_file in
  try
    let lexing_buffer =
      Lexing.from_channel ~with_positions:true input_channel
    in
    let parsetree_structure = Parse.implementation lexing_buffer in
    Format.fprintf Format.std_formatter "%a\n"
      Ocaml_ast_json.Pprintast_json.structure parsetree_structure;
    close_in input_channel
  with e ->
    close_in_noerr input_channel;
    raise e

let main input_file output_file =
  let input_channel = open_in input_file in
  let output_channel = open_out output_file in
  try
    let lexing_buffer =
      Lexing.from_channel ~with_positions:true input_channel
    in
    let parsetree_structure = Parse.implementation lexing_buffer in
    Format.fprintf
      (Format.formatter_of_out_channel output_channel)
      "%a\n" Ocaml_ast_json.Pprintast_json.structure parsetree_structure;
    close_in input_channel;
    close_out output_channel
  with e ->
    close_in_noerr input_channel;
    close_out_noerr output_channel;
    raise e

let () =
  let usage_message = "parse-ocaml-to-json <file> [--output-file <output>]" in
  let input_file = ref "" in
  let output_file = ref "" in
  let specification_list =
    [
      ("--output-file", Arg.Set_string output_file, "Set the output file name");
    ]
  in
  Arg.parse specification_list
    (fun filename -> input_file := filename)
    usage_message;

  if !output_file = "" then main_stdout !input_file
  else main !input_file !output_file
