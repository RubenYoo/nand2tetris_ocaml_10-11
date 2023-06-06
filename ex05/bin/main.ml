open Jack_compiler
let my_read_file filename =
  let ic = open_in filename in
  let rec read_lines lines =
    try
      let line = input_line ic in
      read_lines (line :: lines)
    with
      End_of_file ->
        close_in ic;
        String.concat "\n" (List.rev lines)
  in
  read_lines []


let main =

  let source_path = Array.get Sys.argv 0 in
  Sys.readdir  source_path
  |> Array.to_list
  |> List.iter (fun file ->
    
    if Filename.check_suffix file ".jack" then
      let index = String.rindex file '.' in
      if index > -1 then
        let destination_path = String.sub file 0 (index + 1) ^ "T.xml" in

        (*let input_string = my_read_file(source_path ^ "/" ^ file) in*)
        let write_file = open_out destination_path in
        CompilerEngine.compile (source_path ^ "/" ^ file);
        Out_channel.flush write_file;
        let read_file = my_read_file destination_path in
        let input_string_parse = my_read_file read_file in
        let destination_path_parse =  String.sub file 0 (index + 1) ^ ".vm" in
        let write_file_parse = open_out destination_path_parse in
        (*CodeParser.ParseClass (CodeGeneratorWriter.create (), input_string_parse, write_file_parse);*)
        Out_channel.flush write_file_parse;
        (*In_channel.close read_file;
        Out_channel.close write_file_parse*)
  )

let () = main