(** Executes compiler on input files. *)
open Stdio
let rindex_opt (str : string) (chr : char) : int option =
  let len = String.length str in
  let rec find_last index =
    if index < 0 then None
    else if int_of_char  str.[index] =  int_of_char chr then Some index
    else find_last (index - 1)
  in
  find_last (len - 1)
let split_extension filename =
  match rindex_opt filename '.' with
  | Some index -> 
    let name = String.sub filename 0 index in
    let extension = String.sub filename (index + 1) (String.length filename - index - 1) in
    (name, extension)
  | None -> (filename, "")
let get_output_file_name (input_filename : string) : string =
  let
    base_filename, (_) = split_extension input_filename
  in
  base_filename ^ ".vm"

(* Returns list of input .jack files.

  returns all .jack files at the top level of that directory.
*)
let get_input_files (input_path : string) : string list =
 
    let files = input_path |> Sys.readdir in
    let filter_jack_files files =
      Array.fold_right (fun filename acc ->
        if Filename.check_suffix filename ".jack" then
          (Sys.argv.(1) ^ filename) :: acc
        else
          acc
      ) files []
      |> List.rev in
      filter_jack_files files 


(* Prints the position information in [lexbuf] to [out_channel]. *)
let print_position
    (out_channel : Out_channel.t)
    (lexbuf : Lexing.lexbuf)
  : unit =
  let p = lexbuf.lex_curr_p in
  Printf.fprintf out_channel "%s"  p.pos_fname ;
  Printf.fprintf out_channel "%s" (string_of_int  p.pos_lnum);
  Printf.fprintf out_channel "%s" (string_of_int (p.pos_cnum - p.pos_bol + 1))

(* Parses the contents of lexbuf, printing any error that appears. *)
let try_parse (lexbuf : Lexing.lexbuf) : Ast_types.class_declaration =
  (*compile .jack to tokens, according lexer rules*)
  (*Then parse it to vm according to parser.mly*)
  try Parser.program Lexer.read lexbuf with
  | Lexer.Lexical_error err_msg ->
    Printf.fprintf stderr "%a: lexing error %s\n" print_position lexbuf err_msg;
    exit (-1)
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)



let rec process_input_files (input_files: 'a list) =
  match input_files with
  | [] -> ()  (* Base case: empty list *)
  | input_filename :: rest ->
    let lexbuf = Lexing.from_channel (In_channel.create input_filename) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_filename };
    let jack_code = try_parse lexbuf in
    let vm_code = Compile.compile_program jack_code in
    let output_filename = get_output_file_name input_filename in
    print_endline("compiled to vm: " ^ output_filename);
    Out_channel.write_lines output_filename vm_code;
    process_input_files rest  (* Recursively process the rest of the files *)



(* Runs the compiler on the input path. *)
let run_compiler (input_path : string) : unit =
  let input_files = get_input_files input_path in
  process_input_files input_files


let main =
  run_compiler Sys.argv.(1)

let () = main
    
