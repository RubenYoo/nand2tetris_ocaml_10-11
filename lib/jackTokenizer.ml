
type tokenizer = {
  mutable file : in_channel;
  mutable has_more_token : bool;
  mutable current_token : string;
};;

type token = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST | PASS;;
type key = CLASS | METHOD | FUNCTION | CONSTRUCTOR | INT | BOOLEAN | CHAR | VOID | VAR | STATIC | FIELD |
            LET | DO | IF | ELSE | WHILE | RETURN | TRUE | FALSE | NULL | THIS | PASS;;


let token_type (current: string) =
  let is_valid_string = Str.string_match (Str.regexp  "^\"[^\"\n]*\"$") current 0 in
  let is_valid_identifier = Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z0-9_]*") current 0 in
  let is_valid_int = Str.string_match (Str.regexp "^(?:0|[1-9]\\d{0,4}|327[0-6][0-7])$") current 0 in
  try
    match current with
    | "class" | "constructor" | "function" | "method" | "field" | "static" | "var" | "int" | "char" | "boolean"
    | "void" | "true" | "false" | "null" | "this" | "let" | "do" | "if" | "else" | "while" | "return" -> KEYWORD
    | "{" | "}" | "(" | ")" | "[" | "]" | "." | "," | ";" | "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">"
    | "=" | "~" -> SYMBOL
    | _ -> 
      if is_valid_string then
        STRING_CONST
      else if is_valid_identifier then
        IDENTIFIER
      else if is_valid_int then
        INT_CONST
      else
        failwith "this token is not supported"
  with
  | Failure _ -> PASS
;;

let keyword (current:string) = 
  try
    match current with
    | "class" -> CLASS
    | "constructor" -> CONSTRUCTOR
    | "function" -> FUNCTION
    | "method" -> METHOD
    | "field" -> FIELD
    | "static" -> STATIC
    | "var"  -> VAR
    | "int" -> INT
    | "char" -> CHAR
    | "boolean" -> BOOLEAN
    | "void" -> VOID
    | "true" -> TRUE
    | "false" -> FALSE
    | "null" -> NULL
    | "this" -> THIS
    | "let" -> LET
    | "do" -> DO
    | "if" -> IF
    | "else" -> ELSE
    | "while" -> WHILE
    | "return" -> RETURN
    | _ -> failwith "this keyword is not supported"
  with
  | Failure _ -> PASS
;;

let is_terminal (tok: string) =
  match tok with
    | "{" | "}" | "(" | ")" | "[" | "]" | "." | "," | ";" | "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">"
    | "=" | "~" -> true
    | _ -> false;;

let has_more_tokens (t:tokenizer) = t.has_more_token;;

let advance (t:tokenizer) =
  try
    (*print_endline "--";*)
    t.current_token <- "";
    let rec read_char () =
      let current = (input_char t.file) in
      (*print_string current;*)
      if current = '/' then
        handle_comment ()
      else if current = ' ' || int_of_char current  = 10 || int_of_char current = 13 || current = '\t' then
        read_char ()
      else
        handle_token current 
    and handle_comment () =
      
      let next_char = String.make 1 (input_char t.file) in
      seek_in t.file (pos_in t.file - 1);
      match next_char with
      | "/" -> consume_line_comment ()
      | "*" -> consume_block_comment ()
      | _ -> handle_token '/' 
    and consume_line_comment () =
      let rec read_line () =
        let c = input_char t.file in
        match c with
        | '\n' -> read_char ()
        | _ -> read_line ()
      in
      read_line ()
    and consume_block_comment () =
      let rec read_block () =
        let c = String.make 1 (input_char t.file) in
        match c with
        | "*" ->
          let next_char = String.make 1 (input_char t.file) in
          begin
            match next_char with
            | "/" -> read_char ()
            | _ -> read_block ()
          end
        | _ -> read_block () 
      in
      read_block ()
    and handle_token cur =
      t.current_token <- t.current_token ^  String.make 1  cur;
      let next_char = String.make 1 (input_char t.file) in
      seek_in t.file (pos_in t.file - 1);

      (*let curr_tok = token_type t.current_token in
      let next_tok = token_type (t.current_token ^ next_char) in
      if curr_tok == next_tok then
        read_char()*)
    
      if next_char <> " " && not (is_terminal t.current_token) && not (is_terminal next_char) then
        read_char()
    in
    read_char ()
with End_of_file ->
  close_in t.file;
  t.has_more_token <- false;;

let symbol (t: tokenizer) =
  String.get t.current_token 0;;

let identifier (t: tokenizer) =
  t.current_token;;

let int_val (t:tokenizer) =
  int_of_string t.current_token;;

let string_val (t:tokenizer) =
  t.current_token;;

let constructor (file_path:string) = 
  let open_file = open_in file_path in
  let t = {file = open_file; has_more_token = true; current_token = ""} in
  t;;
