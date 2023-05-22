type tokenizer = {
  mutable file : in_channel;
  mutable has_more_token : bool;
  mutable current_token : string;
};;

type token = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST | PASS;;
type key = CLASS | METHOD | FUNCTION | CONSTRUCTOR | INT | BOOLEAN | CHAR | VOID | VAR | STATIC | FIELD |
            LET | DO | IF | ELSE | WHILE | RETURN | TRUE | FALSE | NULL | THIS;;


let token_type (current:string) =
  try
    match current with
    | "class" | "constructor" | "function" | "method" | "field" | "static" | "var" | "int" | "char" | "boolean"
    | "void" | "true" | "false" | "null" | "this" | "let" | "do" | "if" | "else" | "while" | "return" -> KEYWORD
    | "{" | "}" | "(" | ")" | "[" | "]" | "." | "," | ";" | "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">"
    | "=" | "~" -> SYMBOL
    | _ ->
      let length = String.length current in
      let rec is_valid_string_char i =
        if i >= length then
          true
        else
          let c = String.get current i in
          if c = '\"' || c = '\n' then
            false
          else
            is_valid_string_char (i + 1)
      in
      if length > 1 && String.get current 0 = '\"' && String.get current (length - 1) = '\"' && is_valid_string_char 1 then
        STRING_CONST
      else if length > 0 &&
              let first_char = String.get current 0 in
              (first_char >= 'a' && first_char <= 'z') || (first_char >= 'A' && first_char <= 'Z') || first_char = '_' &&
              let rec is_valid_identifier_char i =
                if i >= length then
                  true
                else
                  let c = String.get current i in
                  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'
              in
              is_valid_identifier_char 1 then
        IDENTIFIER
      else
        let num = int_of_string current in
        if num >= 0 && num <= 32767 then
          INT_CONST
        else
          failwith "This token is not supported"

  with
  | Failure _ -> PASS
  ;;


let has_more_tokens (t:tokenizer) = t.has_more_token;;

let advance (t:tokenizer) = 
  try
    if has_more_tokens t then
      t.current_token <- input_line t.file;
  with End_of_file ->
    close_in t.file;
    t.has_more_token <- false;;
  
let constructor (file_path:string) = 
  let open_file = open_in file_path in
  let t = {file = open_file; has_more_token = true; current_token = ""} in
  advance t;
  t;;


