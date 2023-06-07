open Scanf
type tokenType = KEYWORD | SYMBOL | IDENTIFIER | INT_CONST | STRING_CONST
type keywordType = CLASS | CONSTRUCTOR | FUNCTION | METHOD | FIELD | STATIC | VAR | INT | CHAR | BOOLEAN | VOID | TRUE | FALSE | NULL | THIS | LET | DO | IF | ELSE | WHILE | RETURN
type symbolType = LEFT_CURLY_BRACKET | RIGHT_CURLY_BRACKET | LEFT_ROUND_BRACKET | RIGHT_ROUND_BRACKET | LEFT_SQUARE_BRACKET | RIGHT_SQUARE_BRACKET | PERIOD | COMMA | SEMI_COLON | PLUS_SIGN | HYPHEN | ASTERISK | SLASH | AMPERSAND | VERTICAL_LINE | LESS_THAN_SIGN | GREATER_THAN_SIGN | EQUAL | TILDE
exception ArgumentError

let symbol_type_to_string = function
  | LEFT_CURLY_BRACKET     -> "left_curly_bracket"
  | RIGHT_CURLY_BRACKET    -> "right_curly_bracket"
  | LEFT_ROUND_BRACKET     -> "left_round_bracket"
  | RIGHT_ROUND_BRACKET    -> "right_round_bracket"
  | LEFT_SQUARE_BRACKET    -> "left_square_bracket"
  | RIGHT_SQUARE_BRACKET   -> "right_square_bracket"
  | PERIOD                 -> "period"
  | COMMA                  -> "comma"
  | SEMI_COLON             -> "semi_colon"
  | PLUS_SIGN              -> "plus_sign"
  | HYPHEN                 -> "hyphen"
  | ASTERISK               -> "asterisk"
  | SLASH                  -> "slash"
  | AMPERSAND              -> "ampersand"
  | VERTICAL_LINE          -> "vertical_line"
  | LESS_THAN_SIGN         -> "less_than_sign"
  | GREATER_THAN_SIGN      -> "greater_than_sign"
  | EQUAL                  -> "equal"
  | TILDE                  -> "tilde"
let reg = Str.regexp "[-\\{\\}\\(\\)\\[\\.,;\\+\\*\\/&\\|<>=~]\\|\\]"

let string_to_symbol_type = function
  | "{"       -> LEFT_CURLY_BRACKET
  | "}"       -> RIGHT_CURLY_BRACKET
  | "("       -> LEFT_ROUND_BRACKET
  | ")"       -> RIGHT_ROUND_BRACKET
  | "["       -> LEFT_SQUARE_BRACKET
  | "]"       -> RIGHT_SQUARE_BRACKET
  | "."       -> PERIOD
  | ","       -> COMMA
  | ";"       -> SEMI_COLON
  | "+"       -> PLUS_SIGN
  | "-"       -> HYPHEN
  | "*"       -> ASTERISK
  | "/"       -> SLASH
  | "&"       -> AMPERSAND
  | "|"       -> VERTICAL_LINE
  | "<"       -> LESS_THAN_SIGN
  | ">"       -> GREATER_THAN_SIGN
  | "="       -> EQUAL
  | "~"       -> TILDE
  | _         -> raise Not_found


let keyword_type_to_string = function
  | "class"        -> CLASS
  | "constructor"  -> CONSTRUCTOR
  | "function"     -> FUNCTION
  | "method"       -> METHOD
  | "field"        -> FIELD
  | "static"       -> STATIC
  | "var"          -> VAR
  | "int"          -> INT
  | "char"         -> CHAR
  | "boolean"      -> BOOLEAN
  | "void"         -> VOID
  | "true"         -> TRUE
  | "false"        -> FALSE
  | "null"         -> NULL
  | "this"         -> THIS
  | "let"          -> LET
  | "do"           -> DO
  | "if"           -> IF
  | "else"         -> ELSE
  | "while"        -> WHILE
  | "return"       -> RETURN
  | _              -> raise Not_found

let token_type_of_string = function
  | "KEYWORD" -> KEYWORD
  | "SYMBOL"-> SYMBOL
  | "IDENTIFIER" -> IDENTIFIER
  | "INT_CONST" -> INT_CONST 
  | "STRING_CONST" -> STRING_CONST
  | _ -> raise ArgumentError

  let token_value_of_string = function
  | "KEYWORD" -> KEYWORD
  | "SYMBOL"-> SYMBOL
  | "IDENTIFIER" -> IDENTIFIER
  | "INT_CONST" -> INT_CONST 
  | "STRING_CONST" -> STRING_CONST
  | _ -> raise ArgumentError



type token = {
  token_type: tokenType;
  value: string;
}



module KeywordMap = Map.Make (String)

let keyword_map =
  KeywordMap.empty
    |> KeywordMap.add "class" CLASS
    |> KeywordMap.add "constructor" CONSTRUCTOR
    |> KeywordMap.add "function" FUNCTION
    |> KeywordMap.add "method" METHOD
    |> KeywordMap.add "field" FIELD
    |> KeywordMap.add "static" STATIC
    |> KeywordMap.add "var" VAR
    |> KeywordMap.add "int" INT
    |> KeywordMap.add "char" CHAR
    |> KeywordMap.add "boolean" BOOLEAN
    |> KeywordMap.add "void" VOID
    |> KeywordMap.add "true" TRUE
    |> KeywordMap.add "false" FALSE
    |> KeywordMap.add "null" NULL
    |> KeywordMap.add "this" THIS
    |> KeywordMap.add "let" LET
    |> KeywordMap.add "do" DO
    |> KeywordMap.add "if" IF
    |> KeywordMap.add "else" ELSE
    |> KeywordMap.add "while" WHILE
    |> KeywordMap.add "return" RETURN
let parse_token input =
  try Some (Scanning.from_channel (input "<%[^>]>\n" (fun s -> s)))
  with End_of_file -> None


let rec tokenize_unit unit tokens =
  match unit with
      "" -> tokens
    | unit ->
      try
        let index = Str.search_forward reg unit 0 in
        let token = if index <> 0 then
            [Batteries.String.slice ~first: 0 ~last: index unit] else [] in
        let rest_unit = Batteries.String.slice ~first: (index + 1) unit in
        let token = token @ [Batteries.String.slice ~first: index ~last: (index + 1) unit] in
        tokenize_unit rest_unit (tokens @ token)
      with Not_found ->
        tokenize_unit "" (tokens @ [unit])

let scanbuf_to_string scanbuf =
  Scanf.bscanf scanbuf "%s" (fun str -> str)

module SymbolMap = Map.Make (String)
let symbol_map =
  SymbolMap.empty
    |> SymbolMap.add "{" LEFT_CURLY_BRACKET
    |> SymbolMap.add "}" RIGHT_CURLY_BRACKET
    |> SymbolMap.add "(" LEFT_ROUND_BRACKET
    |> SymbolMap.add ")" RIGHT_ROUND_BRACKET
    |> SymbolMap.add "[" LEFT_SQUARE_BRACKET
    |> SymbolMap.add "]" RIGHT_SQUARE_BRACKET
    |> SymbolMap.add "." PERIOD
    |> SymbolMap.add "," COMMA
    |> SymbolMap.add ";" SEMI_COLON
    |> SymbolMap.add "+" PLUS_SIGN
    |> SymbolMap.add "-" HYPHEN
    |> SymbolMap.add "*" ASTERISK
    |> SymbolMap.add "/" SLASH
    |> SymbolMap.add "&" AMPERSAND
    |> SymbolMap.add "|" VERTICAL_LINE
    |> SymbolMap.add "<" LESS_THAN_SIGN
    |> SymbolMap.add ">" GREATER_THAN_SIGN
    |> SymbolMap.add "=" EQUAL
    |> SymbolMap.add "~" TILDE

let before_double_quote line =
  Batteries.String.exists line "\"" && Batteries.String.exists line " " &&
    (
      let dq_index = Batteries.String.find line "\"" in
      let space_index = Batteries.String.find line " " in
      dq_index < space_index
    )


let parse_value input =
  try Some (Scanning.from_channel (input "%s\n" (fun s -> s)))
  with End_of_file -> None

let current_token tokens =
  match tokens with
      [] -> raise ArgumentError
    | head :: _ -> head

let option_to_scanbuf = function
  | Some scanbuf -> scanbuf
  | None -> raise (Invalid_argument "Empty scanbuf option")

let rec parse_tokens input =
  match parse_token input with
  | Some token_type ->
      let value = parse_value input in
      { token_type =  token_type_of_string (scanbuf_to_string token_type); value = scanbuf_to_string (option_to_scanbuf value) } :: parse_tokens input
  | None -> []

let keywords tokens =
  KeywordMap.find (current_token tokens) keyword_map

let keyword token =
  KeywordMap.find token keyword_map

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let translate_to_vm token =
  match token.token_type with
  | KEYWORD ->
      (match keyword token.value with
      | CLASS -> "// Translate CLASS keyword to VM code"
      (* Add translation for other keywords *)
      | _ -> "")
  | SYMBOL ->
      (match string_to_symbol_type  token.value with
      | LEFT_CURLY_BRACKET -> "// Translate left curly bracket symbol to VM code"
      (* Add translation for other symbols *)
      | _ -> "")
  | IDENTIFIER -> "// Translate identifier to VM code"
  | INT_CONST -> "// Translate integer constant to VM code"
  | STRING_CONST -> "// Translate string constant to VM code"



let rec tokenize_line line tokens =
  match line with
      "" -> tokens
    | line when Batteries.String.starts_with line "\"" ->
      let index = Batteries.String.find_from line 1 "\"" in
      let rest_line = Batteries.String.slice ~first: (index + 1) line in
      let token = Batteries.String.slice ~first: 0 ~last: (index + 1) line in
      tokenize_line rest_line (tokens @ [token])
    | line when before_double_quote line ->
      let index = Batteries.String.find line "\"" in
      let unit = Batteries.String.slice ~first: 0 ~last: index line in
      let rest_line = Batteries.String.slice ~first: index line
        |> Batteries.String.trim in
      tokenize_line rest_line (tokenize_unit unit tokens)
    | line when Batteries.String.exists line " " ->
      let index = Batteries.String.find line " " in
      let unit = Batteries.String.slice ~first: 0 ~last: index line in
      let rest_line = Batteries.String.slice ~first: (index + 1) line
        |> Batteries.String.trim in
      tokenize_line rest_line (tokenize_unit unit tokens)
    | unit ->
      tokenize_line "" (tokenize_unit unit tokens)


let delete_singleline_comment line =
  if Batteries.String.exists line "//" then
    let index_from = Batteries.String.find line "//" in
    Batteries.String.slice ~first: 0 ~last: index_from line
  else
    line

let split_without_empty_lines content =
  Batteries.String.split_on_string content ~by: "\n"
    |> List.map delete_singleline_comment
    |> List.map Batteries.String.trim
    |> List.filter (fun x -> not (Batteries.String.is_empty x))

let translate_tokens_to_vm tokens =
  List.map translate_to_vm tokens
  |> List.filter (fun line -> line <> "")
let rec delete_multiline_comment content =
  if Batteries.String.exists content "/*" then
    let index_from = Batteries.String.find content "/*" in
    let index_to   = Batteries.String.find content "*/" in
    (Batteries.String.slice ~first: 0 ~last: index_from content) ^ (Batteries.String.slice ~first: (index_to + 2) content)
      |> delete_multiline_comment
  else
    content

let rec tokenize_all_lines lines tokens =
  match lines with 
    [] -> tokens
  | line :: rest ->
    tokenize_all_lines rest (tokenize_line line tokens)

let tokenize content =
  let lines = delete_multiline_comment content
    |> split_without_empty_lines in
  tokenize_all_lines lines []

let create filepath =
  read_whole_file filepath
    |> tokenize

let convert_jack_to_vm jack_file_path vm_file_path =
  (*let jack_content = read_whole_file jack_file_path in
  let tokens = tokenize jack_content in*)
  print_endline vm_file_path;
  let tokens = create jack_file_path in
  List.iter print_endline tokens

