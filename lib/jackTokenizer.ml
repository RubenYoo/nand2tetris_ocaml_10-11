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


let rec do_while t condition =
  t.current_token <- "\"";
  if not(condition) then begin
    let next_char = input_char t.file in
    t.current_token <- t.current_token ^ (String.make 1 next_char);
    do_while t (next_char = '"')
  end


let rec do_while_word t condition =
  t.current_token <- "";
  if not(condition) then begin
    let next_char = input_char t.file in
    t.current_token <- t.current_token ^ (String.make 1 next_char);
    do_while_word t (next_char <> ' ')
  end

  
let rec read_next_word channel word t =
  let next_char = input_char channel in
  if is_terminal (String.make 1 next_char) then
    (seek_in t.file (pos_in t.file - 1));
  if  next_char = ' ' || is_terminal (String.make 1 next_char)  then
    if String.length word > 0 then
      word
    else
      read_next_word channel "" t
  else
    read_next_word channel (word ^ String.make 1 next_char) t

let is_alphabetic_char c =
  let code = Char.code c in
  code >= Char.code 'a' && code <= Char.code 'z' || code >= Char.code 'A' && code <= Char.code 'Z'

let is_token_character c =
  c = ';' || c = ')' || c = '[' || c = ']' || c = '.' || c = '?' || c = '(' || c = '"'

let tokenize_expression expr =
  let len = String.length expr in
  let tokens = ref [] in
  let i = ref 0 in

  while !i < len do
    let current_char = expr.[!i] in
    if is_token_character current_char then
      tokens := (String.make 1 current_char) :: !tokens
    else begin
      let token = ref "" in
      let j = ref !i in
      while !j < len && not (is_token_character expr.[!j]) do
        token := !token ^ (String.make 1 expr.[!j]);
        j := !j + 1
      done;
      tokens := !token :: !tokens;
      i := !j - 1
    end;
    i := !i + 1
  done;

  List.rev !tokens


let split_by_spaces str =
  String.split_on_char ' ' str

let handle_word word  ft =
  if ft = false  then
    ([], false)
  else
    let my_list = [] in
    if String.length word = 1 then
      match word  with
      | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"->
        let my_list = word :: my_list in
        (my_list, true)
      | "{" | "}" | "(" | ")" | "[" | "]" | "." | "," | ";" | "+" | "-" | "*" | "/" | "&" | "|" | "<" | ">"| "=" | "~"  ->  
        let my_list = word :: my_list in
        (my_list, true)
      | _ -> (my_list, true)
    else
      let start = String.sub word 0 2 in
      if start = "//" || start = "/*" then 
        (my_list, false)
      else if start = "*/" then
        (my_list, false)
      else

        let tokens = tokenize_expression word in
        List.iter print_endline (tokens);
        ([], true)
  ;;
  
let remove_leading_spaces line =
  let rec remove_leading_spaces_helper idx =
    if idx < String.length line && (String.get line idx = ' ' ||  String.get line idx = '\t') then
      remove_leading_spaces_helper (idx + 1)
    else
      String.sub line idx (String.length line - idx)
  in
  remove_leading_spaces_helper 0


let advance_two (t: tokenizer) =
  let current_line = input_line t.file in
  let wordis = remove_leading_spaces current_line in
  let words = Str.split (Str.regexp " ") wordis in
  let ft = true in
  let rec process_words includeBool = function
    | [] -> ()
    | current_str :: rest ->
      let (listy, bb) = handle_word current_str includeBool  in
      (* Do something with listy, booll, and includeBool *)
      if bb = true  && listy <> [] then
        print_endline(List.hd listy);
      process_words bb rest
  in

  let initialBool = ft in  (* Set initial boolean value here *)
  process_words initialBool words



let advance (t:tokenizer) =
  try
    (*print_endline "--";*)
    t.current_token <- "";
    let rec read_char () =
      let current =(input_char t.file) in
      (*print_string current;*)
      if current = '/' then
        handle_comment ()
      else if current = ' ' || int_of_char current  = 10 || int_of_char current = 13 || current = '\t' then
        read_char ()
      else if current = '"' then
        do_while t (current = '"')

        
      else
        handle_token (String.make 1 current)
    and handle_comment () =
      
      let next_char = String.make 1 (input_char t.file) in
      seek_in t.file (pos_in t.file - 1);
      match next_char with
      | "/" -> consume_line_comment ()
      | "*" -> consume_block_comment ()
      | _ -> handle_token "/"
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
      if String.length cur = 1    &&  is_alphabetic_char (String.get cur 0) then
        let next_word_opt = read_next_word t.file cur t in
        print_endline next_word_opt;
      else
        t.current_token <- t.current_token ^ cur;
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
