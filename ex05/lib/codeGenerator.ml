

let write_symbol symbol file_writer typ =
  if typ = "expression" then
    match symbol with
    | "+" -> output_string file_writer "add\n"
    | "-" -> output_string file_writer "sub\n"
    | "*" -> output_string file_writer "call Math.multiply 2\n"
    | "/" -> output_string file_writer "call Math.divide 2\n"
    | "&amp;" -> output_string file_writer "and\n"
    | "|" -> output_string file_writer "or\n"
    | "&lt;" -> output_string file_writer "lt\n"
    | "&gt;" -> output_string file_writer "gt\n"
    | "=" -> output_string file_writer "eq\n"
    | _ -> ()
  else if typ = "term" then
    match symbol with
    | "~" -> output_string file_writer "not\n"
    | "-" -> output_string file_writer "neg\n"
    | "|" -> output_string file_writer "or\n"
    | "&lt;" -> output_string file_writer "lt\n"
    | "&gt;" -> output_string file_writer "gt\n"
    | "=" -> output_string file_writer "eq\n"
    | _ -> ()
  else ()

let write_keyword keyword file_writer =
  match keyword with
  | "true" -> output_string file_writer "push constant 0\nnot\n"
  | "false" | "null" -> output_string file_writer "push constant 0\n"
  | "this" -> output_string file_writer "push pointer 0\n"
  | _ -> ()
let write_string_constant string_constant file_writer =
  let length = string_of_int (String.length string_constant) in
  output_string file_writer ("push constant " ^ length ^ "\n");
  output_string file_writer "call String.new 1\n";
  for i = 0 to String.length string_constant - 1 do
    let temp = int_of_char string_constant.[i] in
    let temp_str = string_of_int temp in
    output_string file_writer ("push constant " ^ temp_str ^ "\n");
    output_string file_writer ("call String.appendChar 2\n")
  done


let write_op file_writer op typ =
  if typ = "expression" then
    match op with
    | "+" -> output_string file_writer "add\n"
    | "-" -> output_string file_writer "sub\n"
    | "*" -> output_string file_writer "call Math.multiply 2\n"
    | "/" -> output_string file_writer "call Math.divide 2\n"
    | "&amp;" -> output_string file_writer "and\n"
    | "|" -> output_string file_writer "or\n"
    | "&lt;" -> output_string file_writer "lt\n"
    | "&gt;" -> output_string file_writer "gt\n"
    | "=" -> output_string file_writer "eq\n"
    | _ -> ()
  else if typ = "term" then
    match op with
    | "~" -> output_string file_writer "not\n"
    | "-" -> output_string file_writer "neg\n"
    | "|" -> output_string file_writer "or\n"
    | "&lt;" -> output_string file_writer "lt\n"
    | "&gt;" -> output_string file_writer "gt\n"
    | "=" -> output_string file_writer "eq\n"
    | _ -> ()
  else ()



let write_integer_constant integer_constant file_writer =
  let constant = string_of_int integer_constant in
  output_string file_writer ("push constant " ^ constant ^ "\n");



