let expression = "length(i));"

let is_token_character c =
  c = ';' || c = ')'

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

let tokens = tokenize_expression expression ;;

List.iter print_endline tokens;
