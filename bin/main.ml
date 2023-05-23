open N_translator

let print_all_token (file_path: string) =
  let t = (JackTokenizer.constructor file_path) in
  while JackTokenizer.has_more_tokens t do
    JackTokenizer.advance t;
    print_endline t.current_token;
  done;
;;




let main () = 
  let file = (Sys.argv.(1)) in
  print_endline file;
  print_all_token file;;


let () = main ();;
