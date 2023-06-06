(*type symbol_kind =
  | STATIC
  | FIELD
  | ARG
  | VAR
let symbol_kind_id = function
  | STATIC -> "static"
  | FIELD -> "field"
  | ARG -> "arg"
  | VAR -> "var"

module SymbolKind = struct
  let map = [
    ("static", STATIC);
    ("field", FIELD);
    ("arg", ARG);
    ("var", VAR)
  ];;

  let get_symbol_kind_enum id map =
    try
      List.assoc id map
    with Not_found -> failwith "SymbolKind not found"
end


type kind = {
  mutable id : string ;

};;

let create_kind id =
  { id = id };;


type symbol = {
  mutable name : string;
  mutable type_ : string;
  mutable kind : SymbolKind;
  mutable id : int ;
};;


let create_symbol name type_ kind id =
  { name = name; type_ = type_; kind = kind; id = id };;
  *)
