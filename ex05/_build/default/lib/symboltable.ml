

type symbol_kind =
  | STATIC 
  | FIELD 
  | ARG 
  | VAR

type symbol = {
  name : string;
  type_ : string;
  kind : symbol_kind;
  id : int;
}

type symbol_table = {
  mutable class_table : (string, symbol) Hashtbl.t;
  mutable method_table : (string, symbol) Hashtbl.t;
  mutable static_counter : int;
  mutable field_counter : int;
  mutable arg_counter : int;
  mutable var_counter : int;
}

let create_symbol_table () =
  {
    class_table = Hashtbl.create 16;
    method_table = Hashtbl.create 16;
    static_counter = 0;
    field_counter = 0;
    arg_counter = 0;
    var_counter = 0;
  }

let start_subroutine st is_method =
  st.method_table <- Hashtbl.create 16;
  st.arg_counter <- 0;
  st.var_counter <- 0;
  if is_method then st.arg_counter <- 1

let define st name type_ kind =
  match kind with
  | FIELD ->
      let symbol = { name = name; type_ = type_; kind = FIELD; id = st.field_counter } in
      Hashtbl.add st.class_table name symbol;
      st.field_counter <- st.field_counter + 1
  | STATIC ->
      let symbol = { name = name; type_ = type_; kind = STATIC; id = st.static_counter } in
      Hashtbl.add st.class_table name symbol;
      st.static_counter <- st.static_counter + 1
  | ARG ->
      let symbol = { name = name; type_ = type_; kind = ARG; id = st.arg_counter } in
      Hashtbl.add st.method_table name symbol;
      st.arg_counter <- st.arg_counter + 1
  | VAR ->
      let symbol = { name = name; type_ = type_; kind = VAR; id = st.var_counter } in
      Hashtbl.add st.method_table name symbol;
      st.var_counter <- st.var_counter + 1

let var_count st kind =
  match kind with
  | FIELD -> st.field_counter
  | STATIC -> st.static_counter
  | ARG -> st.arg_counter
  | VAR -> st.var_counter

let kind_of st name =
  if Hashtbl.mem st.method_table name then
    (Hashtbl.find st.method_table name).kind
  else if Hashtbl.mem st.class_table name then
    (Hashtbl.find st.class_table name).kind
  else
    failwith "Symbol not found"

let type_of st name =
  if Hashtbl.mem st.method_table name then
    (Hashtbl.find st.method_table name).type_
  else if Hashtbl.mem st.class_table name then
    (Hashtbl.find st.class_table name).type_
  else
    failwith "Symbol not found"

let index_of st name =
  if Hashtbl.mem st.method_table name then
    (Hashtbl.find st.method_table name).id
  else if Hashtbl.mem st.class_table name then
    (Hashtbl.find st.class_table name).id
  else
    0
