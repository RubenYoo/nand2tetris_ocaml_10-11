(*The AST represents the structure and meaning of programs written in the OCaml-like language. It consists of different types of nodes that correspond to various language constructs.

Expressions:

An expression represents a computation or a value in the program.
The expression types include:
Integer Literal: Represents an integer value.
Boolean Literal: Represents a boolean value.
String Literal: Represents a string value.
Variable: Represents a reference to a variable by its name.
Binary Operation: Represents an operation performed on two expressions.
Unary Operation: Represents an operation performed on a single expression.
Function Call: Represents a call to a function with arguments.
Binary and Unary Operators:

Binary operators are operations that take two expressions as operands and produce a result.
Unary operators are operations that take a single expression as an operand and produce a result.
Examples of binary operators include addition, subtraction, multiplication, division, equality comparison, and comparison operators like less than and greater than.
Examples of unary operators include negation and logical negation.
Statements:

A statement represents an action or a sequence of actions performed in the program.
The statement types include:
Let Statement: Represents variable declaration and assignment.
If Statement: Represents a conditional statement with an expression as the condition, and two lists of statements for the true and false branches.
While Statement: Represents a loop that executes a block of statements as long as a condition is true.
Print Statement: Represents a statement that outputs the value of an expression.
Lvalues:

An lvalue represents the left-hand side of an assignment or a reference to a variable.
The lvalue types include:
Variable Lvalue: Represents a reference to a variable by its name.
The AST provides a structured representation of the program, allowing the compiler or interpreter to analyze and process the code more easily. Each node in the AST captures the relevant information and relationships between different parts of the program, enabling further analysis, transformation, or execution of the code.*)
type variable_type =
  | Integer_type
  | Char_type
  | Boolean_type
  | Object_type of string  (* class name *)

type typed_variable = variable_type * string

type class_variable_kind =
  | Static  (** one instance of variable for class *)
  | Field  (** one instance of variable for each object *)

type class_variable = class_variable_kind * typed_variable

type subroutine_type =
  | Constructor_type
  | Function_type
  | Method_type

type subroutine_name =
  | This_call of string  (** method call from [this] pointer *)
  | Other_call of string * string

type keyword_constant =
  | True
  | False
  | Null
  | This

type binary_operator =
  | Plus
  | Minus
  | Multiply
  | Divide
  | Bitwise_and
  | Bitwise_or
  | Less_than
  | Greater_than
  | Equals

type unary_operator =
  | Negative
  | Bitwise_negation

type expression =
  | Integer_constant of int
  | String_constant of string
  | Keyword_constant of keyword_constant
  | Lvalue of lvalue
  | Subroutine_call of subroutine_name * expression list
  | Binary_operator of binary_operator * expression * expression
  | Unary_operator of unary_operator * expression
and lvalue =
  | Variable of string  (** name of variable *)
  | Array_element of string * expression  (** name of array, index into array *)

type statement =
  | Let_statement of lvalue * expression
  (** condition, statements if true, statements if false *)
  | If_statement of expression * statement list * statement list
  | While_statement of expression * statement list
  | Do_statement of subroutine_name * expression list
  | Return_statement of expression option

type subroutine = {
  function_type : subroutine_type ;
  return_type : variable_type option ;
  function_name : string ;
  parameters : typed_variable list ;
  local_variables : typed_variable list ;
  function_body : statement list ;
}

type class_declaration = {
  name : string ;
  class_variables: class_variable list ;
  subroutines : subroutine list ;
}
