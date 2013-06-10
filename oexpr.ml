exception SyntaxError of string
exception VariableNotFound of string

module StringMap = Map.Make(String);;

module type EXPRESSION_EVALUATOR =
sig
  type compiled_expression
  type binding = string * float
  val compile : string -> compiled_expression
  val evaluate : compiled_expression -> binding list -> float
  val compiled_tostring : compiled_expression -> string
end;;

module type EVALUATOR_SETTINGS = 
sig
  type expr_func = float list -> float
  type arity = int
  val functions : (expr_func * arity) StringMap.t
  val precedens: Token.oper -> int
end;;
