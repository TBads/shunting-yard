open Oexpr
open Token

module Make (Es : EVALUATOR_SETTINGS) : EXPRESSION_EVALUATOR = struct
  type expr = 
      Number of float 
    | Var of string
    | Function of Es.expr_func * Es.arity
    | Operator of oper

  type compiled_expression = expr list
  type binding = string * float
  let ef = Es.functions
    
  let rec build_rpn lexbuf op_stack =
    try
      let t = Lexer.token lexbuf in
      match t with
	| Num n -> Number (n) :: build_rpn lexbuf op_stack
	| String s -> 
	  if StringMap.mem s ef 
	  then let (a,d) = StringMap.find s ef in (* why do i need this let? *)
	       Function (a,d) :: build_rpn lexbuf op_stack
	  else Var (s) :: build_rpn lexbuf op_stack
	| Op o  -> 
	  let (output,new_stack) = add_operator o op_stack in
	  output @ (build_rpn lexbuf new_stack)
	| LeftParen -> build_rpn lexbuf (t :: op_stack)
	| RightParen -> 
	  let (output, new_stack) = handle_rparen op_stack in
	  output @ (build_rpn lexbuf new_stack)
	| ArgumentSep -> 
	  let (output,new_stack) = handle_seperator op_stack in
	  output @ (build_rpn lexbuf new_stack)
    with Lexer.Eof -> end_game op_stack
  and handle_seperator stack = 
    match stack with 
      | [] ->  raise (SyntaxError "Missing paren")
      | LeftParen :: ss -> ([],ss)
      | (Op o) :: ss -> 
	let (re_out, nstack) = handle_seperator ss in
	(Operator o :: re_out, nstack)
      | _ -> failwith "Symbol not allowed on stack"
  and handle_rparen stack =
    match stack with
      | [] -> raise (SyntaxError "Missing paren")
      | LeftParen :: ss ->
	(match ss with 
	  | String n :: sse -> 
	    if StringMap.mem n ef 
	    then  let (a,d) = StringMap.find n ef in ([Function (a,d)],sse)
	    else ([], ss)
	  | _ -> ([],ss))
      | (Op o) :: ss  -> 
	let (re_out,nstack) = handle_rparen ss in
	(Operator o :: re_out,nstack)
      | _ -> failwith "Symbol not allowed on stack"
  and add_operator op stack  =
    match stack with
      | (Op o) :: ss -> 
	let pre = (Es.precedens op) - (Es.precedens o) in
	if pre < 0 then let (re_out,nstack) = add_operator op ss in 
			(Operator (o) :: re_out,nstack)
	else ([], Op (op) :: stack) 
      | _ -> ([], Op (op) :: stack)
  and end_game stack =
    match stack with 
      | [] -> []
      | s :: ss -> match s with
	  | Op o -> Operator (o) :: end_game ss
	  | _ -> raise (SyntaxError("Shit done fail"))

  let rec get_var name bindings =
    match bindings with 
      | [] -> raise (VariableNotFound name)
      | (n,v) :: vs -> if n = name then v else get_var name vs 

  let do_op op n1 n2 =
    match op with
      | Add -> n2 +. n1
      | Subtract -> n2 -. n1
      | Divide -> n2 /. n1
      | Multiply -> n2 *. n1
      | Exponent -> n2 ** n1
       
  let rec get_args n stack = 
    match (n,stack) with
      | (0, _) -> ([],stack)
      | (n, []) -> failwith "Compiler error"
      | (n, a :: sa) -> 
	let (nr, ns) = get_args (n-1) sa in (a :: nr, ns)
 
  let compile s = build_rpn (Lexing.from_string s) []
  let evaluate expression bindings = 
    let rec stack_evaluator exp stack =
      match exp with 
	| [] -> List.nth stack 0;
	| Number f :: es -> stack_evaluator es (f :: stack)
	| Var v :: es -> stack_evaluator es ((get_var v bindings) :: stack)
	| Function (func,arr)  :: es -> 
	  let (args, new_stack) = get_args arr stack in
	  let res = func args in stack_evaluator es (res :: new_stack)
	| Operator o :: es -> 
	  match stack with 
	    | n1 :: n2 :: sn  -> stack_evaluator es ((do_op o n1 n2) :: sn)
	    | _ -> raise (SyntaxError "Invalid operator usage")
    in
    stack_evaluator expression []

  let rec compiled_tostring c =
    match c with 
      | [] -> ""
      | Number n :: r -> (string_of_float n) ^ " " ^(compiled_tostring r)
      | Var x :: r -> x ^ " " ^ (compiled_tostring r)
      | Operator o :: r -> (pp_operator o) ^ " " ^ (compiled_tostring r)
      | _ -> "NOT YET!"
end
