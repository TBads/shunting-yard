open OUnit
open Oexpr

let calc_sin (args:float list) = 
  match args with 
    | [n] -> n +. 42.0
    | _ ->  raise (SyntaxError "Arrity mismatch")

module C : EVALUATOR_SETTINGS = struct
  type expr_func = float list -> float
  type arity = int
  let functions = Oexpr.StringMap.singleton "simon" (calc_sin, 1)
  let precedens o = 
    match o with
      | Token.Add -> 1
      | Token.Subtract -> 1
      | Token.Divide -> 10
      | Token.Multiply -> 10
      | Token.Exponent -> 20
end;;

module ShuntTester = Shunting.Make(C)
open ShuntTester

let compile_and_eval s =
  evaluate (compile s) [] 

let eval_with_bindings e bind = 
  evaluate (compile e) bind 

let test_plain1 _ = 
  let expr = "1+1" in
  assert_equal 2.0 (compile_and_eval expr)

let test_plain2 _ = 
  let expr = "10*(1.5+10/5)" in
  assert_equal 35.0 (compile_and_eval expr)

let test_plain3 _ = 
  let expr = "2^3+1" in
  assert_equal 9.0 (compile_and_eval expr)

let test_var1 _ = 
  let expr = "x+1" in
  assert_equal 9.0 (eval_with_bindings expr [("x",8.0)])

let test_var2 _ = 
  let expr = "x+y" in
  assert_equal 9.0 (eval_with_bindings expr [("x",8.0);("y",1.0)])

let test_func _ =
  let expr = "x+simon(y)" in
  assert_equal 51.0 (eval_with_bindings expr [("x",8.0);("y",1.0)])

let suite = "Shunting Yard Algorithm test" 
  >::: ["test_plain1" >:: test_plain1;
	"test_plain2" >:: test_plain2;
	"test_plain3" >:: test_plain3;
	"test_var1" >:: test_var1;
	"test_var2" >:: test_var2;
	"test_func" >:: test_func] 

let _ =
    run_test_tt_main suite;


