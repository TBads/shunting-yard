module CalcSettings : Oexpr.EVALUATOR_SETTINGS = struct
  type expr_func = float list -> float
  type arity = int
  let functions = Oexpr.StringMap.empty
  let precedens o = 
    match o with
      | Token.Add -> 1
      | Token.Subtract -> 1
      | Token.Divide -> 10
      | Token.Multiply -> 10
      | Token.Exponent -> 20
end

module Calc = Shunting.Make(CalcSettings)
open Calc
