type oper = Add | Subtract | Divide | Multiply | Exponent
type token = 
    Num of float
  | Op of oper
  | String of string
  | ArgumentSep
  | LeftParen
  | RightParen

val pp_operator : oper -> string
