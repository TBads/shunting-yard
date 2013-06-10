type oper = Add | Subtract | Divide | Multiply | Exponent
type token = 
    Num of float
  | Op of oper
  | String of string
  | ArgumentSep
  | LeftParen
  | RightParen

let pp_operator o =
  match o with
    | Add -> "+"
    | Subtract -> "-"
    | Divide -> "/"
    | Multiply -> "*"
    | Exponent -> "^"
