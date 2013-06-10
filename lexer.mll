{
  open Token
  exception Eof
}
let digit = ['0' - '9']
let char = ['a'-'z''A'-'Z']
let cd = char | digit
rule token = parse 
  | [' ' '\t']     { token lexbuf }
  | ',' {ArgumentSep}
  | '(' {LeftParen}
  | ')' {RightParen}
  | '+' {Op Add}
  | '-' {Op Subtract}
  | '*' {Op Multiply}
  | '/' {Op Divide}
  | '^' {Op Exponent}
  | digit+ ('.' digit+)? as lxm {Num(float_of_string lxm)}
  | char cd* as lxm {String(lxm)}
  | eof            { raise Eof }
