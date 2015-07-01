{
  open Parser
  open String
  exception LexerError of string
}


let digit = ['0'-'9']
let space = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z' '_' ]
let ident = alpha (alpha | digit)+

  rule token = parse
  | space
      { token lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; token lexbuf }
  | "->"
      { ARROW }
  | "S0:"
      { S0 }
  | "F:"
      { FINAL }
  | ident as name
      { IDENT name }
  | alpha as a
      { ALPHA (String.make 1 a) }
  | eof
      { EOF }