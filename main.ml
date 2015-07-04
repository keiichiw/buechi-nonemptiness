open Syntax
open Printf
open Find_lasso

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then
    Format.printf "Usage: ./main.native [filename]\n"
  else
    let fname = Sys.argv.(1) in
    let inchan = open_in fname in
    let filebuf = Lexing.from_channel inchan in
    try
      let parse = Parser.main Lexer.token filebuf in
      List.iter print_expr parse;
      Find_lasso.main parse
    with
    | Lexer.LexerError msg  ->
      eprintf "lexer error: %s" msg
    | Parsing.Parse_error  ->
      eprintf "parse error\n"
