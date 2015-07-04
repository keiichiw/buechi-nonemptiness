open Syntax
open Printf
open Find_lasso

let prefix fname =
  let len = String.length fname in
  let ext = if 2 < len then String.sub fname (len - 4) 4 else "" in
  if ext = ".txt" then
    String.sub fname 0 (len - 4)
  else
    fname

let () =
  let argc = Array.length Sys.argv in
  if argc != 2 then
    Format.printf "Usage: ./main.native [filename]\n"
  else
    let fname = Sys.argv.(1) in
    let inchan = open_in fname in
    let filebuf = Lexing.from_channel inchan in
    try
      let data = Parser.main Lexer.token filebuf in
      Find_lasso.main (prefix fname) data
    with
    | Lexer.LexerError msg  ->
      eprintf "lexer error: %s" msg
    | Parsing.Parse_error  ->
      eprintf "parse error\n"
