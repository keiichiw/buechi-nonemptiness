open Syntax
open Printf
open Find_lasso

let out_file fname =
  let len = String.length fname in
  let ext = if 2 < len then String.sub fname (len - 4) 4 else "" in
  if ext = ".txt" then
    Printf.sprintf "%s.dot" (String.sub fname 0 (len - 4))
  else
    Printf.sprintf "%s.dot" fname

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
      List.iter print_expr data;
      let outchan = open_out (out_file fname) in
      Find_lasso.main outchan data
    with
    | Lexer.LexerError msg  ->
      eprintf "lexer error: %s" msg
    | Parsing.Parse_error  ->
      eprintf "parse error\n"
