open Projx
open My_utils
open Tokenizer
open Parser
open Pretty
open Error

let source = read_file "./input/queries.px"

let () =
  try
    let tokens = tokenize (explode source) in
    let ast    = parse tokens in
    Checker.check ast;
    print_endline (print_program ast)
  with
  | ProjxError e -> 
      print_endline (format_error e);
      exit 1
  | Failure msg  ->
      print_endline msg;
      exit 1