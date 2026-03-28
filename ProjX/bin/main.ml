open Projx
open My_utils
open Tokenizer
open Parser
open Pretty

let source = read_file "./input/queries.px"

let () =
  let tokens = tokenize (explode source) in
  let ast    = parse tokens in
  Checker.check ast;
  print_endline (print_program ast)