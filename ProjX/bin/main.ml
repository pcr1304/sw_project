open ProjX
open My_utils
open Tokenizer
let source = read_file "./input/queries.jp"
 
let () =
  let tokens = tokenize (explode source) in 
  let tokens_as_str = print_tokens tokens in
  print_endline tokens_as_str 