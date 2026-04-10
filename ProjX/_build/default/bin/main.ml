open Projx
open My_utils
open Tokenizer
open Parser
open Pretty

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let source = read_file "./input/queries.px"

let () =
  let tokens = tokenize (explode source) in
  let ast    = parse tokens in
  let js     = Emitter.emit_program ast in
  write_file "./frontend/data.js" js;
  print_endline (print_program ast)