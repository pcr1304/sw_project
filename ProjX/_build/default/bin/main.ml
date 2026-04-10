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

let open_browser path =
  let cmd =
    match Sys.os_type with
    | "Win32" | "Cygwin" -> Printf.sprintf "start \"\" \"%s\"" path
    | "Apple_os" -> Printf.sprintf "open \"%s\"" path
    | _ -> Printf.sprintf "xdg-open \"%s\"" path
  in
  let _ = Sys.command cmd in ()

let () =
  let tokens = tokenize (explode source) in
  let ast    = parse tokens in
  let js     = Emitter.emit_program ast in
  write_file "./frontend/data.js" js;
  print_endline (print_program ast);
  open_browser ".\\frontend\\index.html"