(* ══════════════════════════════════════════════════════════════════
   ProjX v3 - Main Entry Point (Simple Version)
   ══════════════════════════════════════════════════════════════════ *)

open Projx

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Command-line Options
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type mode = 
  | Run
  | Lex
  | Parse
  | Check
  | Pretty

let mode = ref Run
let input_file = ref ""

let usage_msg = 
  "ProjX v3 - Projectile Motion DSL\n\n" ^
  "Usage: projx [options] <input_file>\n\nOptions:"

let speclist = [
  ("--lex",    Arg.Unit (fun () -> mode := Lex),    "  Tokenize and print tokens");
  ("--parse",  Arg.Unit (fun () -> mode := Parse),  "  Parse and print AST");
  ("--check",  Arg.Unit (fun () -> mode := Check),  "  Semantic analysis only");
  ("--pretty", Arg.Unit (fun () -> mode := Pretty), "  Pretty print the code");
  ("--run",    Arg.Unit (fun () -> mode := Run),    "  Execute the program (default)");
]

let anon_fun filename = input_file := filename

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Main Entry Point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let () =
  try
    (* Parse arguments *)
    Arg.parse speclist anon_fun usage_msg;

    (* Validate input *)
    if !input_file = "" then begin
      Printf.eprintf "Error: No input file specified\n";
      Arg.usage speclist usage_msg;
      exit 1
    end;

    if not (Sys.file_exists !input_file) then begin
      Printf.eprintf "Error: File '%s' not found\n" !input_file;
      exit 1
    end;

    (* Read and tokenize *)
    let source = My_utils.read_file !input_file in
    let char_list = My_utils.explode source in
    let tokens = Tokenizer.tokenize char_list in

    (* Execute based on mode *)
    match !mode with
    
    | Lex ->
        Printf.printf "=== TOKENS ===\n";
        Printf.printf "%s" (Tokenizer.print_tokens tokens);
        Printf.printf "\n=== END ===\n"

    | Parse ->
        let ast = Parser.parse tokens in
        Printf.printf "=== AST ===\n";
        Printf.printf "%s\n" (Pretty.print_program ast);
        Printf.printf "=== END ===\n"

    | Check ->
        let ast = Parser.parse tokens in
        Checker.check ast;
        Printf.printf "✓ Semantic analysis passed\n"

    | Pretty ->
        let ast = Parser.parse tokens in
        Checker.check ast;
        Printf.printf "%s\n" (Pretty.print_program ast)

    | Run ->
        let ast = Parser.parse tokens in
        Checker.check ast;
        Printf.printf "╔════════════════════════════════════════╗\n";
        Printf.printf "║   ProjX v3 - Projectile Motion DSL    ║\n";
        Printf.printf "╚════════════════════════════════════════╝\n\n";
        Eval.eval_program ast;
        Printf.printf "\n✓ Program completed successfully\n"

  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Error.ProjxError e ->
      Printf.eprintf "%s\n" (Error.format_error e);
      exit 1
  | Sys_error msg ->
      Printf.eprintf "System Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected Error: %s\n" (Printexc.to_string e);
      exit 1