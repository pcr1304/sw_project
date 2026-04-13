(* ── ProjX v3 error types ── *)

(* ── error kinds ── *)
type error_kind =
  | LexError
  | ParseError
  | SemanticError

(* ── error record ── *)
type projx_error = {
  kind    : error_kind;
  msg     : string;
  line    : int option;
  col     : int option;
}

(* ── constructors ── *)
let lex_error ?line ?col msg =
  { kind = LexError; msg; line; col }

let parse_error ?line ?col msg =
  { kind = ParseError; msg; line; col }

let semantic_error msg =
  { kind = SemanticError; msg; line = None; col = None }

(* ── kind to string ── *)
let str_kind = function
  | LexError      -> "Lex Error"
  | ParseError    -> "Parse Error"
  | SemanticError -> "Semantic Error"

(* ── format for printing ── *)
let format_error e =
  let location = match (e.line, e.col) with
    | (Some l, Some c) -> Printf.sprintf " at line %d, col %d" l c
    | (Some l, None)   -> Printf.sprintf " at line %d" l
    | _                -> ""
  in
  Printf.sprintf "%s%s: %s" (str_kind e.kind) location e.msg

(* ── raise as OCaml exception ── *)
exception ProjxError of projx_error

let raise_error e = raise (ProjxError e)

let lex_fail ?line ?col msg =
  raise_error (lex_error ?line ?col msg)

let parse_fail ?line ?col msg =
  raise_error (parse_error ?line ?col msg)

let semantic_fail msg =
  raise_error (semantic_error msg)