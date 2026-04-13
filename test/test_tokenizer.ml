(* ============================================================
   test_tokenizer.ml
   OUnit2 test suite for ProjX v4 Tokenizer
   Compile:
     ocamlfind ocamlopt -package ounit2 -linkpkg \
       tokenizer.ml test_tokenizer.ml -o run_tests
   Run:
     ./run_tests
   ============================================================ *)

open OUnit2
open Tokenizer

(* ── helpers ─────────────────────────────────────────────── *)

(** Lex a source string into a token list (strips trailing END). *)
let lex src =
  let chars = List.init (String.length src) (String.get src) in
  tokenize chars

(** Return only the token kinds from a token list. *)
let kinds toks = List.map (fun t -> t.kind) toks

(** Return only the text fields. *)
let texts toks = List.map (fun t -> t.text) toks

(** Return only the lit_val fields. *)
let lits toks = List.map (fun t -> t.lit_val) toks

(** Lex and drop the final END token. *)
let lex_no_end src =
  let toks = lex src in
  match List.rev toks with
  | { kind = END; _ } :: rest -> List.rev rest
  | _ -> toks


(* ============================================================
   SECTION 1: TEN DISTINCT DSL PROGRAMS
   Each program is > 5 lines of DSL source.
   ============================================================ *)

(* ── Program 1 : basic projectile block ── *)
let prog1 = {|
projectile cannon {
  angle = 45
  speed = 100
  mass  = 2.5
  launch_from = (0, 0, 0)
}
|}

(* ── Program 2 : simulate block with air resistance ── *)
let prog2 = {|
simulate env {
  gravity          = 9.81
  air_resistance   = true
  air_density      = 1.225
  wind_x           = 3.0
  wind_y           = 0.0
  wind_z           = -1.5
  drag_coefficient = 0.47
  cross_section    = 0.005
}
|}

(* ── Program 3 : fork / branch ── *)
let prog3 = {|
fork scenario {
  branch low  { angle = 15  speed = 80  }
  branch mid  { angle = 45  speed = 80  }
  branch high { angle = 75  speed = 80  }
}
|}

(* ── Program 4 : game block ── *)
let prog4 = {|
game demo {
  planet = "Earth"
  level  = 1
  lives  = 3
  plot   = true
}
|}

(* ── Program 5 : let / set / for loop ── *)
let prog5 = {|
let x = 0
set x = x + 1
for i from 1 to 10 step 2 {
  set x = x * i
}
|}

(* ── Program 6 : while / if / else ── *)
let prog6 = {|
let v = 50
while v > 0 {
  if v < 10 {
    set v = 0
  } else {
    set v = v - 5
  }
}
|}

(* ── Program 7 : logical / comparison operators ── *)
let prog7 = {|
if x == 0 and y != 1 {
  plot
}
if a <= 3 or b >= 7 {
  range
}
if not (c < 2) {
  max_height
}
|}

(* ── Program 8 : collision / bounce / restitution ── *)
let prog8 = {|
simulate col {
  collide       = true
  collision_vel = 0.8
  min_dist      = 0.1
  bounce        = true
  times         = 3
  restitution   = 0.9
}
|}

(* ── Program 9 : 3-D azimuth / wind_z / check / tower ── *)
let prog9 = {|
projectile rocket {
  angle         = 30
  angle_azimuth = 90
  speed         = 250
  mass          = 10.0
}
simulate atmo {
  wind_z = 2.5
  check  = true
  tower  = 50
}
|}

(* ── Program 10 : arithmetic expressions & repeat ── *)
let prog10 = {|
let dist = (speed * speed) / gravity
repeat 5 {
  set dist = dist - 1.0
}
max_range = dist
min_vel   = 5.5
|}

(* ── Program 11 : string literals & comments ── *)
let prog11 = {|
# This is a comment
game mission {
  planet = "Mars"          // another comment
  level  = 2
  lives  = 5
}
// end of program
|}

(* ── Program 12 : corner-case – empty body with nested braces ── *)
let prog12 = {|
projectile empty {
}
simulate empty {
}
fork empty {
}
|}


(* ============================================================
   SECTION 2 : UNIT TESTS (OUnit2)
   ============================================================ *)

(* ────────────────────────────────────────────────
   2.1  Tests for  str_tok
   ──────────────────────────────────────────────── *)

let test_str_tok_keywords _ =
  assert_equal "PROJECTILE"       (str_tok PROJECTILE);
  assert_equal "SIMULATE"         (str_tok SIMULATE);
  assert_equal "FORK"             (str_tok FORK);
  assert_equal "GAME"             (str_tok GAME);
  assert_equal "ANGLE"            (str_tok ANGLE);
  assert_equal "SPEED"            (str_tok SPEED);
  assert_equal "GRAVITY"          (str_tok GRAVITY)

let test_str_tok_3d_additions _ =
  assert_equal "ANGLE_AZIMUTH"    (str_tok ANGLE_AZIMUTH);
  assert_equal "WIND_Z"           (str_tok WIND_Z);
  assert_equal "AIR_DENSITY"      (str_tok AIR_DENSITY)

let test_str_tok_operators _ =
  assert_equal "EQ"               (str_tok EQ);
  assert_equal "NEQ"              (str_tok NEQ);
  assert_equal "LEQ"              (str_tok LEQ);
  assert_equal "GEQ"              (str_tok GEQ);
  assert_equal "LESS"             (str_tok LESS);
  assert_equal "MORE"             (str_tok MORE)

let test_str_tok_punctuation _ =
  assert_equal "LEFT_CURL"        (str_tok LEFT_CURL);
  assert_equal "RIGHT_CURL"       (str_tok RIGHT_CURL);
  assert_equal "LEFT_PAR"         (str_tok LEFT_PAR);
  assert_equal "RIGHT_PAR"        (str_tok RIGHT_PAR);
  assert_equal "DOT"              (str_tok DOT);
  assert_equal "COMMA"            (str_tok COMMA)

let test_str_tok_literals _ =
  assert_equal "IDF"              (str_tok IDF);
  assert_equal "STR"              (str_tok STR);
  assert_equal "INT"              (str_tok INT);
  assert_equal "FLOAT"            (str_tok FLOAT);
  assert_equal "END"              (str_tok END)


(* ────────────────────────────────────────────────
   2.2  Tests for  key_id
   ──────────────────────────────────────────────── *)

let test_key_id_block_keywords _ =
  assert_equal (PROJECTILE, "null") (key_id "projectile");
  assert_equal (SIMULATE,   "null") (key_id "simulate");
  assert_equal (FORK,       "null") (key_id "fork");
  assert_equal (GAME,       "null") (key_id "game")

let test_key_id_control_flow _ =
  assert_equal (LET,    "null") (key_id "let");
  assert_equal (SET,    "null") (key_id "set");
  assert_equal (FOR,    "null") (key_id "for");
  assert_equal (FROM,   "null") (key_id "from");
  assert_equal (TO,     "null") (key_id "to");
  assert_equal (STEP,   "null") (key_id "step");
  assert_equal (REPEAT, "null") (key_id "repeat");
  assert_equal (WHILE,  "null") (key_id "while");
  assert_equal (IF,     "null") (key_id "if");
  assert_equal (ELSE,   "null") (key_id "else")

let test_key_id_logical_ops _ =
  assert_equal (AND, "null") (key_id "and");
  assert_equal (OR,  "null") (key_id "or");
  assert_equal (NOT, "null") (key_id "not")

let test_key_id_true_false _ =
  (* "true" and "false" map to IDF with lit_val "null" *)
  assert_equal (IDF, "null") (key_id "true");
  assert_equal (IDF, "null") (key_id "false")

let test_key_id_unknown_identifier _ =
  (* arbitrary identifier: lit_val carries the name itself *)
  assert_equal (IDF, "myVar")   (key_id "myVar");
  assert_equal (IDF, "x")       (key_id "x");
  assert_equal (IDF, "hello")   (key_id "hello")

let test_key_id_3d_keywords _ =
  assert_equal (ANGLE_AZIMUTH, "null") (key_id "angle_azimuth");
  assert_equal (WIND_Z,        "null") (key_id "wind_z");
  assert_equal (AIR_DENSITY,   "null") (key_id "air_density")


(* ────────────────────────────────────────────────
   2.3  Tests for  is_digit
   ──────────────────────────────────────────────── *)

let test_is_digit_true _ =
  assert_bool "0 is digit" (is_digit '0');
  assert_bool "5 is digit" (is_digit '5');
  assert_bool "9 is digit" (is_digit '9')

let test_is_digit_false _ =
  assert_bool "a is not digit" (not (is_digit 'a'));
  assert_bool "_ is not digit" (not (is_digit '_'));
  assert_bool "space is not digit" (not (is_digit ' '))

let test_is_digit_boundary _ =
  (* boundary: char just before '0' and just after '9' *)
  assert_bool "/ is not digit" (not (is_digit '/'));
  assert_bool ": is not digit" (not (is_digit ':'))


(* ────────────────────────────────────────────────
   2.4  Tests for  is_alpha
   ──────────────────────────────────────────────── *)

let test_is_alpha_lowercase _ =
  assert_bool "a" (is_alpha 'a');
  assert_bool "m" (is_alpha 'm');
  assert_bool "z" (is_alpha 'z')

let test_is_alpha_uppercase _ =
  assert_bool "A" (is_alpha 'A');
  assert_bool "M" (is_alpha 'M');
  assert_bool "Z" (is_alpha 'Z')

let test_is_alpha_underscore _ =
  assert_bool "_ is alpha" (is_alpha '_')

let test_is_alpha_false _ =
  assert_bool "0 is not alpha" (not (is_alpha '0'));
  assert_bool "! is not alpha" (not (is_alpha '!'));
  assert_bool ". is not alpha" (not (is_alpha '.'))


(* ────────────────────────────────────────────────
   2.5  Tests for  is_alnum
   ──────────────────────────────────────────────── *)

let test_is_alnum_alpha _ =
  assert_bool "a" (is_alnum 'a');
  assert_bool "Z" (is_alnum 'Z');
  assert_bool "_" (is_alnum '_')

let test_is_alnum_digit _ =
  assert_bool "3" (is_alnum '3');
  assert_bool "0" (is_alnum '0')

let test_is_alnum_false _ =
  assert_bool "+" (not (is_alnum '+'));
  assert_bool " " (not (is_alnum ' '));
  assert_bool "@" (not (is_alnum '@'))


(* ────────────────────────────────────────────────
   2.6  Tests for  make_tok
   ──────────────────────────────────────────────── *)

let test_make_tok_fields _ =
  let t = make_tok INT "42" "42" in
  assert_equal INT  t.kind;
  assert_equal "42" t.text;
  assert_equal "42" t.lit_val

let test_make_tok_keyword _ =
  let t = make_tok GRAVITY "gravity" "null" in
  assert_equal GRAVITY   t.kind;
  assert_equal "gravity" t.text;
  assert_equal "null"    t.lit_val

let test_make_tok_operator _ =
  let t = make_tok EQ "==" "null" in
  assert_equal EQ     t.kind;
  assert_equal "=="   t.text;
  assert_equal "null" t.lit_val


(* ────────────────────────────────────────────────
   2.7  Tests for  tokenize  – general
   ──────────────────────────────────────────────── *)

let test_tokenize_empty_string _ =
  let toks = lex "" in
  assert_equal [ END ] (kinds toks)

let test_tokenize_only_whitespace _ =
  let toks = lex "   \t\n\r  " in
  assert_equal [ END ] (kinds toks)

let test_tokenize_single_int _ =
  let toks = lex_no_end "42" in
  assert_equal [ INT ] (kinds toks);
  assert_equal [ "42" ] (texts toks);
  assert_equal [ "42" ] (lits  toks)

let test_tokenize_single_float _ =
  let toks = lex_no_end "3.14" in
  assert_equal [ FLOAT ] (kinds toks);
  assert_equal [ "3.14" ] (texts toks)

let test_tokenize_float_not_int _ =
  (* Ensure 3.14 is FLOAT, not INT followed by DOT *)
  let toks = lex_no_end "3.14" in
  assert_equal 1 (List.length toks);
  assert_equal FLOAT (List.hd toks).kind

let test_tokenize_integer_no_float _ =
  (* "3." has no digit after dot → should yield INT "3", DOT "." *)
  let toks = lex_no_end "3." in
  assert_equal [ INT; DOT ] (kinds toks)

let test_tokenize_string_literal _ =
  let toks = lex_no_end {|"hello world"|} in
  assert_equal [ STR ] (kinds toks);
  let t = List.hd toks in
  assert_equal "\"hello world\"" t.text;
  assert_equal "hello world"     t.lit_val

let test_tokenize_empty_string_literal _ =
  let toks = lex_no_end {|""|} in
  assert_equal [ STR ] (kinds toks);
  assert_equal "\"\""  (List.hd toks).text;
  assert_equal ""      (List.hd toks).lit_val

let test_tokenize_unterminated_string _ =
  assert_raises
    (Failure "Lex error: unterminated string")
    (fun () -> lex {|"oops|})

let test_tokenize_unexpected_char _ =
  assert_raises
    (Failure "Lex error: unexpected character '@'")
    (fun () -> lex "@")

(* ────────────────────────────────────────────────
   2.8  Tests for  tokenize  – comments
   ──────────────────────────────────────────────── *)

let test_tokenize_hash_comment _ =
  let toks = lex_no_end "# entire line is comment\n42" in
  assert_equal [ INT ] (kinds toks)

let test_tokenize_slash_comment _ =
  let toks = lex_no_end "// entire line\nspeed" in
  assert_equal [ SPEED ] (kinds toks)

let test_tokenize_comment_at_end _ =
  (* Comment at very end (no newline): tokenizer should reach [] → END *)
  let toks = lex "x // no newline" in
  assert_equal [ IDF; END ] (kinds toks)

let test_tokenize_hash_inline _ =
  let toks = lex_no_end "gravity # ignore rest\nair_density" in
  assert_equal [ GRAVITY; AIR_DENSITY ] (kinds toks)

(* ────────────────────────────────────────────────
   2.9  Tests for  tokenize  – operators
   ──────────────────────────────────────────────── *)

let test_tokenize_two_char_ops _ =
  let toks = lex_no_end "== != <= >=" in
  assert_equal [ EQ; NEQ; LEQ; GEQ ] (kinds toks)

let test_tokenize_single_char_ops _ =
  let toks = lex_no_end "= < > + - * /" in
  assert_equal [ ASSIGN; LESS; MORE; PLUS; MINUS; STAR; SLASH ] (kinds toks)

let test_tokenize_op_priority _ =
  (* "<=" must not be lexed as "<" then "=" *)
  let toks = lex_no_end "<=" in
  assert_equal [ LEQ ] (kinds toks);
  assert_equal [ "<=" ] (texts toks)

let test_tokenize_op_eq_vs_assign _ =
  let toks = lex_no_end "==" in
  assert_equal [ EQ ] (kinds toks);
  let toks2 = lex_no_end "=" in
  assert_equal [ ASSIGN ] (kinds toks2)

(* ────────────────────────────────────────────────
   2.10  Tests for  tokenize  – punctuation
   ──────────────────────────────────────────────── *)

let test_tokenize_punctuation _ =
  let toks = lex_no_end "{ } ( ) . ," in
  assert_equal
    [ LEFT_CURL; RIGHT_CURL; LEFT_PAR; RIGHT_PAR; DOT; COMMA ]
    (kinds toks)

let test_tokenize_punctuation_text _ =
  let toks = lex_no_end "{" in
  assert_equal "{" (List.hd toks).text

(* ────────────────────────────────────────────────
   2.11  Tests for  tokenize  – DSL programs
   ──────────────────────────────────────────────── *)

(* Program 1 – basic projectile block *)
let test_prog1_starts _ =
  let toks = lex_no_end prog1 in
  (* first real token should be PROJECTILE *)
  assert_equal PROJECTILE (List.hd toks).kind

let test_prog1_has_angle _ =
  let toks = lex_no_end prog1 in
  assert_bool "contains ANGLE"
    (List.exists (fun t -> t.kind = ANGLE) toks)

let test_prog1_float_mass _ =
  let toks = lex_no_end prog1 in
  (* mass = 2.5 → FLOAT with text "2.5" *)
  assert_bool "contains FLOAT 2.5"
    (List.exists (fun t -> t.kind = FLOAT && t.text = "2.5") toks)

(* Program 2 – simulate block *)
let test_prog2_gravity_float _ =
  let toks = lex_no_end prog2 in
  assert_bool "9.81 present"
    (List.exists (fun t -> t.kind = FLOAT && t.text = "9.81") toks)

let test_prog2_wind_z _ =
  let toks = lex_no_end prog2 in
  assert_bool "WIND_Z present"
    (List.exists (fun t -> t.kind = WIND_Z) toks)

let test_prog2_neg_float _ =
  (* -1.5 is lexed as MINUS then FLOAT "1.5" *)
  let toks = lex_no_end prog2 in
  assert_bool "MINUS present"
    (List.exists (fun t -> t.kind = MINUS) toks);
  assert_bool "1.5 present"
    (List.exists (fun t -> t.kind = FLOAT && t.text = "1.5") toks)

(* Program 3 – fork / branch *)
let test_prog3_fork _ =
  let toks = lex_no_end prog3 in
  assert_equal FORK (List.hd toks).kind

let test_prog3_branch_count _ =
  let toks = lex_no_end prog3 in
  let n = List.length (List.filter (fun t -> t.kind = BRANCH) toks) in
  assert_equal 3 n

(* Program 4 – game block *)
let test_prog4_game _ =
  let toks = lex_no_end prog4 in
  assert_equal GAME (List.hd toks).kind

let test_prog4_string_earth _ =
  let toks = lex_no_end prog4 in
  assert_bool "STR Earth"
    (List.exists (fun t -> t.kind = STR && t.lit_val = "Earth") toks)

(* Program 5 – let / for *)
let test_prog5_let_for _ =
  let toks = lex_no_end prog5 in
  assert_bool "LET present"  (List.exists (fun t -> t.kind = LET)  toks);
  assert_bool "FOR present"  (List.exists (fun t -> t.kind = FOR)  toks);
  assert_bool "FROM present" (List.exists (fun t -> t.kind = FROM) toks);
  assert_bool "TO present"   (List.exists (fun t -> t.kind = TO)   toks);
  assert_bool "STEP present" (List.exists (fun t -> t.kind = STEP) toks)

let test_prog5_int_literals _ =
  let toks = lex_no_end prog5 in
  assert_bool "int 10 present"
    (List.exists (fun t -> t.kind = INT && t.text = "10") toks)

(* Program 6 – while / if / else *)
let test_prog6_while _ =
  let toks = lex_no_end prog6 in
  assert_bool "WHILE" (List.exists (fun t -> t.kind = WHILE) toks)

let test_prog6_if_else _ =
  let toks = lex_no_end prog6 in
  assert_bool "IF"   (List.exists (fun t -> t.kind = IF)   toks);
  assert_bool "ELSE" (List.exists (fun t -> t.kind = ELSE) toks)

(* Program 7 – logical / comparison operators *)
let test_prog7_logical _ =
  let toks = lex_no_end prog7 in
  assert_bool "AND" (List.exists (fun t -> t.kind = AND) toks);
  assert_bool "OR"  (List.exists (fun t -> t.kind = OR)  toks);
  assert_bool "NOT" (List.exists (fun t -> t.kind = NOT) toks)

let test_prog7_comparisons _ =
  let toks = lex_no_end prog7 in
  assert_bool "EQ"  (List.exists (fun t -> t.kind = EQ)  toks);
  assert_bool "NEQ" (List.exists (fun t -> t.kind = NEQ) toks);
  assert_bool "LEQ" (List.exists (fun t -> t.kind = LEQ) toks);
  assert_bool "GEQ" (List.exists (fun t -> t.kind = GEQ) toks);
  assert_bool "LESS"(List.exists (fun t -> t.kind = LESS) toks)

(* Program 8 – collision *)
let test_prog8_collide _ =
  let toks = lex_no_end prog8 in
  assert_bool "COLLIDE"       (List.exists (fun t -> t.kind = COLLIDE)       toks);
  assert_bool "BOUNCE"        (List.exists (fun t -> t.kind = BOUNCE)        toks);
  assert_bool "RESTITUTION"   (List.exists (fun t -> t.kind = RESTITUTION)   toks);
  assert_bool "TIMES"         (List.exists (fun t -> t.kind = TIMES)         toks)

(* Program 9 – 3D *)
let test_prog9_azimuth _ =
  let toks = lex_no_end prog9 in
  assert_bool "ANGLE_AZIMUTH"
    (List.exists (fun t -> t.kind = ANGLE_AZIMUTH) toks)

let test_prog9_check_tower _ =
  let toks = lex_no_end prog9 in
  assert_bool "CHECK" (List.exists (fun t -> t.kind = CHECK) toks);
  assert_bool "TOWER" (List.exists (fun t -> t.kind = TOWER) toks)

(* Program 10 – arithmetic & repeat *)
let test_prog10_repeat _ =
  let toks = lex_no_end prog10 in
  assert_bool "REPEAT" (List.exists (fun t -> t.kind = REPEAT) toks)

let test_prog10_arithmetic_ops _ =
  let toks = lex_no_end prog10 in
  assert_bool "STAR"  (List.exists (fun t -> t.kind = STAR)  toks);
  assert_bool "SLASH" (List.exists (fun t -> t.kind = SLASH) toks);
  assert_bool "MINUS" (List.exists (fun t -> t.kind = MINUS) toks)

(* Program 11 – comments stripped *)
let test_prog11_comments_stripped _ =
  let toks = lex_no_end prog11 in
  (* Verify no HASH or SLASH tokens exist from comment chars *)
  List.iter (fun t ->
    assert_bool "no raw # or // tokens"
      (t.kind <> IDF || (t.text <> "#" && t.text <> "//"))
  ) toks

let test_prog11_string_mars _ =
  let toks = lex_no_end prog11 in
  assert_bool "STR Mars"
    (List.exists (fun t -> t.kind = STR && t.lit_val = "Mars") toks)

(* Program 12 – empty bodies *)
let test_prog12_empty_bodies _ =
  let toks = lex_no_end prog12 in
  let n_left  = List.length (List.filter (fun t -> t.kind = LEFT_CURL)  toks) in
  let n_right = List.length (List.filter (fun t -> t.kind = RIGHT_CURL) toks) in
  assert_equal n_left n_right;
  assert_equal 3 n_left

(* ────────────────────────────────────────────────
   2.12  Edge-case / corner-case tests
   ──────────────────────────────────────────────── *)

let test_multidigit_int _ =
  let toks = lex_no_end "12345" in
  assert_equal [ INT ] (kinds toks);
  assert_equal "12345" (List.hd toks).text

let test_float_leading_zero _ =
  let toks = lex_no_end "0.5" in
  assert_equal [ FLOAT ] (kinds toks);
  assert_equal "0.5" (List.hd toks).text

let test_identifier_with_digits _ =
  let toks = lex_no_end "var1" in
  assert_equal [ IDF ] (kinds toks);
  assert_equal "var1" (List.hd toks).text

let test_identifier_starts_underscore _ =
  let toks = lex_no_end "_priv" in
  assert_equal [ IDF ] (kinds toks);
  assert_equal "_priv" (List.hd toks).text

let test_keyword_uppercase_not_recognised _ =
  (* Keywords are lowercase only; uppercase → IDF *)
  let toks = lex_no_end "Projectile" in
  assert_equal [ IDF ] (kinds toks)

let test_adjacent_tokens_no_space _ =
  let toks = lex_no_end "angle=45" in
  assert_equal [ ANGLE; ASSIGN; INT ] (kinds toks)

let test_string_with_spaces _ =
  let toks = lex_no_end {|"hello world"|} in
  assert_equal "hello world" (List.hd toks).lit_val

let test_string_with_numbers _ =
  let toks = lex_no_end {|"level 99"|} in
  assert_equal "level 99" (List.hd toks).lit_val

let test_end_token_always_last _ =
  let toks = lex "angle = 45" in
  assert_equal END (List.hd (List.rev toks)).kind

let test_end_token_text _ =
  let toks = lex "" in
  let e = List.hd toks in
  assert_equal END  e.kind;
  assert_equal ""   e.text;
  assert_equal "null" e.lit_val

let test_zero_is_int _ =
  let toks = lex_no_end "0" in
  assert_equal [ INT ] (kinds toks);
  assert_equal "0" (List.hd toks).text

let test_true_false_are_idf _ =
  let toks = lex_no_end "true false" in
  assert_equal [ IDF; IDF ] (kinds toks)

let test_consecutive_operators _ =
  let toks = lex_no_end "+-*/" in
  assert_equal [ PLUS; MINUS; STAR; SLASH ] (kinds toks)

let test_paren_expr _ =
  let toks = lex_no_end "(x + 1)" in
  assert_equal [ LEFT_PAR; IDF; PLUS; INT; RIGHT_PAR ] (kinds toks)

let test_dot_access _ =
  let toks = lex_no_end "obj.field" in
  assert_equal [ IDF; DOT; IDF ] (kinds toks)

let test_comma_separated _ =
  let toks = lex_no_end "a, b, c" in
  assert_equal [ IDF; COMMA; IDF; COMMA; IDF ] (kinds toks)

let test_neq_not_two_tokens _ =
  let toks = lex_no_end "!=" in
  assert_equal [ NEQ ] (kinds toks)

let test_geq_not_two_tokens _ =
  let toks = lex_no_end ">=" in
  assert_equal [ GEQ ] (kinds toks)

let test_all_wind_directions _ =
  let toks = lex_no_end "wind_x wind_y wind_z" in
  assert_equal [ WIND_X; WIND_Y; WIND_Z ] (kinds toks)

let test_planet_level_lives _ =
  let toks = lex_no_end "planet level lives" in
  assert_equal [ PLANET; LEVEL; LIVES ] (kinds toks)

let test_slash_single_char _ =
  (* A lone '/' that is NOT followed by another '/' must be SLASH *)
  let toks = lex_no_end "10 / 2" in
  assert_equal [ INT; SLASH; INT ] (kinds toks)

let test_comment_does_not_eat_next_line _ =
  let toks = lex_no_end "# comment\ngravity" in
  assert_equal [ GRAVITY ] (kinds toks)

let test_lit_val_keyword_is_null _ =
  let toks = lex_no_end "for" in
  assert_equal "null" (List.hd toks).lit_val

let test_lit_val_idf_carries_name _ =
  let toks = lex_no_end "myAngle" in
  assert_equal "myAngle" (List.hd toks).lit_val

let test_float_lit_val_eq_text _ =
  let toks = lex_no_end "9.81" in
  let t = List.hd toks in
  assert_equal t.text t.lit_val

let test_int_lit_val_eq_text _ =
  let toks = lex_no_end "42" in
  let t = List.hd toks in
  assert_equal t.text t.lit_val

let test_multiple_strings _ =
  let toks = lex_no_end {|"alpha" "beta"|} in
  assert_equal [ STR; STR ] (kinds toks);
  assert_equal "alpha" (List.nth toks 0).lit_val;
  assert_equal "beta"  (List.nth toks 1).lit_val

let test_block_with_assignment _ =
  let src = "projectile p { speed = 80 }" in
  let toks = lex_no_end src in
  assert_equal
    [ PROJECTILE; IDF; LEFT_CURL; SPEED; ASSIGN; INT; RIGHT_CURL ]
    (kinds toks)

(* ============================================================
   SECTION 3 : Test suite assembly
   ============================================================ *)

let suite =
  "ProjX Tokenizer Tests" >::: [

    (* str_tok *)
    "str_tok_keywords"    >:: test_str_tok_keywords;
    "str_tok_3d"          >:: test_str_tok_3d_additions;
    "str_tok_operators"   >:: test_str_tok_operators;
    "str_tok_punctuation" >:: test_str_tok_punctuation;
    "str_tok_literals"    >:: test_str_tok_literals;

    (* key_id *)
    "key_id_blocks"       >:: test_key_id_block_keywords;
    "key_id_control"      >:: test_key_id_control_flow;
    "key_id_logical"      >:: test_key_id_logical_ops;
    "key_id_true_false"   >:: test_key_id_true_false;
    "key_id_unknown"      >:: test_key_id_unknown_identifier;
    "key_id_3d"           >:: test_key_id_3d_keywords;

    (* is_digit *)
    "is_digit_true"       >:: test_is_digit_true;
    "is_digit_false"      >:: test_is_digit_false;
    "is_digit_boundary"   >:: test_is_digit_boundary;

    (* is_alpha *)
    "is_alpha_lower"      >:: test_is_alpha_lowercase;
    "is_alpha_upper"      >:: test_is_alpha_uppercase;
    "is_alpha_underscore" >:: test_is_alpha_underscore;
    "is_alpha_false"      >:: test_is_alpha_false;

    (* is_alnum *)
    "is_alnum_alpha"      >:: test_is_alnum_alpha;
    "is_alnum_digit"      >:: test_is_alnum_digit;
    "is_alnum_false"      >:: test_is_alnum_false;

    (* make_tok *)
    "make_tok_fields"     >:: test_make_tok_fields;
    "make_tok_keyword"    >:: test_make_tok_keyword;
    "make_tok_operator"   >:: test_make_tok_operator;

    (* tokenize – general *)
    "tok_empty"           >:: test_tokenize_empty_string;
    "tok_whitespace"      >:: test_tokenize_only_whitespace;
    "tok_int"             >:: test_tokenize_single_int;
    "tok_float"           >:: test_tokenize_single_float;
    "tok_float_not_int"   >:: test_tokenize_float_not_int;
    "tok_int_dot"         >:: test_tokenize_integer_no_float;
    "tok_string"          >:: test_tokenize_string_literal;
    "tok_empty_str"       >:: test_tokenize_empty_string_literal;
    "tok_unterm_str"      >:: test_tokenize_unterminated_string;
    "tok_bad_char"        >:: test_tokenize_unexpected_char;

    (* tokenize – comments *)
    "tok_hash_comment"    >:: test_tokenize_hash_comment;
    "tok_slash_comment"   >:: test_tokenize_slash_comment;
    "tok_comment_at_end"  >:: test_tokenize_comment_at_end;
    "tok_hash_inline"     >:: test_tokenize_hash_inline;

    (* tokenize – operators *)
    "tok_two_char_ops"    >:: test_tokenize_two_char_ops;
    "tok_one_char_ops"    >:: test_tokenize_single_char_ops;
    "tok_op_priority"     >:: test_tokenize_op_priority;
    "tok_eq_vs_assign"    >:: test_tokenize_op_eq_vs_assign;

    (* tokenize – punctuation *)
    "tok_punct"           >:: test_tokenize_punctuation;
    "tok_punct_text"      >:: test_tokenize_punctuation_text;

    (* DSL Program tests *)
    "prog1_start"         >:: test_prog1_starts;
    "prog1_angle"         >:: test_prog1_has_angle;
    "prog1_float_mass"    >:: test_prog1_float_mass;
    "prog2_gravity"       >:: test_prog2_gravity_float;
    "prog2_wind_z"        >:: test_prog2_wind_z;
    "prog2_neg_float"     >:: test_prog2_neg_float;
    "prog3_fork"          >:: test_prog3_fork;
    "prog3_branches"      >:: test_prog3_branch_count;
    "prog4_game"          >:: test_prog4_game;
    "prog4_string"        >:: test_prog4_string_earth;
    "prog5_let_for"       >:: test_prog5_let_for;
    "prog5_int"           >:: test_prog5_int_literals;
    "prog6_while"         >:: test_prog6_while;
    "prog6_if_else"       >:: test_prog6_if_else;
    "prog7_logical"       >:: test_prog7_logical;
    "prog7_compare"       >:: test_prog7_comparisons;
    "prog8_collide"       >:: test_prog8_collide;
    "prog9_azimuth"       >:: test_prog9_azimuth;
    "prog9_check_tower"   >:: test_prog9_check_tower;
    "prog10_repeat"       >:: test_prog10_repeat;
    "prog10_arith"        >:: test_prog10_arithmetic_ops;
    "prog11_comments"     >:: test_prog11_comments_stripped;
    "prog11_mars"         >:: test_prog11_string_mars;
    "prog12_empty"        >:: test_prog12_empty_bodies;

    (* edge / corner cases *)
    "edge_multidigit"     >:: test_multidigit_int;
    "edge_float_zero"     >:: test_float_leading_zero;
    "edge_idf_digits"     >:: test_identifier_with_digits;
    "edge_idf_underscore" >:: test_identifier_starts_underscore;
    "edge_uppercase_idf"  >:: test_keyword_uppercase_not_recognised;
    "edge_nospace"        >:: test_adjacent_tokens_no_space;
    "edge_str_spaces"     >:: test_string_with_spaces;
    "edge_str_numbers"    >:: test_string_with_numbers;
    "edge_end_last"       >:: test_end_token_always_last;
    "edge_end_text"       >:: test_end_token_text;
    "edge_zero_int"       >:: test_zero_is_int;
    "edge_true_false"     >:: test_true_false_are_idf;
    "edge_consec_ops"     >:: test_consecutive_operators;
    "edge_paren"          >:: test_paren_expr;
    "edge_dot"            >:: test_dot_access;
    "edge_comma"          >:: test_comma_separated;
    "edge_neq"            >:: test_neq_not_two_tokens;
    "edge_geq"            >:: test_geq_not_two_tokens;
    "edge_winds"          >:: test_all_wind_directions;
    "edge_planet_etc"     >:: test_planet_level_lives;
    "edge_slash_single"   >:: test_slash_single_char;
    "edge_comment_line"   >:: test_comment_does_not_eat_next_line;
    "edge_kw_null_lit"    >:: test_lit_val_keyword_is_null;
    "edge_idf_lit_name"   >:: test_lit_val_idf_carries_name;
    "edge_float_lit"      >:: test_float_lit_val_eq_text;
    "edge_int_lit"        >:: test_int_lit_val_eq_text;
    "edge_multi_str"      >:: test_multiple_strings;
    "edge_block_assign"   >:: test_block_with_assignment;
  ]

let () = run_test_tt_main suite