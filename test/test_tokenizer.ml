(* ══════════════════════════════════════════════════════════════════
   Tokenizer Unit Tests
   ══════════════════════════════════════════════════════════════════ *)

open Projx

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Test Helper Functions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let tokenize_string s =
  let chars = My_utils.explode s in
  Tokenizer.tokenize chars

let token_kinds tokens = List.map (fun tok -> tok.Tokenizer.kind) tokens

let print_test_result name passed =
  if passed then Printf.printf "✓ %s\n" name
  else begin
    Printf.printf "✗ %s FAILED\n" name;
    exit 1
  end

let assert_equal expected actual test_name =
  let result = expected = actual in
  if not result then begin
    Printf.printf "Expected: %d tokens, Got: %d tokens\n" (List.length expected)
      (List.length actual)
  end;
  print_test_result test_name result

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Test Cases
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* Test 1: Keywords *)
let test_keywords () =
  let input = "projectile simulate fork game" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.PROJECTILE;
      Tokenizer.SIMULATE;
      Tokenizer.FORK;
      Tokenizer.GAME;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Keywords"

(* Test 2: Identifiers *)
let test_identifiers () =
  let input = "ball arrow my_var x1" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.IDF; Tokenizer.IDF; Tokenizer.IDF; Tokenizer.IDF; Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Identifiers"

(* Test 3: Numbers *)
let test_numbers () =
  let input = "42 3.14 0 0.5" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.INT;
      Tokenizer.FLOAT;
      Tokenizer.INT;
      Tokenizer.FLOAT;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Numbers"

(* Test 4: Operators *)
let test_operators () =
  let input = "+ - * / == != < > <= >=" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.PLUS;
      Tokenizer.MINUS;
      Tokenizer.STAR;
      Tokenizer.SLASH;
      Tokenizer.EQ;
      Tokenizer.NEQ;
      Tokenizer.LESS;
      Tokenizer.MORE;
      Tokenizer.LEQ;
      Tokenizer.GEQ;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Operators"

(* Test 5: Punctuation *)
let test_punctuation () =
  let input = "( ) { } , . =" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.LEFT_PAR;
      Tokenizer.RIGHT_PAR;
      Tokenizer.LEFT_CURL;
      Tokenizer.RIGHT_CURL;
      Tokenizer.COMMA;
      Tokenizer.DOT;
      Tokenizer.ASSIGN;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Punctuation"

(* Test 6: Strings *)
let test_strings () =
  let input = "\"hello\" \"world\"" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected = [ Tokenizer.STR; Tokenizer.STR; Tokenizer.END ] in
  let result = assert_equal expected kinds "String Literals" in
  (* Also check literal values *)
  let tok1 = List.nth tokens 0 in
  let tok2 = List.nth tokens 1 in
  print_test_result "String Value 1" (tok1.lit_val = "hello");
  print_test_result "String Value 2" (tok2.lit_val = "world");
  result

(* Test 7: Comments *)
let test_comments () =
  let input = "projectile // this is a comment\nball" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected = [ Tokenizer.PROJECTILE; Tokenizer.IDF; Tokenizer.END ] in
  assert_equal expected kinds "Comments"

(* Test 8: Air Resistance Keywords *)
let test_air_resistance_keywords () =
  let input =
    "mass drag_coefficient cross_section air_resistance air_density wind_x \
     wind_y"
  in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.MASS;
      Tokenizer.DRAG_COEFFICIENT;
      Tokenizer.CROSS_SECTION;
      Tokenizer.AIR_RESISTANCE;
      Tokenizer.AIR_DENSITY;
      Tokenizer.WIND_X;
      Tokenizer.WIND_Y;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Air Resistance Keywords"

(* Test 9: Complex Expression *)
let test_complex_expression () =
  let input = "let x = 5 + 3 * 2" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.LET;
      Tokenizer.IDF;
      Tokenizer.ASSIGN;
      Tokenizer.INT;
      Tokenizer.PLUS;
      Tokenizer.INT;
      Tokenizer.STAR;
      Tokenizer.INT;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Complex Expression"

(* Test 10: Projectile Block *)
let test_projectile_block () =
  let input = "projectile ball { angle 45 speed 30 }" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.PROJECTILE;
      Tokenizer.IDF;
      Tokenizer.LEFT_CURL;
      Tokenizer.ANGLE;
      Tokenizer.INT;
      Tokenizer.SPEED;
      Tokenizer.INT;
      Tokenizer.RIGHT_CURL;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Projectile Block"

(* Test 11: Simulate Block *)
let test_simulate_block () =
  let input = "simulate { gravity 9.8 plot ball }" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.SIMULATE;
      Tokenizer.LEFT_CURL;
      Tokenizer.GRAVITY;
      Tokenizer.FLOAT;
      Tokenizer.PLOT;
      Tokenizer.IDF;
      Tokenizer.RIGHT_CURL;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Simulate Block"

(* Test 12: Fork Block *)
let test_fork_block () =
  let input = "fork ball { branch \"Earth\" { gravity 9.8 } }" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.FORK;
      Tokenizer.IDF;
      Tokenizer.LEFT_CURL;
      Tokenizer.BRANCH;
      Tokenizer.STR;
      Tokenizer.LEFT_CURL;
      Tokenizer.GRAVITY;
      Tokenizer.FLOAT;
      Tokenizer.RIGHT_CURL;
      Tokenizer.RIGHT_CURL;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Fork Block"

(* Test 13: Game Block *)
let test_game_block () =
  let input = "game { planet earth level 1 lives 3 }" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.GAME;
      Tokenizer.LEFT_CURL;
      Tokenizer.PLANET;
      Tokenizer.IDF;
      Tokenizer.LEVEL;
      Tokenizer.INT;
      Tokenizer.LIVES;
      Tokenizer.INT;
      Tokenizer.RIGHT_CURL;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Game Block"

(* Test 14: Control Flow *)
let test_control_flow () =
  let input = "for if while repeat else" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.FOR;
      Tokenizer.IF;
      Tokenizer.WHILE;
      Tokenizer.REPEAT;
      Tokenizer.ELSE;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Control Flow Keywords"

(* Test 15: Logical Operators *)
let test_logical_operators () =
  let input = "and or not" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [ Tokenizer.AND; Tokenizer.OR; Tokenizer.NOT; Tokenizer.END ]
  in
  assert_equal expected kinds "Logical Operators"

(* Test 16: Whitespace Handling *)
let test_whitespace () =
  let input = "  projectile   ball  " in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected = [ Tokenizer.PROJECTILE; Tokenizer.IDF; Tokenizer.END ] in
  assert_equal expected kinds "Whitespace Handling"

(* Test 17: Multiline *)
let test_multiline () =
  let input = "projectile\nball\n{\nangle\n45\n}" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.PROJECTILE;
      Tokenizer.IDF;
      Tokenizer.LEFT_CURL;
      Tokenizer.ANGLE;
      Tokenizer.INT;
      Tokenizer.RIGHT_CURL;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Multiline Input"

(* Test 18: Negative Numbers (tokenized as MINUS + number) *)
let test_negative_numbers () =
  let input = "-5.0 -10" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.MINUS;
      Tokenizer.FLOAT;
      Tokenizer.MINUS;
      Tokenizer.INT;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Negative Numbers"

(* Test 19: Dot Queries *)
let test_dot_queries () =
  let input = "range.ball() max_height.ball()" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.RANGE;
      Tokenizer.DOT;
      Tokenizer.IDF;
      Tokenizer.LEFT_PAR;
      Tokenizer.RIGHT_PAR;
      Tokenizer.MAX_HEIGHT;
      Tokenizer.DOT;
      Tokenizer.IDF;
      Tokenizer.LEFT_PAR;
      Tokenizer.RIGHT_PAR;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Dot Queries"

(* Test 20: Launch From *)
let test_launch_from () =
  let input = "launch_from (0, 1.5, 0)" in
  let tokens = tokenize_string input in
  let kinds = token_kinds tokens in
  let expected =
    [
      Tokenizer.LAUNCH_FROM;
      Tokenizer.LEFT_PAR;
      Tokenizer.INT;
      Tokenizer.COMMA;
      Tokenizer.FLOAT;
      Tokenizer.COMMA;
      Tokenizer.INT;
      Tokenizer.RIGHT_PAR;
      Tokenizer.END;
    ]
  in
  assert_equal expected kinds "Launch From"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Main Test Runner
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let () =
  Printf.printf "\n╔═══════════════════════════════════════╗\n";
  Printf.printf "║   Tokenizer Unit Tests                ║\n";
  Printf.printf "╚═══════════════════════════════════════╝\n\n";

  test_keywords ();
  test_identifiers ();
  test_numbers ();
  test_operators ();
  test_punctuation ();
  test_strings ();
  test_comments ();
  test_air_resistance_keywords ();
  test_complex_expression ();
  test_projectile_block ();
  test_simulate_block ();
  test_fork_block ();
  test_game_block ();
  test_control_flow ();
  test_logical_operators ();
  test_whitespace ();
  test_multiline ();
  test_negative_numbers ();
  test_dot_queries ();
  test_launch_from ();

  Printf.printf "\n All tokenizer tests passed!\n\n"
