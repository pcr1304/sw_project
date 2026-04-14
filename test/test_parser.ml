(* =============================================================
   test_parser.ml
   OUnit2 test suite for ProjX v4 Parser (+ tokenizer helpers)

   Compile:
     ocamlfind ocamlopt \
       -package ounit2 -linkpkg \
       tokenizer.ml ast.ml parser.ml test_parser.ml \
       -o run_parser_tests
   Run:
     ./run_parser_tests
   ============================================================= *)

open OUnit2
open Tokenizer
open Ast
open Parser

(* ── pipeline helper: source string → AST ── *)
let parse_src src =
  let chars = List.init (String.length src) (String.get src) in
  let tokens = tokenize chars in
  parse tokens

(* parse exactly one statement *)
let parse_one src =
  match parse_src src with
  | [s] -> s
  | lst ->
      failwith (Printf.sprintf "expected 1 stmt, got %d" (List.length lst))

(* tokenise helper (re-exposed for token-stream tests) *)
let lex src =
  let chars = List.init (String.length src) (String.get src) in
  tokenize chars


(* =============================================================
   SECTION 1 — TEN DISTINCT DSL PROGRAMS  (> 5 lines each)
   ============================================================= *)

(* ── Program 1: minimal projectile ── *)
let prog1 = {|
projectile ball {
  angle 45
  speed 100
}
|}

(* ── Program 2: full projectile with all optional fields ── *)
let prog2 = {|
projectile rocket {
  angle         30
  angle_azimuth 90
  speed         250
  launch_from   (0, 0, 10, 0)
  mass          5.0
  drag_coefficient 0.47
  cross_section 0.008
}
|}

(* ── Program 3: simulate block with physics settings ── *)
let prog3 = {|
simulate {
  gravity        9.81
  air_resistance true
  air_density    1.225
  wind_x         2.0
  wind_y         0.0
  wind_z        -1.5
}
|}

(* ── Program 4: simulate with projectile inside + plot/range ── *)
let prog4 = {|
simulate {
  projectile cannon {
    angle 45
    speed 80
  }
  plot   cannon
  range  cannon
  max_range  cannon
  max_height cannon
}
|}

(* ── Program 5: fork / branch ── *)
let prog5 = {|
fork scenario {
  branch "low"  { gravity 9.81  wind_x 0.0 }
  branch "mid"  { gravity 9.81  wind_x 5.0 }
  branch "high" { gravity 9.81  wind_x 10.0 }
}
|}

(* ── Program 6: game block ── *)
let prog6 = {|
game {
  planet Earth
  level  1
  lives  3
}
|}

(* ── Program 7: let / set / for loop ── *)
let prog7 = {|
let x = 0
set x = x + 1
for i from 1 to 10 step 2 {
  set x = x * i
  set x = x - 1
}
|}

(* ── Program 8: while / if / else ── *)
let prog8 = {|
let v = 50
while v > 0 {
  if v < 10 {
    set v = 0
  } else {
    set v = v - 5
  }
}
|}

(* ── Program 9: simulate with bounce / collision / check ── *)
let prog9 = {|
simulate {
  projectile a { angle 30  speed 100 }
  projectile b { angle 60  speed 80  }
  collide       a b
  collision_vel a b
  min_dist      a b
  bounce a times 3 restitution 0.9
  check a > b
}
|}

(* ── Program 10: repeat + arithmetic expressions ── *)
let prog10 = {|
let dist = 100 * 100 / 9.81
repeat 5 {
  set dist = dist - 10
  set dist = dist + 2
}
|}

(* ── Program 11: nested for inside simulate ── *)
let prog11 = {|
simulate {
  gravity 9.81
  for i from 0 to 5 step 1 {
    wind_x i
    wind_y i
  }
}
|}

(* ── Program 12: if without else (corner case) ── *)
let prog12 = {|
let x = 5
if x == 5 {
  set x = 0
}
let y = x + 1
|}


(* =============================================================
   SECTION 2 — UNIT TESTS
   ============================================================= *)

(* ─────────────────────────────────────────
   2.1  peek
   ───────────────────────────────────────── *)

let test_peek_first_token _ =
  let toks = lex "gravity 9.81" in
  let t = peek toks in
  assert_equal GRAVITY t.kind

let test_peek_empty_returns_end _ =
  let t = peek [] in
  assert_equal END t.kind;
  assert_equal "" t.text

let test_peek_does_not_consume _ =
  let toks = lex "42" in
  let _ = peek toks in
  let _ = peek toks in          (* call twice *)
  let t = peek toks in
  assert_equal INT t.kind       (* still INT *)

(* ─────────────────────────────────────────
   2.2  advance
   ───────────────────────────────────────── *)

let test_advance_moves_forward _ =
  let toks = lex "gravity 9.81" in
  let toks2 = advance toks in
  assert_equal FLOAT (peek toks2).kind

let test_advance_empty_stays_empty _ =
  let result = advance [] in
  assert_equal [] result

let test_advance_single_element _ =
  let toks = lex "42" in          (* INT, END *)
  let toks2 = advance toks in     (* END *)
  assert_equal END (peek toks2).kind

(* ─────────────────────────────────────────
   2.3  expect
   ───────────────────────────────────────── *)

let test_expect_correct_kind _ =
  let toks = lex "{ }" in
  let rest = expect LEFT_CURL toks in
  assert_equal RIGHT_CURL (peek rest).kind

let test_expect_wrong_kind_raises _ =
  let toks = lex "}" in
  assert_raises
    (Failure "Parse Error: expected LEFT_CURL but got '}'")
    (fun () -> expect LEFT_CURL toks)

let test_expect_empty_raises _ =
  assert_raises
    (Failure "Parse Error: expected LEFT_CURL but got end of input")
    (fun () -> expect LEFT_CURL [])

(* ─────────────────────────────────────────
   2.4  expect_idf
   ───────────────────────────────────────── *)

let test_expect_idf_returns_name _ =
  let toks = lex "myVar" in
  let name, _ = expect_idf toks in
  assert_equal "myVar" name

let test_expect_idf_advances _ =
  let toks = lex "myVar 42" in
  let _, rest = expect_idf toks in
  assert_equal INT (peek rest).kind

let test_expect_idf_non_idf_raises _ =
  let toks = lex "42" in
  assert_raises
    (Failure "Parse Error: expected identifier but got '42'")
    (fun () -> expect_idf toks)

let test_expect_idf_empty_raises _ =
  assert_raises
    (Failure "Parse Error: expected identifier but got end of input")
    (fun () -> expect_idf [])

(* ─────────────────────────────────────────
   2.5  expect_str
   ───────────────────────────────────────── *)

let test_expect_str_returns_lit _ =
  let toks = lex {|"hello"|} in
  let s, _ = expect_str toks in
  assert_equal "hello" s

let test_expect_str_advances _ =
  let toks = lex {|"hi" 42|} in
  let _, rest = expect_str toks in
  assert_equal INT (peek rest).kind

let test_expect_str_non_str_raises _ =
  let toks = lex "42" in
  assert_raises
    (Failure "Parse Error: expected string but got '42'")
    (fun () -> expect_str toks)

(* ─────────────────────────────────────────
   2.6  parse_expr / parse_term / parse_factor
   ───────────────────────────────────────── *)

let test_expr_single_int _ =
  let toks = lex "42" in
  let e, _ = parse_expr toks in
  assert_equal (Num 42.0) e

let test_expr_single_float _ =
  let toks = lex "3.14" in
  let e, _ = parse_expr toks in
  assert_equal (Num 3.14) e

let test_expr_negative_number _ =
  let toks = lex "-5" in
  let e, _ = parse_expr toks in
  assert_equal (Num (-5.0)) e

let test_expr_addition _ =
  let toks = lex "1 + 2" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Add, Num 1.0, Num 2.0)) e

let test_expr_subtraction _ =
  let toks = lex "10 - 3" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Sub, Num 10.0, Num 3.0)) e

let test_expr_multiplication _ =
  let toks = lex "4 * 5" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Mul, Num 4.0, Num 5.0)) e

let test_expr_division _ =
  let toks = lex "10 / 2" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Div, Num 10.0, Num 2.0)) e

let test_expr_precedence_mul_before_add _ =
  (* 1 + 2 * 3  →  Add(1, Mul(2,3)) *)
  let toks = lex "1 + 2 * 3" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Add, Num 1.0, Binop (Mul, Num 2.0, Num 3.0))) e

let test_expr_parentheses_override _ =
  (* (1 + 2) * 3  →  Mul(Add(1,2), 3) *)
  let toks = lex "(1 + 2) * 3" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Mul, Binop (Add, Num 1.0, Num 2.0), Num 3.0)) e

let test_expr_variable _ =
  let toks = lex "x" in
  let e, _ = parse_expr toks in
  assert_equal (Var "x") e

let test_expr_var_in_binop _ =
  let toks = lex "x + 1" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Add, Var "x", Num 1.0)) e

let test_expr_left_assoc_add _ =
  (* 1 + 2 + 3  →  Add(Add(1,2), 3)  — left assoc *)
  let toks = lex "1 + 2 + 3" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Add, Binop (Add, Num 1.0, Num 2.0), Num 3.0)) e

let test_expr_unknown_token_raises _ =
  assert_raises
    (Failure "Lex error: unexpected character '@'")
    (fun () -> ignore (parse_expr (lex "@")))
(* ─────────────────────────────────────────
   2.7  parse_cmpop
   ───────────────────────────────────────── *)

let test_cmpop_eq _ =
  let toks = lex "==" in
  let op, _ = parse_cmpop toks in
  assert_equal Eq op

let test_cmpop_neq _ =
  let toks = lex "!=" in
  let op, _ = parse_cmpop toks in
  assert_equal Neq op

let test_cmpop_leq _ =
  let toks = lex "<=" in
  let op, _ = parse_cmpop toks in
  assert_equal Leq op

let test_cmpop_geq _ =
  let toks = lex ">=" in
  let op, _ = parse_cmpop toks in
  assert_equal Geq op

let test_cmpop_lt _ =
  let toks = lex "<" in
  let op, _ = parse_cmpop toks in
  assert_equal Lt op

let test_cmpop_gt _ =
  let toks = lex ">" in
  let op, _ = parse_cmpop toks in
  assert_equal Gt op

let test_cmpop_bad_token_raises _ =
  let toks = lex "+" in
  assert_raises
    (Failure "Parse Error: expected comparison operator but got '+'")
    (fun () -> ignore (parse_cmpop toks))

(* ─────────────────────────────────────────
   2.8  parse_cond
   ───────────────────────────────────────── *)

let test_cond_simple_cmp _ =
  let toks = lex "x > 0" in
  let c, _ = parse_cond toks in
  assert_equal (Cmp (Gt, Var "x", Num 0.0)) c

let test_cond_and _ =
  let toks = lex "x > 0 and y < 5" in
  let c, _ = parse_cond toks in
  assert_equal
    (And (Cmp (Gt, Var "x", Num 0.0), Cmp (Lt, Var "y", Num 5.0)))
    c

let test_cond_or _ =
  let toks = lex "x == 1 or y == 2" in
  let c, _ = parse_cond toks in
  assert_equal
    (Or (Cmp (Eq, Var "x", Num 1.0), Cmp (Eq, Var "y", Num 2.0)))
    c

let test_cond_not _ =
  let toks = lex "not x == 0" in
  let c, _ = parse_cond toks in
  assert_equal (Not (Cmp (Eq, Var "x", Num 0.0))) c

let test_cond_eq _ =
  let toks = lex "a == b" in
  let c, _ = parse_cond toks in
  assert_equal (Cmp (Eq, Var "a", Var "b")) c

let test_cond_neq _ =
  let toks = lex "a != b" in
  let c, _ = parse_cond toks in
  assert_equal (Cmp (Neq, Var "a", Var "b")) c

(* ─────────────────────────────────────────
   2.9  parse_projectile  (top-level)
   ───────────────────────────────────────── *)

let test_projectile_minimal _ =
  match parse_one prog1 with
  | Projectile p ->
      assert_equal "ball"      p.name;
      assert_equal (Num 45.0)  p.angle;
      assert_equal (Num 100.0) p.speed;
      assert_equal None        p.azimuth;
      assert_equal None        p.mass
  | _ -> assert_failure "expected Projectile"

let test_projectile_full _ =
  match parse_one prog2 with
  | Projectile p ->
      assert_equal "rocket"        p.name;
      assert_equal (Num 30.0)      p.angle;
      assert_equal (Some (Num 90.0)) p.azimuth;
      assert_equal (Num 250.0)     p.speed;
      assert_equal (Some (Num 5.0)) p.mass;
      assert_bool  "drag set"      (p.drag_coeff <> None);
      assert_bool  "cs set"        (p.cross_section <> None)
  | _ -> assert_failure "expected Projectile"

let test_projectile_launch_from_4args _ =
  match parse_one prog2 with
  | Projectile p ->
      (match p.launch_from with
       | Some (Num 0.0, Num 0.0, Num 10.0, Num 0.0) -> ()
       | _ -> assert_failure "wrong launch_from")
  | _ -> assert_failure "expected Projectile"

let test_projectile_missing_angle_raises _ =
  assert_raises
    (Failure "Parse Error: projectile 'x' missing angle")
    (fun () -> parse_src "projectile x { speed 50 }")

let test_projectile_missing_speed_raises _ =
  assert_raises
    (Failure "Parse Error: projectile 'x' missing speed")
    (fun () -> parse_src "projectile x { angle 45 }")

(* ─────────────────────────────────────────
   2.10  parse_sim_stmts / parse_sim_stmt
   ───────────────────────────────────────── *)

let test_simulate_gravity _ =
  match parse_one prog3 with
  | Simulate stmts ->
      assert_bool "SGravity present"
        (List.exists (function SGravity (Num 9.81) -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_air_resistance_true _ =
  match parse_one prog3 with
  | Simulate stmts ->
      assert_bool "SAirResistance true"
        (List.exists (function SAirResistance true -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_wind_z _ =
  match parse_one prog3 with
  | Simulate stmts ->
      (* wind_z -1.5 → SWindZ (Num -1.5) *)
      assert_bool "SWindZ present"
        (List.exists (function SWindZ _ -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_plot_range _ =
  match parse_one prog4 with
  | Simulate stmts ->
      assert_bool "SPlot" (List.exists (function SPlot "cannon" -> true | _ -> false) stmts);
      assert_bool "SRange" (List.exists (function SRange "cannon" -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_max_range_max_height _ =
  match parse_one prog4 with
  | Simulate stmts ->
      assert_bool "SMaxRange"
        (List.exists (function SMaxRange "cannon" -> true | _ -> false) stmts);
      assert_bool "SMaxHeight"
        (List.exists (function SMaxHeight "cannon" -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_bounce _ =
  match parse_one prog9 with
  | Simulate stmts ->
      assert_bool "SBounce"
        (List.exists (function SBounce ("a", Num 3.0, Num 0.9) -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_collide _ =
  match parse_one prog9 with
  | Simulate stmts ->
      assert_bool "SCollide"
        (List.exists (function SCollide ("a","b") -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_check _ =
  match parse_one prog9 with
  | Simulate stmts ->
      assert_bool "SCheck"
        (List.exists (function SCheck _ -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_for _ =
  match parse_one prog11 with
  | Simulate stmts ->
      assert_bool "SFor"
        (List.exists (function SFor _ -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate"

let test_simulate_unclosed_raises _ =
  assert_raises
    (Failure "Parse Error: unclosed simulate block")
    (fun () -> parse_src "simulate { gravity 9.81")

(* ─────────────────────────────────────────
   2.11  parse_fork
   ───────────────────────────────────────── *)

let test_fork_name _ =
  match parse_one prog5 with
  | Fork (name, _) -> assert_equal "scenario" name
  | _ -> assert_failure "expected Fork"

let test_fork_branch_count _ =
  match parse_one prog5 with
  | Fork (_, branches) -> assert_equal 3 (List.length branches)
  | _ -> assert_failure "expected Fork"

let test_fork_branch_labels _ =
  match parse_one prog5 with
  | Fork (_, branches) ->
      let labels = List.map (fun b -> b.label) branches in
      assert_equal ["low"; "mid"; "high"] labels
  | _ -> assert_failure "expected Fork"

let test_fork_empty_branches _ =
  let src = {|fork empty { }|} in
  match parse_one src with
  | Fork ("empty", []) -> ()
  | _ -> assert_failure "expected empty fork"

(* ─────────────────────────────────────────
   2.12  parse_game
   ───────────────────────────────────────── *)

let test_game_planet _ =
  match parse_one prog6 with
  | Game g -> assert_equal "Earth" g.planet
  | _ -> assert_failure "expected Game"

let test_game_level _ =
  match parse_one prog6 with
  | Game g -> assert_equal (Num 1.0) g.level
  | _ -> assert_failure "expected Game"

let test_game_lives _ =
  match parse_one prog6 with
  | Game g -> assert_equal (Num 3.0) g.lives
  | _ -> assert_failure "expected Game"

(* ─────────────────────────────────────────
   2.13  Let / Set
   ───────────────────────────────────────── *)

let test_let_simple _ =
  match parse_one "let x = 42" with
  | Let ("x", Num 42.0) -> ()
  | _ -> assert_failure "expected Let"

let test_let_expr _ =
  match parse_one "let d = 2 * 3" with
  | Let ("d", Binop (Mul, Num 2.0, Num 3.0)) -> ()
  | _ -> assert_failure "expected Let with Binop"

let test_set_simple _ =
  match parse_one "set x = 0" with
  | Set ("x", Num 0.0) -> ()
  | _ -> assert_failure "expected Set"

let test_set_with_var _ =
  match parse_one "set x = x + 1" with
  | Set ("x", Binop (Add, Var "x", Num 1.0)) -> ()
  | _ -> assert_failure "expected Set with Var"

(* ─────────────────────────────────────────
   2.14  For / Repeat / While
   ───────────────────────────────────────── *)

let test_for_basic _ =
  let prog = parse_src prog7 in
  assert_bool "For present"
    (List.exists (function For _ -> true | _ -> false) prog)

let test_for_fields _ =
  match parse_one "for i from 1 to 10 step 2 { set i = i }" with
  | For ("i", Num 1.0, Num 10.0, Num 2.0, _) -> ()
  | _ -> assert_failure "For fields wrong"

let test_for_body _ =
  match parse_one "for i from 1 to 5 step 1 { let x = i }" with
  | For (_, _, _, _, [Let ("x", Var "i")]) -> ()
  | _ -> assert_failure "For body wrong"

let test_repeat_count _ =
  match parse_one "repeat 5 { set x = 0 }" with
  | Repeat (Num 5.0, _) -> ()
  | _ -> assert_failure "Repeat count wrong"

let test_repeat_body _ =
  match parse_one "repeat 3 { set v = v - 1 }" with
  | Repeat (_, [Set ("v", Binop (Sub, Var "v", Num 1.0))]) -> ()
  | _ -> assert_failure "Repeat body wrong"

let test_repeat_zero _ =
  match parse_one "repeat 0 { set x = 1 }" with
  | Repeat (Num 0.0, _) -> ()
  | _ -> assert_failure "Repeat zero"

let test_while_condition _ =
  match parse_one "while v > 0 { set v = v - 1 }" with
  | While (Cmp (Gt, Var "v", Num 0.0), _) -> ()
  | _ -> assert_failure "While condition wrong"

let test_while_body _ =
  match parse_one "while v > 0 { set v = v - 1 }" with
  | While (_, [Set ("v", Binop (Sub, Var "v", Num 1.0))]) -> ()
  | _ -> assert_failure "While body wrong"

let test_while_complex_cond _ =
  match parse_one "while x > 0 and y < 10 { set x = x - 1 }" with
  | While (And (Cmp (Gt,_,_), Cmp (Lt,_,_)), _) -> ()
  | _ -> assert_failure "While complex cond"

(* ─────────────────────────────────────────
   2.15  IfElse
   ───────────────────────────────────────── *)

let test_if_no_else _ =
  match parse_one "if x == 0 { set x = 1 }" with
  | IfElse (Cmp (Eq, Var "x", Num 0.0), [Set ("x", Num 1.0)], None) -> ()
  | _ -> assert_failure "IfElse no else"

let test_if_with_else _ =
  match parse_one "if x > 0 { set x = 1 } else { set x = 0 }" with
  | IfElse (_, _, Some [Set ("x", Num 0.0)]) -> ()
  | _ -> assert_failure "IfElse with else"

let test_if_cond_not _ =
  match parse_one "if not x == 0 { set x = 1 }" with
  | IfElse (Not (Cmp (Eq, Var "x", Num 0.0)), _, _) -> ()
  | _ -> assert_failure "IfElse Not"

(* ─────────────────────────────────────────
   2.16  parse  (entry point / program-level)
   ───────────────────────────────────────── *)

let test_parse_empty_program _ =
  let prog = parse_src "" in
  assert_equal [] prog

let test_parse_multiple_stmts _ =
  let prog = parse_src prog7 in
  assert_equal 3 (List.length prog)   (* let + set + for *)

let test_parse_trailing_token_raises _ =
  assert_raises
    (Failure "Parse Error: unexpected token '}' after end of program")
    (fun () -> parse_src "let x = 1 }")

let test_parse_prog12_stmt_count _ =
  let prog = parse_src prog12 in
  (* let x, if, let y  →  3 stmts *)
  assert_equal 3 (List.length prog)

(* ─────────────────────────────────────────
   2.17  DSL program smoke tests
   ───────────────────────────────────────── *)

let test_prog1_parses _ =
  let prog = parse_src prog1 in
  assert_equal 1 (List.length prog)

let test_prog2_parses _ =
  let prog = parse_src prog2 in
  assert_equal 1 (List.length prog)

let test_prog3_parses _ =
  let prog = parse_src prog3 in
  assert_equal 1 (List.length prog)

let test_prog4_parses _ =
  let prog = parse_src prog4 in
  assert_equal 1 (List.length prog)

let test_prog5_parses _ =
  let prog = parse_src prog5 in
  assert_equal 1 (List.length prog)

let test_prog6_parses _ =
  let prog = parse_src prog6 in
  assert_equal 1 (List.length prog)

let test_prog7_parses _ =
  let prog = parse_src prog7 in
  assert_equal 3 (List.length prog)

let test_prog8_parses _ =
  let prog = parse_src prog8 in
  assert_equal 2 (List.length prog)  (* let + while *)

let test_prog9_parses _ =
  let prog = parse_src prog9 in
  assert_equal 1 (List.length prog)

let test_prog10_parses _ =
  let prog = parse_src prog10 in
  assert_equal 2 (List.length prog)  (* let + repeat *)

let test_prog11_parses _ =
  let prog = parse_src prog11 in
  assert_equal 1 (List.length prog)

let test_prog12_parses _ =
  let prog = parse_src prog12 in
  assert_equal 3 (List.length prog)

(* ─────────────────────────────────────────
   2.18  Corner / error cases
   ───────────────────────────────────────── *)

let test_simulate_air_resistance_false _ =
  match parse_one "simulate { air_resistance false }" with
  | Simulate [SAirResistance false] -> ()
  | _ -> assert_failure "air_resistance false"

let test_simulate_air_resistance_zero _ =
  (* 0 → false *)
  match parse_one "simulate { air_resistance 0 }" with
  | Simulate [SAirResistance false] -> ()
  | _ -> assert_failure "air_resistance 0 → false"

let test_simulate_air_resistance_positive _ =
  (* 1.0 → true *)
  match parse_one "simulate { air_resistance 1.0 }" with
  | Simulate [SAirResistance true] -> ()
  | _ -> assert_failure "air_resistance 1.0 → true"

let test_launch_from_3_args_default_t _ =
  (* 3-arg launch_from: z=e3, t defaults to 0 *)
  match parse_one "projectile p { angle 45  speed 50  launch_from (1, 2, 3) }" with
  | Projectile p ->
      (match p.launch_from with
       | Some (Num 1.0, Num 2.0, Num 3.0, Num 0.0) -> ()
       | _ -> assert_failure "wrong 3-arg launch_from")
  | _ -> assert_failure "expected Projectile"

let test_negative_expr_in_assign _ =
  match parse_one "let v = -9.81" with
  | Let ("v", Num n) -> assert_bool "negative" (n < 0.0)
  | _ -> assert_failure "Let negative"

let test_expr_nested_parens _ =
  let toks = lex "((3 + 4))" in
  let e, _ = parse_expr toks in
  assert_equal (Binop (Add, Num 3.0, Num 4.0)) e

let test_top_level_unknown_raises _ =
  assert_raises
    (Failure "Parse Error: unexpected token '42' at top level")
    (fun () -> parse_src "42")

let test_simulate_unknown_raises _ =
  assert_raises
    (Failure "Parse Error: unexpected token '42' in simulate block")
    (fun () -> parse_src "simulate { 42 }")

let test_fork_bad_token_raises _ =
  assert_raises
    (Failure "Parse Error: expected branch but got '42'")
    (fun () -> parse_src "fork f { 42 }")

let test_cond_chain_and_or _ =
  (* a > 0 and b > 0 or c > 0  →  Or(And(…),…) left-assoc *)
  let toks = lex "a > 0 and b > 0 or c > 0" in
  let c, _ = parse_cond toks in
  (match c with
   | Or (And _, _) -> ()
   | _ -> assert_failure "expected Or(And,_)")

let test_while_nested_if _ =
  let src = {|
    while x > 0 {
      if x == 1 {
        set x = 0
      }
    }
  |} in
  match parse_one src with
  | While (_, [IfElse _]) -> ()
  | _ -> assert_failure "nested if in while"

let test_simulate_repeat_nested _ =
  let prog = parse_src prog10 in
  let repeat_stmt = List.find (function Repeat _ -> true | _ -> false) prog in
  match repeat_stmt with
  | Repeat (Num 5.0, body) ->
      assert_equal 2 (List.length body)
  | _ -> assert_failure "expected Repeat with 2 body stmts"

let test_min_vel_in_simulate _ =
  let src = {|
    simulate {
      projectile p { angle 45  speed 80 }
      min_vel p tower (100, 50)
    }
  |} in
  match parse_one src with
  | Simulate stmts ->
      assert_bool "SMinVel"
        (List.exists (function SMinVel ("p", Num 100.0, Num 50.0) -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate with SMinVel"

let test_collision_vel_in_simulate _ =
  let src = "simulate { projectile a { angle 30 speed 50 } projectile b { angle 60 speed 70 } collision_vel a b }" in
  match parse_one src with
  | Simulate stmts ->
      assert_bool "SCollisionVel"
        (List.exists (function SCollisionVel ("a","b") -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate with SCollisionVel"

let test_max_rect_in_simulate _ =
  let src = "simulate { projectile p { angle 45 speed 100 } max_rectangle p }" in
  match parse_one src with
  | Simulate stmts ->
      assert_bool "SMaxRect"
        (List.exists (function SMaxRect "p" -> true | _ -> false) stmts)
  | _ -> assert_failure "expected Simulate with SMaxRect"

(* =============================================================
   SECTION 3 — Test suite assembly
   ============================================================= *)

let suite =
  "ProjX Parser Tests" >::: [

    (* peek *)
    "peek_first"              >:: test_peek_first_token;
    "peek_empty"              >:: test_peek_empty_returns_end;
    "peek_no_consume"         >:: test_peek_does_not_consume;

    (* advance *)
    "advance_forward"         >:: test_advance_moves_forward;
    "advance_empty"           >:: test_advance_empty_stays_empty;
    "advance_single"          >:: test_advance_single_element;

    (* expect *)
    "expect_ok"               >:: test_expect_correct_kind;
    "expect_wrong"            >:: test_expect_wrong_kind_raises;
    "expect_empty"            >:: test_expect_empty_raises;

    (* expect_idf *)
    "expect_idf_name"         >:: test_expect_idf_returns_name;
    "expect_idf_advances"     >:: test_expect_idf_advances;
    "expect_idf_wrong"        >:: test_expect_idf_non_idf_raises;
    "expect_idf_empty"        >:: test_expect_idf_empty_raises;

    (* expect_str *)
    "expect_str_lit"          >:: test_expect_str_returns_lit;
    "expect_str_advances"     >:: test_expect_str_advances;
    "expect_str_wrong"        >:: test_expect_str_non_str_raises;

    (* parse_expr *)
    "expr_int"                >:: test_expr_single_int;
    "expr_float"              >:: test_expr_single_float;
    "expr_negative"           >:: test_expr_negative_number;
    "expr_add"                >:: test_expr_addition;
    "expr_sub"                >:: test_expr_subtraction;
    "expr_mul"                >:: test_expr_multiplication;
    "expr_div"                >:: test_expr_division;
    "expr_precedence"         >:: test_expr_precedence_mul_before_add;
    "expr_parens"             >:: test_expr_parentheses_override;
    "expr_var"                >:: test_expr_variable;
    "expr_var_binop"          >:: test_expr_var_in_binop;
    "expr_left_assoc"         >:: test_expr_left_assoc_add;
    "expr_bad_token"          >:: test_expr_unknown_token_raises;

    (* parse_cmpop *)
    "cmpop_eq"                >:: test_cmpop_eq;
    "cmpop_neq"               >:: test_cmpop_neq;
    "cmpop_leq"               >:: test_cmpop_leq;
    "cmpop_geq"               >:: test_cmpop_geq;
    "cmpop_lt"                >:: test_cmpop_lt;
    "cmpop_gt"                >:: test_cmpop_gt;
    "cmpop_bad"               >:: test_cmpop_bad_token_raises;

    (* parse_cond *)
    "cond_cmp"                >:: test_cond_simple_cmp;
    "cond_and"                >:: test_cond_and;
    "cond_or"                 >:: test_cond_or;
    "cond_not"                >:: test_cond_not;
    "cond_eq"                 >:: test_cond_eq;
    "cond_neq"                >:: test_cond_neq;

    (* parse_projectile *)
    "proj_minimal"            >:: test_projectile_minimal;
    "proj_full"               >:: test_projectile_full;
    "proj_launch_from"        >:: test_projectile_launch_from_4args;
    "proj_missing_angle"      >:: test_projectile_missing_angle_raises;
    "proj_missing_speed"      >:: test_projectile_missing_speed_raises;

    (* parse_sim_stmts *)
    "sim_gravity"             >:: test_simulate_gravity;
    "sim_air_true"            >:: test_simulate_air_resistance_true;
    "sim_wind_z"              >:: test_simulate_wind_z;
    "sim_plot_range"          >:: test_simulate_plot_range;
    "sim_max_range_height"    >:: test_simulate_max_range_max_height;
    "sim_bounce"              >:: test_simulate_bounce;
    "sim_collide"             >:: test_simulate_collide;
    "sim_check"               >:: test_simulate_check;
    "sim_for"                 >:: test_simulate_for;
    "sim_unclosed"            >:: test_simulate_unclosed_raises;

    (* parse_fork *)
    "fork_name"               >:: test_fork_name;
    "fork_count"              >:: test_fork_branch_count;
    "fork_labels"             >:: test_fork_branch_labels;
    "fork_empty"              >:: test_fork_empty_branches;

    (* parse_game *)
    "game_planet"             >:: test_game_planet;
    "game_level"              >:: test_game_level;
    "game_lives"              >:: test_game_lives;

    (* Let / Set *)
    "let_simple"              >:: test_let_simple;
    "let_expr"                >:: test_let_expr;
    "set_simple"              >:: test_set_simple;
    "set_var"                 >:: test_set_with_var;

    (* For / Repeat / While *)
    "for_present"             >:: test_for_basic;
    "for_fields"              >:: test_for_fields;
    "for_body"                >:: test_for_body;
    "repeat_count"            >:: test_repeat_count;
    "repeat_body"             >:: test_repeat_body;
    "repeat_zero"             >:: test_repeat_zero;
    "while_cond"              >:: test_while_condition;
    "while_body"              >:: test_while_body;
    "while_complex"           >:: test_while_complex_cond;

    (* IfElse *)
    "if_no_else"              >:: test_if_no_else;
    "if_else"                 >:: test_if_with_else;
    "if_not"                  >:: test_if_cond_not;

    (* parse entry point *)
    "parse_empty"             >:: test_parse_empty_program;
    "parse_multi"             >:: test_parse_multiple_stmts;
    "parse_trailing"          >:: test_parse_trailing_token_raises;
    "parse_prog12_count"      >:: test_parse_prog12_stmt_count;

    (* DSL program smoke tests *)
    "prog1_ok"                >:: test_prog1_parses;
    "prog2_ok"                >:: test_prog2_parses;
    "prog3_ok"                >:: test_prog3_parses;
    "prog4_ok"                >:: test_prog4_parses;
    "prog5_ok"                >:: test_prog5_parses;
    "prog6_ok"                >:: test_prog6_parses;
    "prog7_ok"                >:: test_prog7_parses;
    "prog8_ok"                >:: test_prog8_parses;
    "prog9_ok"                >:: test_prog9_parses;
    "prog10_ok"               >:: test_prog10_parses;
    "prog11_ok"               >:: test_prog11_parses;
    "prog12_ok"               >:: test_prog12_parses;

    (* corner / error cases *)
    "air_false"               >:: test_simulate_air_resistance_false;
    "air_zero"                >:: test_simulate_air_resistance_zero;
    "air_positive"            >:: test_simulate_air_resistance_positive;
    "launch_3args"            >:: test_launch_from_3_args_default_t;
    "negative_let"            >:: test_negative_expr_in_assign;
    "nested_parens"           >:: test_expr_nested_parens;
    "toplevel_bad"            >:: test_top_level_unknown_raises;
    "simulate_bad"            >:: test_simulate_unknown_raises;
    "fork_bad"                >:: test_fork_bad_token_raises;
    "cond_chain"              >:: test_cond_chain_and_or;
    "while_nested_if"         >:: test_while_nested_if;
    "simulate_repeat"         >:: test_simulate_repeat_nested;
    "min_vel"                 >:: test_min_vel_in_simulate;
    "collision_vel"           >:: test_collision_vel_in_simulate;
    "max_rect"                >:: test_max_rect_in_simulate;
  ]

let () = run_test_tt_main suite