(* ══════════════════════════════════════════════════════════════════
   Parser Unit Tests
   ══════════════════════════════════════════════════════════════════ *)

open Projx

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Test Helper Functions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse_string s =
  let chars = My_utils.explode s in
  let tokens = Tokenizer.tokenize chars in
  Parser.parse tokens

let print_test_result name passed =
  if passed then
    Printf.printf "✓ %s\n" name
  else begin
    Printf.printf "✗ %s FAILED\n" name;
    exit 1
  end

let assert_parse_success input test_name =
  try
    let _ = parse_string input in
    print_test_result test_name true
  with
  | Failure msg ->
      Printf.printf "Parse failed: %s\n" msg;
      print_test_result test_name false

let assert_parse_fail input test_name =
  try
    let _ = parse_string input in
    print_test_result test_name false
  with
  | Failure _ ->
      print_test_result test_name true

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Test Cases
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* Test 1: Simple Projectile *)
let test_simple_projectile () =
  let input = "projectile ball { angle 45 speed 30 }" in
  assert_parse_success input "Simple Projectile"

(* Test 2: Projectile with Launch From *)
let test_projectile_launch_from () =
  let input = "projectile ball { angle 45 speed 30 launch_from (0, 1.5, 0) }" in
  assert_parse_success input "Projectile with Launch From"

(* Test 3: Projectile with Air Resistance *)
let test_projectile_air_resistance () =
  let input = "projectile bullet { 
    angle 45 
    speed 100 
    mass 0.01 
    drag_coefficient 0.295 
    cross_section 0.0002 
  }" in
  assert_parse_success input "Projectile with Air Resistance"

(* Test 4: Missing Angle - Should Fail *)
let test_missing_angle () =
  let input = "projectile ball { speed 30 }" in
  assert_parse_fail input "Missing Angle (Should Fail)"

(* Test 5: Missing Speed - Should Fail *)
let test_missing_speed () =
  let input = "projectile ball { angle 45 }" in
  assert_parse_fail input "Missing Speed (Should Fail)"

(* Test 6: Simple Simulate *)
let test_simple_simulate () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate { gravity 9.8 plot ball }
  " in
  assert_parse_success input "Simple Simulate"

(* Test 7: Simulate with Air Resistance *)
let test_simulate_air_resistance () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate { 
      gravity 9.8 
      air_resistance true
      air_density 1.225
      wind_x -5.0
      wind_y 0
      plot ball 
    }
  " in
  assert_parse_success input "Simulate with Air Resistance"

(* Test 8: Simulate with Multiple Operations *)
let test_simulate_multiple_ops () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate { 
      gravity 9.8 
      plot ball 
      range ball
      max_height ball
      max_range ball
    }
  " in
  assert_parse_success input "Simulate with Multiple Operations"

(* Test 9: Fork Block *)
let test_fork () =
  let input = "
    projectile ball { angle 45 speed 30 }
    fork ball {
      branch \"Earth\" { gravity 9.8 }
      branch \"Moon\" { gravity 1.62 }
    }
  " in
  assert_parse_success input "Fork Block"

(* Test 10: Game Block *)
let test_game () =
  let input = "
    game { planet earth level 1 lives 3 }
  " in
  assert_parse_success input "Game Block"

(* Test 11: Let Statement *)
let test_let () =
  let input = "let x = 5" in
  assert_parse_success input "Let Statement"

(* Test 12: Set Statement *)
let test_set () =
  let input = "
    let x = 5
    set x = 10
  " in
  assert_parse_success input "Set Statement"

(* Test 13: For Loop *)
let test_for_loop () =
  let input = "
    for i from 1 to 10 step 1 {
      let x = i + 5
    }
  " in
  assert_parse_success input "For Loop"

(* Test 14: While Loop *)
let test_while_loop () =
  let input = "
    let x = 0
    while x < 10 {
      set x = x + 1
    }
  " in
  assert_parse_success input "While Loop"

(* Test 15: Repeat Loop *)
let test_repeat () =
  let input = "
    repeat 5 {
      let x = 10
    }
  " in
  assert_parse_success input "Repeat Loop"

(* Test 16: If Statement *)
let test_if () =
  let input = "
    let x = 5
    if x > 3 {
      let y = 10
    }
  " in
  assert_parse_success input "If Statement"

(* Test 17: If-Else Statement *)
let test_if_else () =
  let input = "
    let x = 5
    if x > 3 {
      let y = 10
    } else {
      let y = 0
    }
  " in
  assert_parse_success input "If-Else Statement"

(* Test 18: Arithmetic Expressions *)
let test_arithmetic () =
  let input = "let x = 5 + 3 * 2 - 1 / 2" in
  assert_parse_success input "Arithmetic Expressions"

(* Test 19: Negative Numbers *)
let test_negative_numbers () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate { 
      gravity 9.8 
      wind_x -5.0
      plot ball 
    }
  " in
  assert_parse_success input "Negative Numbers"

(* Test 20: Parenthesized Expressions *)
let test_parentheses () =
  let input = "let x = (5 + 3) * 2" in
  assert_parse_success input "Parenthesized Expressions"

(* Test 21: Comparison Operators *)
let test_comparisons () =
  let input = "
    let x = 5
    if x == 5 {
      let y = 1
    }
    if x != 3 {
      let z = 2
    }
    if x < 10 {
      let a = 3
    }
    if x > 0 {
      let b = 4
    }
    if x <= 5 {
      let c = 5
    }
    if x >= 5 {
      let d = 6
    }
  " in
  assert_parse_success input "Comparison Operators"

(* Test 22: Logical Operators *)
let test_logical_operators () =
  let input = "
    let x = 5
    let y = 10
    if x > 0 and y < 20 {
      let z = 1
    }
    if x < 0 or y > 5 {
      let w = 2
    }
    if not x == 0 {
      let v = 3
    }
  " in
  assert_parse_success input "Logical Operators"

(* Test 23: Dot Queries *)
let test_dot_queries () =
  let input = "
    projectile ball { angle 45 speed 30 }
    let r = range.ball()
    let mh = max_height.ball()
  " in
  assert_parse_success input "Dot Queries"

(* Test 24: Dot Queries with Gravity *)
let test_dot_queries_gravity () =
  let input = "
    projectile ball { angle 45 speed 30 }
    let r = range.ball(9.8)
  " in
  assert_parse_success input "Dot Queries with Gravity"

(* Test 25: Collision Detection *)
let test_collision () =
  let input = "
    projectile p1 { angle 45 speed 30 }
    projectile p2 { angle 50 speed 25 }
    simulate {
      gravity 9.8
      plot p1
      plot p2
      collide p1 p2
      collision_vel p1 p2
      min_dist p1 p2
    }
  " in
  assert_parse_success input "Collision Detection"

(* Test 26: Bounce *)
let test_bounce () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate {
      gravity 9.8
      plot ball
      bounce ball times 3 restitution 0.7
    }
  " in
  assert_parse_success input "Bounce"

(* Test 27: Min Velocity *)
let test_min_velocity () =
  let input = "
    projectile ball { angle 60 speed 35 }
    simulate {
      gravity 9.8
      plot ball
      min_vel ball tower (50, 20)
    }
  " in
  assert_parse_success input "Min Velocity"

(* Test 28: Check Statement *)
let test_check () =
  let input = "
    projectile ball { angle 45 speed 30 }
    simulate {
      gravity 9.8
      plot ball
      check max_height.ball() > 20
    }
  " in
  assert_parse_success input "Check Statement"

(* Test 29: Complex Program *)
let test_complex_program () =
  let input = "
    let ang = 45
    
    projectile ball {
      angle ang
      speed 30
      launch_from (0, 0, 0)
    }
    
    projectile bullet {
      angle 45
      speed 100
      mass 0.01
      drag_coefficient 0.47
      cross_section 0.0002
    }
    
    simulate {
      gravity 9.8
      plot ball
      range ball
      max_height ball
    }
    
    simulate {
      gravity 9.8
      air_resistance true
      air_density 1.225
      wind_x -5.0
      wind_y 0
      plot bullet
      range bullet
    }
    
    fork ball {
      branch \"Earth\" { gravity 9.8 }
      branch \"Moon\" { gravity 1.62 }
    }
    
    game {
      planet earth
      level 1
      lives 3
    }
  " in
  assert_parse_success input "Complex Program"

(* Test 30: Variable in Projectile *)
let test_variable_in_projectile () =
  let input = "
    let ang = 45
    let spd = 30
    projectile ball {
      angle ang
      speed spd
    }
  " in
  assert_parse_success input "Variable in Projectile"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Main Test Runner
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let () =
  Printf.printf "\n╔═══════════════════════════════════════╗\n";
  Printf.printf "║   Parser Unit Tests                   ║\n";
  Printf.printf "╚═══════════════════════════════════════╝\n\n";

  test_simple_projectile ();
  test_projectile_launch_from ();
  test_projectile_air_resistance ();
  test_missing_angle ();
  test_missing_speed ();
  test_simple_simulate ();
  test_simulate_air_resistance ();
  test_simulate_multiple_ops ();
  test_fork ();
  test_game ();
  test_let ();
  test_set ();
  test_for_loop ();
  test_while_loop ();
  test_repeat ();
  test_if ();
  test_if_else ();
  test_arithmetic ();
  test_negative_numbers ();
  test_parentheses ();
  test_comparisons ();
  test_logical_operators ();
  test_dot_queries ();
  test_dot_queries_gravity ();
  test_collision ();
  test_bounce ();
  test_min_velocity ();
  test_check ();
  test_complex_program ();
  test_variable_in_projectile ();

  Printf.printf "\n All parser tests passed!\n\n"