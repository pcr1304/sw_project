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
  if passed then Printf.printf "✓ %s\n" name
  else begin
    Printf.printf "✗ %s FAILED\n" name;
    exit 1
  end

let assert_parse_success input test_name =
  try
    let _ = parse_string input in
    print_test_result test_name true
  with Failure msg ->
    Printf.printf "Parse failed: %s\n" msg;
    print_test_result test_name false

let assert_parse_fail input test_name =
  try
    let _ = parse_string input in
    print_test_result test_name false
  with Failure _ -> print_test_result test_name true

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
  let input =
    "projectile bullet { \n\
    \    angle 45 \n\
    \    speed 100 \n\
    \    mass 0.01 \n\
    \    drag_coefficient 0.295 \n\
    \    cross_section 0.0002 \n\
    \  }"
  in
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
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate { gravity 9.8 plot ball }\n\
    \  "
  in
  assert_parse_success input "Simple Simulate"

(* Test 7: Simulate with Air Resistance *)
let test_simulate_air_resistance () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate { \n\
    \      gravity 9.8 \n\
    \      air_resistance true\n\
    \      air_density 1.225\n\
    \      wind_x -5.0\n\
    \      wind_y 0\n\
    \      plot ball \n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Simulate with Air Resistance"

(* Test 8: Simulate with Multiple Operations *)
let test_simulate_multiple_ops () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate { \n\
    \      gravity 9.8 \n\
    \      plot ball \n\
    \      range ball\n\
    \      max_height ball\n\
    \      max_range ball\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Simulate with Multiple Operations"

(* Test 9: Fork Block *)
let test_fork () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    fork ball {\n\
    \      branch \"Earth\" { gravity 9.8 }\n\
    \      branch \"Moon\" { gravity 1.62 }\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Fork Block"

(* Test 10: Game Block *)
let test_game () =
  let input = "\n    game { planet earth level 1 lives 3 }\n  " in
  assert_parse_success input "Game Block"

(* Test 11: Let Statement *)
let test_let () =
  let input = "let x = 5" in
  assert_parse_success input "Let Statement"

(* Test 12: Set Statement *)
let test_set () =
  let input = "\n    let x = 5\n    set x = 10\n  " in
  assert_parse_success input "Set Statement"

(* Test 13: For Loop *)
let test_for_loop () =
  let input =
    "\n    for i from 1 to 10 step 1 {\n      let x = i + 5\n    }\n  "
  in
  assert_parse_success input "For Loop"

(* Test 14: While Loop *)
let test_while_loop () =
  let input =
    "\n    let x = 0\n    while x < 10 {\n      set x = x + 1\n    }\n  "
  in
  assert_parse_success input "While Loop"

(* Test 15: Repeat Loop *)
let test_repeat () =
  let input = "\n    repeat 5 {\n      let x = 10\n    }\n  " in
  assert_parse_success input "Repeat Loop"

(* Test 16: If Statement *)
let test_if () =
  let input = "\n    let x = 5\n    if x > 3 {\n      let y = 10\n    }\n  " in
  assert_parse_success input "If Statement"

(* Test 17: If-Else Statement *)
let test_if_else () =
  let input =
    "\n\
    \    let x = 5\n\
    \    if x > 3 {\n\
    \      let y = 10\n\
    \    } else {\n\
    \      let y = 0\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "If-Else Statement"

(* Test 18: Arithmetic Expressions *)
let test_arithmetic () =
  let input = "let x = 5 + 3 * 2 - 1 / 2" in
  assert_parse_success input "Arithmetic Expressions"

(* Test 19: Negative Numbers *)
let test_negative_numbers () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate { \n\
    \      gravity 9.8 \n\
    \      wind_x -5.0\n\
    \      plot ball \n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Negative Numbers"

(* Test 20: Parenthesized Expressions *)
let test_parentheses () =
  let input = "let x = (5 + 3) * 2" in
  assert_parse_success input "Parenthesized Expressions"

(* Test 21: Comparison Operators *)
let test_comparisons () =
  let input =
    "\n\
    \    let x = 5\n\
    \    if x == 5 {\n\
    \      let y = 1\n\
    \    }\n\
    \    if x != 3 {\n\
    \      let z = 2\n\
    \    }\n\
    \    if x < 10 {\n\
    \      let a = 3\n\
    \    }\n\
    \    if x > 0 {\n\
    \      let b = 4\n\
    \    }\n\
    \    if x <= 5 {\n\
    \      let c = 5\n\
    \    }\n\
    \    if x >= 5 {\n\
    \      let d = 6\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Comparison Operators"

(* Test 22: Logical Operators *)
let test_logical_operators () =
  let input =
    "\n\
    \    let x = 5\n\
    \    let y = 10\n\
    \    if x > 0 and y < 20 {\n\
    \      let z = 1\n\
    \    }\n\
    \    if x < 0 or y > 5 {\n\
    \      let w = 2\n\
    \    }\n\
    \    if not x == 0 {\n\
    \      let v = 3\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Logical Operators"

(* Test 23: Dot Queries *)
let test_dot_queries () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    let r = range.ball()\n\
    \    let mh = max_height.ball()\n\
    \  "
  in
  assert_parse_success input "Dot Queries"

(* Test 24: Dot Queries with Gravity *)
let test_dot_queries_gravity () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    let r = range.ball(9.8)\n\
    \  "
  in
  assert_parse_success input "Dot Queries with Gravity"

(* Test 25: Collision Detection *)
let test_collision () =
  let input =
    "\n\
    \    projectile p1 { angle 45 speed 30 }\n\
    \    projectile p2 { angle 50 speed 25 }\n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      plot p1\n\
    \      plot p2\n\
    \      collide p1 p2\n\
    \      collision_vel p1 p2\n\
    \      min_dist p1 p2\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Collision Detection"

(* Test 26: Bounce *)
let test_bounce () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      plot ball\n\
    \      bounce ball times 3 restitution 0.7\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Bounce"

(* Test 27: Min Velocity *)
let test_min_velocity () =
  let input =
    "\n\
    \    projectile ball { angle 60 speed 35 }\n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      plot ball\n\
    \      min_vel ball tower (50, 20)\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Min Velocity"

(* Test 28: Check Statement *)
let test_check () =
  let input =
    "\n\
    \    projectile ball { angle 45 speed 30 }\n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      plot ball\n\
    \      check max_height.ball() > 20\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Check Statement"

(* Test 29: Complex Program *)
let test_complex_program () =
  let input =
    "\n\
    \    let ang = 45\n\
    \    \n\
    \    projectile ball {\n\
    \      angle ang\n\
    \      speed 30\n\
    \      launch_from (0, 0, 0)\n\
    \    }\n\
    \    \n\
    \    projectile bullet {\n\
    \      angle 45\n\
    \      speed 100\n\
    \      mass 0.01\n\
    \      drag_coefficient 0.47\n\
    \      cross_section 0.0002\n\
    \    }\n\
    \    \n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      plot ball\n\
    \      range ball\n\
    \      max_height ball\n\
    \    }\n\
    \    \n\
    \    simulate {\n\
    \      gravity 9.8\n\
    \      air_resistance true\n\
    \      air_density 1.225\n\
    \      wind_x -5.0\n\
    \      wind_y 0\n\
    \      plot bullet\n\
    \      range bullet\n\
    \    }\n\
    \    \n\
    \    fork ball {\n\
    \      branch \"Earth\" { gravity 9.8 }\n\
    \      branch \"Moon\" { gravity 1.62 }\n\
    \    }\n\
    \    \n\
    \    game {\n\
    \      planet earth\n\
    \      level 1\n\
    \      lives 3\n\
    \    }\n\
    \  "
  in
  assert_parse_success input "Complex Program"

(* Test 30: Variable in Projectile *)
let test_variable_in_projectile () =
  let input =
    "\n\
    \    let ang = 45\n\
    \    let spd = 30\n\
    \    projectile ball {\n\
    \      angle ang\n\
    \      speed spd\n\
    \    }\n\
    \  "
  in
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
