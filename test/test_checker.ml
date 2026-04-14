(* =============================================================
   test_checker.ml
   OUnit2 test suite for ProjX v4 Semantic Checker (checker.ml)

   Compile & run via dune:
     add to test/dune:
       (test (name test_checker) (libraries projx ounit2))
     then:
       dune test
   ============================================================= *)

open OUnit2
open Ast
open Checker

(* ── convenience: build the env type directly ── *)
let mk_env vars projs = { vars; projectiles = projs }
let empty = empty_env

(* ── run check on a full program; returns () or raises ── *)
let run src_ast = check src_ast

(* simpler version without Str — just check prefix *)
let assert_sem_err msg f =
  try
    f ();
    assert_failure ("Expected semantic error containing: " ^ msg)
  with Failure s ->
    assert_bool
      ("Expected '" ^ msg ^ "' in error: " ^ s)
      (let n = String.length msg in
       let sn = String.length s in
       let found = ref false in
       for i = 0 to sn - n do
         if String.sub s i n = msg then found := true
       done;
       !found)

(* ── shorthand AST constructors ── *)
let num x     = Num x
let var x     = Var x
let add l r   = Binop (Add, l, r)
let sub l r   = Binop (Sub, l, r)
let mul l r   = Binop (Mul, l, r)
let div_ l r  = Binop (Div, l, r)
let cmp_gt l r = Cmp (Gt, l, r)
let cmp_eq l r = Cmp (Eq, l, r)

(* minimal valid projectile record (top-level) *)
let mk_proj ?(azimuth=None) ?(lf=None) ?(mass=None)
            ?(drag=None) ?(cs=None) name angle speed =
  Projectile { name; angle; azimuth; speed;
               launch_from = lf; mass;
               drag_coeff = drag; cross_section = cs }

(* minimal valid simulate block *)
let sim_with stmts =
  Simulate stmts

(* a projectile inside a simulate block *)
let sproj ?(azimuth=None) ?(lf=None) ?(mass=None)
          ?(drag=None) ?(cs=None) name angle speed =
  SProjectile { name; angle; azimuth; speed;
                launch_from = lf; mass;
                drag_coeff = drag; cross_section = cs }

(* minimal valid simulate stmts: one projectile, gravity, plot *)
let minimal_sim_stmts name =
  [ sproj name (num 45.0) (num 100.0)
  ; SGravity (num 9.81)
  ; SPlot name ]


(* =============================================================
   SECTION 1 — TEN DISTINCT DSL PROGRAMS (as AST values)
   Each is a stmt list (program).
   ============================================================= *)

(* Program 1: single projectile, no simulate *)
let prog1 =
  [ mk_proj "ball" (num 45.0) (num 100.0) ]

(* Program 2: projectile with all optional fields *)
let prog2 =
  [ mk_proj 
      ~azimuth:(Some (num 90.0))
      ~lf:(Some (num 0.0, num 0.0, num 10.0, num 0.0))
      ~mass:(Some (num 5.0))
      ~drag:(Some (num 0.47))
      ~cs:(Some (num 0.008))
      "rocket" (num 30.0) (num 250.0) ]

(* Program 3: let + set + for loop *)
let prog3 =
  [ Let ("x", num 0.0)
  ; Set ("x", add (var "x") (num 1.0))
  ; For ("i", num 1.0, num 10.0, num 1.0,
      [ Set ("x", mul (var "x") (var "i")) ]) ]

(* Program 4: while + if/else *)
let prog4 =
  [ Let ("v", num 50.0)
  ; While (cmp_gt (var "v") (num 0.0),
      [ IfElse (Cmp (Lt, var "v", num 10.0),
          [ Set ("v", num 0.0) ],
          Some [ Set ("v", sub (var "v") (num 5.0)) ]) ]) ]

(* Program 5: simulate with gravity + plot *)
let prog5 =
  [ sim_with (minimal_sim_stmts "cannon") ]

(* Program 6: simulate with air resistance + wind *)
let prog6 =
  [ sim_with
      [ sproj "p" (num 45.0) (num 80.0)
      ; SGravity (num 9.81)
      ; SAirResistance true
      ; SAirDensity (num 1.225)
      ; SWindX (num 2.0)
      ; SWindY (num 0.0)
      ; SWindZ (num 1.5)
      ; SPlot "p" ] ]

(* Program 7: simulate with collision / bounce *)
let prog7 =
  [ sim_with
      [ sproj "a" (num 30.0) (num 100.0)
      ; sproj "b" (num 60.0) (num 80.0)
      ; SGravity (num 9.81)
      ; SCollide ("a", "b")
      ; SCollisionVel ("a", "b")
      ; SMinDist ("a", "b")
      ; SBounce ("a", num 3.0, num 0.9)
      ; SPlot "a" ] ]

(* Program 8: simulate with for loop inside *)
let prog8 =
  [ sim_with
      [ sproj "p" (num 45.0) (num 100.0)
      ; SGravity (num 9.81)
      ; SFor ("i", num 0.0, num 5.0, num 1.0,
          [ SWindX (var "i") ])
      ; SPlot "p" ] ]

(* Program 9: game block — valid planet *)
let prog9 =
  [ Game { planet = "earth"; level = num 1.0; lives = num 3.0 } ]

(* Program 10: repeat + check *)
let prog10 =
  [ sim_with
      [ sproj "r" (num 45.0) (num 200.0)
      ; SGravity (num 3.72)
      ; SRepeat (num 5.0,
          [ SCheck (cmp_gt (num 100.0) (num 0.0)) ])
      ; SPlot "r" ] ]

(* Program 11: nested let in for body *)
let prog11 =
  [ Let ("total", num 0.0)
  ; For ("i", num 1.0, num 5.0, num 1.0,
      [ Let ("tmp", var "i")
      ; Set ("total", add (var "total") (var "tmp")) ]) ]

(* Program 12: projectile with azimuth + simulate with check *)
let prog12 =
  [ mk_proj ~azimuth:(Some (num 45.0)) "missile" (num 30.0) (num 300.0)
  ; sim_with
      [ sproj 
          ~azimuth:(Some (num 90.0))
          "missile2" (num 45.0) (num 150.0)
      ; SGravity (num 9.81)
      ; SCheck (And (cmp_gt (num 1.0) (num 0.0),
                     Cmp (Lt, num 0.0, num 1.0)))
      ; SPlot "missile2" ] ]


(* =============================================================
   SECTION 2 — UNIT TESTS
   ============================================================= *)

(* ─────────────────────────────────────────
   2.1  sem_err
   ───────────────────────────────────────── *)

let test_sem_err_raises _ =
  assert_raises
    (Failure "Semantic Error: test message")
    (fun () -> sem_err "test message")

let test_sem_err_prefix _ =
  match (try Some (sem_err "oops") with Failure _ -> None) with
  | Some _ -> assert_failure "should have raised"
  | None -> ()

let test_sem_err_message_preserved _ =
  try sem_err "variable 'x' used before declaration"
  with Failure s ->
    assert_bool "contains message"
      (let needle = "variable 'x' used before declaration" in
       let n = String.length needle and sn = String.length s in
       let found = ref false in
       for i = 0 to sn - n do
         if String.sub s i n = needle then found := true
       done; !found)

(* ─────────────────────────────────────────
   2.2  declare_var
   ───────────────────────────────────────── *)

let test_declare_var_adds _ =
  let env2 = declare_var empty "x" in
  assert_bool "x in vars" (List.mem "x" env2.vars)

let test_declare_var_duplicate_raises _ =
  let env2 = declare_var empty "x" in
  assert_sem_err "already declared"
    (fun () -> ignore (declare_var env2 "x"))

let test_declare_var_multiple _ =
  let e1 = declare_var empty "a" in
  let e2 = declare_var e1 "b" in
  assert_bool "a" (List.mem "a" e2.vars);
  assert_bool "b" (List.mem "b" e2.vars)

let test_declare_var_does_not_touch_projs _ =
  let env2 = declare_var empty "x" in
  assert_equal [] env2.projectiles

(* ─────────────────────────────────────────
   2.3  declare_or_update_proj
   ───────────────────────────────────────── *)

let test_declare_proj_adds _ =
  let env2 = declare_or_update_proj empty "cannon" in
  assert_bool "cannon in projectiles" (List.mem "cannon" env2.projectiles)

let test_declare_proj_duplicate_no_raise _ =
  let e1 = declare_or_update_proj empty "cannon" in
  let e2 = declare_or_update_proj e1 "cannon" in
  (* should not raise, and cannon still present *)
  assert_bool "cannon" (List.mem "cannon" e2.projectiles)

let test_declare_proj_multiple _ =
  let e1 = declare_or_update_proj empty "a" in
  let e2 = declare_or_update_proj e1 "b" in
  assert_bool "a" (List.mem "a" e2.projectiles);
  assert_bool "b" (List.mem "b" e2.projectiles)

let test_declare_proj_does_not_touch_vars _ =
  let env2 = declare_or_update_proj empty "p" in
  assert_equal [] env2.vars

(* ─────────────────────────────────────────
   2.4  check_var
   ───────────────────────────────────────── *)

let test_check_var_ok _ =
  let env = mk_env ["x"] [] in
  check_var env "x"   (* no exception *)

let test_check_var_undeclared_raises _ =
  assert_sem_err "used before declaration"
    (fun () -> check_var empty "x")

let test_check_var_wrong_name_raises _ =
  let env = mk_env ["y"] [] in
  assert_sem_err "used before declaration"
    (fun () -> check_var env "x")

(* ─────────────────────────────────────────
   2.5  check_proj
   ───────────────────────────────────────── *)

let test_check_proj_ok _ =
  let env = mk_env [] ["cannon"] in
  check_proj env "cannon"

let test_check_proj_undeclared_raises _ =
  assert_sem_err "used before declaration"
    (fun () -> check_proj empty "cannon")

let test_check_proj_wrong_name _ =
  let env = mk_env [] ["ball"] in
  assert_sem_err "used before declaration"
    (fun () -> check_proj env "cannon")

(* ─────────────────────────────────────────
   2.6  check_set
   ───────────────────────────────────────── *)

let test_check_set_ok _ =
  let env = mk_env ["v"] [] in
  check_set env "v"

let test_check_set_undeclared_raises _ =
  assert_sem_err "use let first"
    (fun () -> check_set empty "v")

let test_check_set_different_var_raises _ =
  let env = mk_env ["x"] [] in
  assert_sem_err "use let first"
    (fun () -> check_set env "y")

(* ─────────────────────────────────────────
   2.7  check_expr
   ───────────────────────────────────────── *)

let test_check_expr_num _ =
  check_expr empty (num 42.0)

let test_check_expr_var_ok _ =
  let env = mk_env ["x"] [] in
  check_expr env (var "x")

let test_check_expr_var_undeclared _ =
  assert_sem_err "used before declaration"
    (fun () -> check_expr empty (var "x"))

let test_check_expr_binop_ok _ =
  let env = mk_env ["x"] [] in
  check_expr env (add (var "x") (num 1.0))

let test_check_expr_binop_bad_left _ =
  assert_sem_err "used before declaration"
    (fun () -> check_expr empty (add (var "x") (num 1.0)))

let test_check_expr_binop_bad_right _ =
  let env = mk_env ["x"] [] in
  assert_sem_err "used before declaration"
    (fun () -> check_expr env (add (var "x") (var "y")))

let test_check_expr_nested_binop _ =
  let env = mk_env ["a";"b";"c"] [] in
  check_expr env (add (mul (var "a") (var "b")) (var "c"))

let test_check_expr_dotq_ok _ =
  let env = mk_env [] ["p"] in
  check_expr env (DotQ (DotRange ("p", None)))

let test_check_expr_dotq_bad_proj _ =
  assert_sem_err "used before declaration"
    (fun () -> check_expr empty (DotQ (DotRange ("p", None))))

(* ─────────────────────────────────────────
   2.8  check_dot_query
   ───────────────────────────────────────── *)

let test_dot_range_ok _ =
  let env = mk_env [] ["p"] in
  check_dot_query env (DotRange ("p", None))

let test_dot_max_range_ok _ =
  let env = mk_env [] ["p"] in
  check_dot_query env (DotMaxRange ("p", None))

let test_dot_max_height_ok _ =
  let env = mk_env [] ["p"] in
  check_dot_query env (DotMaxHeight ("p", None))

let test_dot_max_rect_ok _ =
  let env = mk_env [] ["p"] in
  check_dot_query env (DotMaxRect ("p", None))

let test_dot_min_vel_ok _ =
  let env = mk_env [] ["p"] in
  check_dot_query env (DotMinVel ("p", num 0.0, num 0.0, None))

let test_dot_collide_ok _ =
  let env = mk_env [] ["a";"b"] in
  check_dot_query env (DotCollide ("a", "b", None))

let test_dot_min_dist_ok _ =
  let env = mk_env [] ["a";"b"] in
  check_dot_query env (DotMinDist ("a", "b", None))

let test_dot_collide_bad_p1 _ =
  let env = mk_env [] ["b"] in
  assert_sem_err "used before declaration"
    (fun () -> check_dot_query env (DotCollide ("a", "b", None)))

let test_dot_collide_bad_p2 _ =
  let env = mk_env [] ["a"] in
  assert_sem_err "used before declaration"
    (fun () -> check_dot_query env (DotCollide ("a", "b", None)))

(* ─────────────────────────────────────────
   2.9  check_cond
   ───────────────────────────────────────── *)

let test_check_cond_cmp_ok _ =
  let env = mk_env ["x"] [] in
  check_cond env (cmp_gt (var "x") (num 0.0))

let test_check_cond_cmp_bad _ =
  assert_sem_err "used before declaration"
    (fun () -> check_cond empty (cmp_gt (var "x") (num 0.0)))

let test_check_cond_and_ok _ =
  let env = mk_env ["x";"y"] [] in
  check_cond env (And (cmp_gt (var "x") (num 0.0),
                       Cmp (Lt, var "y", num 5.0)))

let test_check_cond_and_bad_right _ =
  let env = mk_env ["x"] [] in
  assert_sem_err "used before declaration"
    (fun () -> check_cond env
      (And (cmp_gt (var "x") (num 0.0),
            Cmp (Lt, var "y", num 5.0))))

let test_check_cond_or_ok _ =
  let env = mk_env ["a";"b"] [] in
  check_cond env (Or (cmp_eq (var "a") (num 1.0),
                      cmp_eq (var "b") (num 2.0)))

let test_check_cond_not_ok _ =
  let env = mk_env ["x"] [] in
  check_cond env (Not (cmp_eq (var "x") (num 0.0)))

let test_check_cond_not_bad _ =
  assert_sem_err "used before declaration"
    (fun () -> check_cond empty (Not (cmp_eq (var "x") (num 0.0))))

let test_check_cond_booldotq_ok _ =
  let env = mk_env [] ["a";"b"] in
  check_cond env (BoolDotQ (DotCollide ("a", "b", None)))

(* ─────────────────────────────────────────
   2.10  check_sim_stmts
   ───────────────────────────────────────── *)

let test_sim_missing_gravity _ =
  assert_sem_err "missing gravity"
    (fun () -> run [ sim_with
      [ sproj "p" (num 45.0) (num 100.0)
      ; SPlot "p" ] ])

let test_sim_double_gravity _ =
  assert_sem_err "more than one gravity"
    (fun () -> run [ sim_with
      [ sproj "p" (num 45.0) (num 100.0)
      ; SGravity (num 9.81)
      ; SGravity (num 1.62)
      ; SPlot "p" ] ])

let test_sim_missing_plot _ =
  assert_sem_err "must have at least one plot"
    (fun () -> run [ sim_with
      [ sproj "p" (num 45.0) (num 100.0)
      ; SGravity (num 9.81) ] ])

let test_sim_valid_passes _ =
  run [ sim_with (minimal_sim_stmts "p") ]

let test_sim_plot_inside_for_counts _ =
  (* plot inside a for should count *)
  run [ sim_with
    [ sproj "p" (num 45.0) (num 100.0)
    ; SGravity (num 9.81)
    ; SFor ("i", num 0.0, num 3.0, num 1.0,
        [ SPlot "p" ]) ] ]

let test_sim_for_loop_scoping _ =
  (* loop var 'i' is visible inside body but not outside *)
  run [ sim_with
    [ sproj "p" (num 45.0) (num 100.0)
    ; SGravity (num 9.81)
    ; SFor ("i", num 0.0, num 5.0, num 1.0,
        [ SWindX (var "i") ])
    ; SPlot "p" ] ]

let test_sim_for_var_not_leaked _ =
  (* 'i' must not be visible after the for loop *)
  assert_sem_err "used before declaration"
    (fun () -> run [ sim_with
      [ sproj "p" (num 45.0) (num 100.0)
      ; SGravity (num 9.81)
      ; SFor ("i", num 0.0, num 3.0, num 1.0, [ SPlot "p" ])
      ; SWindX (var "i") ] ])

let test_sim_proj_undeclared_plot _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ sim_with
      [ SGravity (num 9.81)
      ; SPlot "ghost" ] ])

let test_sim_collide_undeclared _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ sim_with
      [ sproj "a" (num 30.0) (num 100.0)
      ; SGravity (num 9.81)
      ; SCollide ("a", "b")
      ; SPlot "a" ] ])

let test_sim_check_ok _ =
  run [ sim_with
    [ sproj "p" (num 45.0) (num 100.0)
    ; SGravity (num 9.81)
    ; SCheck (cmp_gt (num 1.0) (num 0.0))
    ; SPlot "p" ] ]

(* ─────────────────────────────────────────
   2.11  check_stmt – Projectile (top-level)
   ───────────────────────────────────────── *)

let test_stmt_proj_ok _ =
  run prog1

let test_stmt_proj_full_ok _ =
  run prog2

let test_stmt_proj_bad_angle_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ mk_proj "p" (var "theta") (num 100.0) ])

let test_stmt_proj_bad_speed_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ mk_proj "p" (num 45.0) (var "v") ])

let test_stmt_proj_bad_mass_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ mk_proj ~mass:(Some (var "m"))
                      "p" (num 45.0) (num 100.0) ])

let test_stmt_proj_azimuth_ok _ =
  run [ mk_proj ~azimuth:(Some (num 90.0))
          "p" (num 30.0) (num 200.0) ]

let test_stmt_proj_azimuth_bad_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ mk_proj ~azimuth:(Some (var "az"))
                       "p" (num 30.0) (num 200.0) ])

(* ─────────────────────────────────────────
   2.12  check_stmt – Let / Set
   ───────────────────────────────────────── *)

let test_let_ok _ =
  run [ Let ("x", num 5.0) ]

let test_let_expr_with_var _ =
  run [ Let ("x", num 1.0)
      ; Let ("y", var "x") ]

let test_let_duplicate_raises _ =
  assert_sem_err "already declared"
    (fun () -> run [ Let ("x", num 1.0)
                   ; Let ("x", num 2.0) ])

let test_let_undeclared_rhs _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ Let ("y", var "x") ])

let test_set_ok _ =
  run [ Let ("x", num 0.0)
      ; Set ("x", num 1.0) ]

let test_set_undeclared_raises _ =
  assert_sem_err "use let first"
    (fun () -> run [ Set ("x", num 1.0) ])

let test_set_bad_rhs _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ Let ("x", num 0.0)
                   ; Set ("x", var "y") ])

(* ─────────────────────────────────────────
   2.13  check_stmt – For / Repeat / While
   ───────────────────────────────────────── *)

let test_for_ok _ =
  run prog3

let test_for_loop_var_scoped _ =
  (* loop var visible inside body *)
  run [ For ("i", num 0.0, num 5.0, num 1.0,
          [ Let ("x", var "i") ]) ]

let test_for_loop_var_not_outside _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ For ("i", num 0.0, num 3.0, num 1.0, [])
      ; Let ("x", var "i") ])

let test_for_bad_bound _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ For ("i", var "start", num 10.0, num 1.0, []) ])

let test_repeat_ok _ =
  run [ Let ("x", num 0.0)
      ; Repeat (num 3.0, [ Set ("x", add (var "x") (num 1.0)) ]) ]

let test_repeat_bad_count _ =
  assert_sem_err "used before declaration"
    (fun () -> run [ Repeat (var "n", []) ])

let test_while_ok _ =
  run prog4

let test_while_bad_cond _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ While (cmp_gt (var "v") (num 0.0), []) ])

(* ─────────────────────────────────────────
   2.14  check_stmt – IfElse
   ───────────────────────────────────────── *)

let test_if_no_else_ok _ =
  run [ Let ("x", num 5.0)
      ; IfElse (cmp_gt (var "x") (num 0.0),
          [ Set ("x", num 0.0) ], None) ]

let test_if_with_else_ok _ =
  run [ Let ("x", num 5.0)
      ; IfElse (cmp_gt (var "x") (num 0.0),
          [ Set ("x", num 1.0) ],
          Some [ Set ("x", num 0.0) ]) ]

let test_if_bad_cond _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ IfElse (cmp_gt (var "x") (num 0.0), [], None) ])

let test_if_bad_then_body _ =
  assert_sem_err "variable not declared"
    (fun () -> run
      [ Let ("x", num 1.0)
      ; IfElse (cmp_gt (var "x") (num 0.0),
          [ Set ("y", num 1.0) ], None) ])

let test_if_bad_else_body _ =
  assert_sem_err "use let first"
    (fun () -> run
      [ Let ("x", num 1.0)
      ; IfElse (cmp_gt (var "x") (num 0.0),
          [ Set ("x", num 1.0) ],
          Some [ Set ("z", num 0.0) ]) ])

(* ─────────────────────────────────────────
   2.15  check_stmt – Game
   ───────────────────────────────────────── *)

let test_game_earth_ok _ =
  run [ Game { planet = "earth"; level = num 1.0; lives = num 3.0 } ]

let test_game_moon_ok _ =
  run [ Game { planet = "moon"; level = num 2.0; lives = num 5.0 } ]

let test_game_mars_ok _ =
  run prog9

let test_game_jupiter_ok _ =
  run [ Game { planet = "jupiter"; level = num 1.0; lives = num 3.0 } ]

let test_game_sun_ok _ =
  run [ Game { planet = "sun"; level = num 1.0; lives = num 1.0 } ]

let test_game_invalid_planet _ =
  assert_sem_err "unknown planet"
    (fun () -> run
      [ Game { planet = "venus"; level = num 1.0; lives = num 3.0 } ])

let test_game_invalid_planet_case_sensitive _ =
  (* "Earth" (capital) is not in valid_planets list *)
  assert_sem_err "unknown planet"
    (fun () -> run
      [ Game { planet = "Earth"; level = num 1.0; lives = num 3.0 } ])

let test_game_bad_level_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ Game { planet = "earth"; level = var "lvl"; lives = num 3.0 } ])

let test_game_bad_lives_var _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ Game { planet = "earth"; level = num 1.0; lives = var "n" } ])

(* ─────────────────────────────────────────
   2.16  check_stmt – Fork
   ───────────────────────────────────────── *)

let test_fork_ok _ =
  (* fork requires the name to be a declared projectile *)
  run [ mk_proj "cannon" (num 45.0) (num 100.0)
      ; Fork ("cannon",
          [ { label = "low";
              br_stmts = minimal_sim_stmts "cannon" }
          ; { label = "high";
              br_stmts = minimal_sim_stmts "cannon" } ]) ]

let test_fork_undeclared_proj _ =
  assert_sem_err "used before declaration"
    (fun () -> run
      [ Fork ("ghost",
          [ { label = "a"; br_stmts = [] } ]) ])

let test_fork_branch_gravity_required _ =
  assert_sem_err "missing gravity"
    (fun () -> run
      [ mk_proj "p" (num 45.0) (num 100.0)
      ; Fork ("p",
          [ { label = "x";
              br_stmts =
                [ sproj "p2" (num 30.0) (num 50.0)
                ; SPlot "p2" ] } ]) ])

(* ─────────────────────────────────────────
   2.17  DSL program smoke tests
   ───────────────────────────────────────── *)

let test_prog1_ok _ = run prog1
let test_prog2_ok _ = run prog2
let test_prog3_ok _ = run prog3
let test_prog4_ok _ = run prog4
let test_prog5_ok _ = run prog5
let test_prog6_ok _ = run prog6
let test_prog7_ok _ = run prog7
let test_prog8_ok _ = run prog8
let test_prog9_ok _ = run prog9
let test_prog10_ok _ = run prog10
let test_prog11_ok _ = run prog11
let test_prog12_ok _ = run prog12

(* ─────────────────────────────────────────
   2.18  check (entry point)
   ───────────────────────────────────────── *)

let test_check_empty_program _ =
  check []   (* should not raise *)

let test_check_multiple_stmts _ =
  check [ Let ("x", num 1.0)
        ; Set ("x", num 2.0)
        ; Let ("y", var "x") ]

let test_check_returns_unit _ =
  let result = check [] in
  assert_equal () result

(* ─────────────────────────────────────────
   2.19  Corner / integration cases
   ───────────────────────────────────────── *)

let test_proj_declared_before_simulate _ =
  (* projectile declared at top level, then used in simulate *)
  run [ mk_proj "cannon" (num 45.0) (num 80.0)
      ; sim_with
          [ sproj "cannon2" (num 30.0) (num 100.0)
          ; SGravity (num 9.81)
          ; SPlot "cannon2" ] ]

let test_simulate_repeat_nested_ok _ =
  run prog10

let test_for_nested_in_while _ =
  run [ Let ("x", num 0.0)
      ; While (cmp_gt (num 5.0) (var "x"),
          [ For ("i", num 0.0, num 3.0, num 1.0,
              [ Set ("x", add (var "x") (var "i")) ]) ]) ]

let test_let_shadow_after_for _ =
  (* 'i' from for is gone; we can re-declare it with let *)
  run [ For ("i", num 0.0, num 3.0, num 1.0, [])
      ; Let ("i", num 99.0) ]

let test_sim_wind_expr_with_outer_var _ =
  (* outer let var visible inside simulate *)
  run [ Let ("w", num 2.5)
      ; sim_with
          [ sproj "p" (num 45.0) (num 100.0)
          ; SGravity (num 9.81)
          ; SWindX (var "w")
          ; SPlot "p" ] ]

let test_sim_min_vel_ok _ =
  run [ sim_with
    [ sproj "p" (num 45.0) (num 100.0)
    ; SGravity (num 9.81)
    ; SMinVel ("p", num 100.0, num 50.0)
    ; SPlot "p" ] ]

let test_sim_max_rect_ok _ =
  run [ sim_with
    [ sproj "p" (num 45.0) (num 100.0)
    ; SGravity (num 9.81)
    ; SMaxRect "p"
    ; SPlot "p" ] ]

let test_multiple_valid_planets _ =
  List.iter (fun planet ->
    run [ Game { planet; level = num 1.0; lives = num 1.0 } ]
  ) valid_planets

let test_if_then_var_not_leaked _ =
  (* var declared in then-branch is not visible after if *)
  assert_sem_err "used before declaration"
    (fun () -> run
      [ Let ("x", num 1.0)
      ; IfElse (cmp_gt (var "x") (num 0.0),
          [ Let ("tmp", num 5.0) ], None)
      ; Let ("y", var "tmp") ])   (* tmp not in scope here *)


(* =============================================================
   SECTION 3 — Test suite assembly
   ============================================================= *)

let suite =
  "ProjX Checker Tests" >::: [

    (* sem_err *)
    "sem_err_raises"              >:: test_sem_err_raises;
    "sem_err_prefix"              >:: test_sem_err_prefix;
    "sem_err_msg"                 >:: test_sem_err_message_preserved;

    (* declare_var *)
    "decl_var_adds"               >:: test_declare_var_adds;
    "decl_var_dup"                >:: test_declare_var_duplicate_raises;
    "decl_var_multi"              >:: test_declare_var_multiple;
    "decl_var_no_projs"           >:: test_declare_var_does_not_touch_projs;

    (* declare_or_update_proj *)
    "decl_proj_adds"              >:: test_declare_proj_adds;
    "decl_proj_dup_ok"            >:: test_declare_proj_duplicate_no_raise;
    "decl_proj_multi"             >:: test_declare_proj_multiple;
    "decl_proj_no_vars"           >:: test_declare_proj_does_not_touch_vars;

    (* check_var *)
    "check_var_ok"                >:: test_check_var_ok;
    "check_var_undecl"            >:: test_check_var_undeclared_raises;
    "check_var_wrong"             >:: test_check_var_wrong_name_raises;

    (* check_proj *)
    "check_proj_ok"               >:: test_check_proj_ok;
    "check_proj_undecl"           >:: test_check_proj_undeclared_raises;
    "check_proj_wrong"            >:: test_check_proj_wrong_name;

    (* check_set *)
    "check_set_ok"                >:: test_check_set_ok;
    "check_set_undecl"            >:: test_check_set_undeclared_raises;
    "check_set_diff"              >:: test_check_set_different_var_raises;

    (* check_expr *)
    "expr_num"                    >:: test_check_expr_num;
    "expr_var_ok"                 >:: test_check_expr_var_ok;
    "expr_var_undecl"             >:: test_check_expr_var_undeclared;
    "expr_binop_ok"               >:: test_check_expr_binop_ok;
    "expr_binop_bad_l"            >:: test_check_expr_binop_bad_left;
    "expr_binop_bad_r"            >:: test_check_expr_binop_bad_right;
    "expr_nested"                 >:: test_check_expr_nested_binop;
    "expr_dotq_ok"                >:: test_check_expr_dotq_ok;
    "expr_dotq_bad"               >:: test_check_expr_dotq_bad_proj;

    (* check_dot_query *)
    "dot_range"                   >:: test_dot_range_ok;
    "dot_max_range"               >:: test_dot_max_range_ok;
    "dot_max_height"              >:: test_dot_max_height_ok;
    "dot_max_rect"                >:: test_dot_max_rect_ok;
    "dot_min_vel"                 >:: test_dot_min_vel_ok;
    "dot_collide_ok"              >:: test_dot_collide_ok;
    "dot_min_dist_ok"             >:: test_dot_min_dist_ok;
    "dot_collide_bad_p1"          >:: test_dot_collide_bad_p1;
    "dot_collide_bad_p2"          >:: test_dot_collide_bad_p2;

    (* check_cond *)
    "cond_cmp_ok"                 >:: test_check_cond_cmp_ok;
    "cond_cmp_bad"                >:: test_check_cond_cmp_bad;
    "cond_and_ok"                 >:: test_check_cond_and_ok;
    "cond_and_bad_r"              >:: test_check_cond_and_bad_right;
    "cond_or_ok"                  >:: test_check_cond_or_ok;
    "cond_not_ok"                 >:: test_check_cond_not_ok;
    "cond_not_bad"                >:: test_check_cond_not_bad;
    "cond_booldotq"               >:: test_check_cond_booldotq_ok;

    (* check_sim_stmts *)
    "sim_no_gravity"              >:: test_sim_missing_gravity;
    "sim_double_gravity"          >:: test_sim_double_gravity;
    "sim_no_plot"                 >:: test_sim_missing_plot;
    "sim_valid"                   >:: test_sim_valid_passes;
    "sim_plot_in_for"             >:: test_sim_plot_inside_for_counts;
    "sim_for_scope"               >:: test_sim_for_loop_scoping;
    "sim_for_no_leak"             >:: test_sim_for_var_not_leaked;
    "sim_proj_undecl"             >:: test_sim_proj_undeclared_plot;
    "sim_collide_undecl"          >:: test_sim_collide_undeclared;
    "sim_check_ok"                >:: test_sim_check_ok;

    (* Projectile top-level *)
    "proj_ok"                     >:: test_stmt_proj_ok;
    "proj_full_ok"                >:: test_stmt_proj_full_ok;
    "proj_bad_angle"              >:: test_stmt_proj_bad_angle_var;
    "proj_bad_speed"              >:: test_stmt_proj_bad_speed_var;
    "proj_bad_mass"               >:: test_stmt_proj_bad_mass_var;
    "proj_azimuth_ok"             >:: test_stmt_proj_azimuth_ok;
    "proj_azimuth_bad"            >:: test_stmt_proj_azimuth_bad_var;

    (* Let / Set *)
    "let_ok"                      >:: test_let_ok;
    "let_chain"                   >:: test_let_expr_with_var;
    "let_dup"                     >:: test_let_duplicate_raises;
    "let_bad_rhs"                 >:: test_let_undeclared_rhs;
    "set_ok"                      >:: test_set_ok;
    "set_undecl"                  >:: test_set_undeclared_raises;
    "set_bad_rhs"                 >:: test_set_bad_rhs;

    (* For / Repeat / While *)
    "for_ok"                      >:: test_for_ok;
    "for_scope"                   >:: test_for_loop_var_scoped;
    "for_no_leak"                 >:: test_for_loop_var_not_outside;
    "for_bad_bound"               >:: test_for_bad_bound;
    "repeat_ok"                   >:: test_repeat_ok;
    "repeat_bad_n"                >:: test_repeat_bad_count;
    "while_ok"                    >:: test_while_ok;
    "while_bad_cond"              >:: test_while_bad_cond;

    (* IfElse *)
    "if_no_else"                  >:: test_if_no_else_ok;
    "if_else_ok"                  >:: test_if_with_else_ok;
    "if_bad_cond"                 >:: test_if_bad_cond;
    "if_bad_then"                 >:: test_if_bad_then_body;
    "if_bad_else"                 >:: test_if_bad_else_body;

    (* Game *)
    "game_earth"                  >:: test_game_earth_ok;
    "game_moon"                   >:: test_game_moon_ok;
    "game_mars"                   >:: test_game_mars_ok;
    "game_jupiter"                >:: test_game_jupiter_ok;
    "game_sun"                    >:: test_game_sun_ok;
    "game_invalid"                >:: test_game_invalid_planet;
    "game_case"                   >:: test_game_invalid_planet_case_sensitive;
    "game_bad_level"              >:: test_game_bad_level_var;
    "game_bad_lives"              >:: test_game_bad_lives_var;

    (* Fork *)
    "fork_ok"                     >:: test_fork_ok;
    "fork_undecl"                 >:: test_fork_undeclared_proj;
    "fork_branch_gravity"         >:: test_fork_branch_gravity_required;

    (* DSL program smoke tests *)
    "prog1"                       >:: test_prog1_ok;
    "prog2"                       >:: test_prog2_ok;
    "prog3"                       >:: test_prog3_ok;
    "prog4"                       >:: test_prog4_ok;
    "prog5"                       >:: test_prog5_ok;
    "prog6"                       >:: test_prog6_ok;
    "prog7"                       >:: test_prog7_ok;
    "prog8"                       >:: test_prog8_ok;
    "prog9"                       >:: test_prog9_ok;
    "prog10"                      >:: test_prog10_ok;
    "prog11"                      >:: test_prog11_ok;
    "prog12"                      >:: test_prog12_ok;

    (* check entry point *)
    "check_empty"                 >:: test_check_empty_program;
    "check_multi"                 >:: test_check_multiple_stmts;
    "check_unit"                  >:: test_check_returns_unit;

    (* corner / integration *)
    "proj_before_sim"             >:: test_proj_declared_before_simulate;
    "sim_repeat_nested"           >:: test_simulate_repeat_nested_ok;
    "for_nested_while"            >:: test_for_nested_in_while;
    "let_after_for"               >:: test_let_shadow_after_for;
    "sim_outer_var"               >:: test_sim_wind_expr_with_outer_var;
    "sim_min_vel"                 >:: test_sim_min_vel_ok;
    "sim_max_rect"                >:: test_sim_max_rect_ok;
    "all_planets"                 >:: test_multiple_valid_planets;
    "if_then_no_leak"             >:: test_if_then_var_not_leaked;
  ]

let () = run_test_tt_main suite