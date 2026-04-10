(* ── ProjX v3 Emitter ──
   Walks the AST, evaluates it,
   and emits a data.js file for the canvas frontend *)

open Ast
open Env
open Physics

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   JSON helpers
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let json_float f =
  if Float.is_integer f then string_of_int (int_of_float f)
  else Printf.sprintf "%.4f" f

let json_string s = "\"" ^ s ^ "\""

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Emit a single projectile as JSON
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let emit_projectile (name, (proj : projectile_val)) =
  let (x0, y0, _) = proj.launch_from in
  Printf.sprintf
    "    {\"id\":%s, \"angle\":%s, \"speed\":%s, \"launch_from\":[%s,%s]}"
    (json_string name)
    (json_float proj.angle)
    (json_float proj.speed)
    (json_float x0)
    (json_float y0)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Emit annotations (computed physics)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let emit_annotations gravity projs =
  let annos = List.concat_map (fun (name, (proj : projectile_val)) ->
    let (x0, y0, _) = proj.launch_from in
    let r  = range proj.angle proj.speed gravity +. x0 in
    let mh = max_height proj.angle proj.speed gravity +. y0 in
    [ Printf.sprintf "    {\"type\":\"range\", \"p\":%s, \"value\":%s}"
        (json_string name) (json_float r);
      Printf.sprintf "    {\"type\":\"max_height\", \"p\":%s, \"value\":%s}"
        (json_string name) (json_float mh) ]
  ) projs in
  annos

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Collect simulate gravity
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec collect_gravity env stmts =
  List.fold_left (fun acc s ->
    match s with
    | SGravity e -> Eval.eval_expr env e
    | _          -> acc
  ) 9.8 stmts

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Emit simulate mode
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and emit_simulate env gravity =
  let projs    = List.rev env.projectiles in
  let proj_js  = List.map emit_projectile projs in
  let anno_js  = emit_annotations gravity projs in
  Printf.sprintf
    "const projxData = {\n\
    \  \"mode\": \"simulate\",\n\
    \  \"gravity\": %s,\n\
    \  \"projectiles\": [\n%s\n  ],\n\
    \  \"annotations\": [\n%s\n  ]\n\
     };\n"
    (json_float gravity)
    (String.concat ",\n" proj_js)
    (String.concat ",\n" anno_js)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Emit game mode
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and emit_game planet gravity level lives =
  Printf.sprintf
    "const projxData = {\n\
    \  \"mode\": \"game\",\n\
    \  \"planet\": %s,\n\
    \  \"gravity\": %s,\n\
    \  \"level\": %s,\n\
    \  \"lives\": %s,\n\
    \  \"targets\": [],\n\
    \  \"walls\": []\n\
     };\n"
    (json_string planet)
    (json_float gravity)
    (json_float level)
    (json_float lives)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Walk program — find mode, emit JS
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and emit_program (prog : program) =
  (* first, evaluate the whole program to build the env *)
  let env = List.fold_left Eval.eval_stmt empty_env prog in

  (* figure out the mode: scan for Game or Simulate *)
  let (mode, gravity, planet, level, lives) =
    List.fold_left (fun (m, g, pl, lv, ls) stmt ->
      match stmt with
      | Simulate ss ->
          let grav = collect_gravity env ss in
          ("simulate", grav, pl, lv, ls)
      | Game { planet; level; lives } ->
          let g   = planet_gravity planet in
          let lvl = Eval.eval_expr env level in
          let lvs = Eval.eval_expr env lives in
          ("game", g, planet, lvl, lvs)
      | _ -> (m, g, pl, lv, ls)
    ) ("simulate", 9.8, "earth", 1.0, 3.0) prog
  in

  match mode with
  | "game"     -> emit_game planet gravity level lives
  | _          -> emit_simulate env gravity
