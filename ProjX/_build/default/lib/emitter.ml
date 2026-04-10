(* ── ProjX v3 Emitter ──
   Walks the AST, evaluates it,
   and emits an array to data.js for the canvas frontend *)

open Ast
open Env

let json_float f =
  if Float.is_integer f then string_of_int (int_of_float f)
  else Printf.sprintf "%.4f" f

let json_string s = "\"" ^ s ^ "\""

let emit_projectile (name, (proj : projectile_val)) =
  let (x0, y0, _) = proj.launch_from in
  Printf.sprintf
    "      {\"id\":%s, \"angle\":%s, \"speed\":%s, \"launch_from\":[%s,%s]}"
    (json_string name)
    (json_float proj.angle)
    (json_float proj.speed)
    (json_float x0)
    (json_float y0)

let emit_annotations range_annos max_h_annos =
  let r_strs = List.map (fun (name, v) ->
    Printf.sprintf "      {\"type\":\"range\", \"p\":%s, \"value\":%s}" (json_string name) (json_float v)
  ) range_annos in
  let m_strs = List.map (fun (name, v) ->
    Printf.sprintf "      {\"type\":\"max_height\", \"p\":%s, \"value\":%s}" (json_string name) (json_float v)
  ) max_h_annos in
  r_strs @ m_strs

let emit_scenario = function
  | SimScenario (label, gravity, projs, r_annos, m_annos) ->
      let projs_js = List.map emit_projectile (List.rev projs) in
      let annos_js = emit_annotations (List.rev r_annos) (List.rev m_annos) in
      Printf.sprintf
        "  {\n\
        \    \"label\": %s,\n\
        \    \"mode\": \"simulate\",\n\
        \    \"gravity\": %s,\n\
        \    \"projectiles\": [\n%s\n    ],\n\
        \    \"annotations\": [\n%s\n    ]\n\
           }"
        (json_string label)
        (json_float gravity)
        (String.concat ",\n" projs_js)
        (String.concat ",\n" annos_js)

  | GameScenario (label, planet, gravity, level, lives) ->
      Printf.sprintf
        "  {\n\
        \    \"label\": %s,\n\
        \    \"mode\": \"game\",\n\
        \    \"planet\": %s,\n\
        \    \"gravity\": %s,\n\
        \    \"level\": %s,\n\
        \    \"lives\": %s,\n\
        \    \"targets\": [],\n\
        \    \"walls\": []\n\
           }"
        (json_string label)
        (json_string planet)
        (json_float gravity)
        (json_float level)
        (json_float lives)

let emit_program (prog : program) =
  emitted_scenarios := [];
  ignore (Eval.eval_program prog);
  let scenarios_js = List.map emit_scenario !emitted_scenarios in
  Printf.sprintf
    "const projxData = [\n%s\n];\n"
    (String.concat ",\n" scenarios_js)
