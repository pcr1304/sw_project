(* ── ProjX v4  json_emit.ml — 3D upgrade ── *)

open Ast
open Env
open Physics

(* ══════════════════════════════════════════════════════════════
   Tiny JSON helpers
   ══════════════════════════════════════════════════════════════ *)

let jf f =
  let s = Printf.sprintf "%.6f" f in
  let i = ref (String.length s - 1) in
  while !i > 0 && s.[!i] = '0' do
    decr i
  done;
  if s.[!i] = '.' then incr i;
  String.sub s 0 (!i + 1)

let js s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf

let jb b = if b then "true" else "false"
let jarr items = "[" ^ String.concat "," items ^ "]"

let jobj pairs =
  let members =
    List.filter_map
      (fun (k, v_opt) ->
        match v_opt with None -> None | Some v -> Some (js k ^ ":" ^ v))
      pairs
  in
  "{" ^ String.concat "," members ^ "}"

(* ══════════════════════════════════════════════════════════════
   Expression / condition evaluator
   ══════════════════════════════════════════════════════════════ *)

let rec je_expr env = function
  | Num n -> n
  | Var name -> get_var name env.vars
  | Binop (op, e1, e2) -> (
      let v1 = je_expr env e1 in
      let v2 = je_expr env e2 in
      match op with
      | Add -> v1 +. v2
      | Sub -> v1 -. v2
      | Mul -> v1 *. v2
      | Div -> if v2 = 0.0 then failwith "Division by zero" else v1 /. v2)
  | DotQ dq -> je_dotq env dq

and je_dotq env = function
  | DotRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = je_gopt env g_opt in
      let x0, _, _, _ = proj.launch_from in
      range proj.angle proj.speed g +. x0
  | DotMaxRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      max_range proj.speed (je_gopt env g_opt)
  | DotMaxHeight (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = je_gopt env g_opt in
      let _, y0, _, _ = proj.launch_from in
      max_height proj.angle proj.speed g +. y0
  | DotMaxRect (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = je_gopt env g_opt in
      let x0, y0, _, _ = proj.launch_from in
      let area, _, _, _ = max_rectangle proj.angle proj.speed g x0 y0 in
      area
  | DotMinVel (p, x_e, h_e, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = je_gopt env g_opt in
      let x0, y0, _, _ = proj.launch_from in
      min_vel proj.angle g (je_expr env x_e) (je_expr env h_e) x0 y0
  | DotCollide (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g = je_gopt env g_opt in
      let x01, y01, _, _ = proj1.launch_from in
      let x02, y02, _, _ = proj2.launch_from in
      let hit, _, _, _ =
        collide ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:g
      in
      if hit then 1.0 else 0.0
  | DotMinDist (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g = je_gopt env g_opt in
      let x01, y01, _, _ = proj1.launch_from in
      let x02, y02, _, _ = proj2.launch_from in
      min_dist ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
        ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:g

and je_gopt env = function Some e -> je_expr env e | None -> 9.8

and je_cond env = function
  | Cmp (op, e1, e2) -> (
      let v1 = je_expr env e1 in
      let v2 = je_expr env e2 in
      match op with
      | Eq -> v1 = v2
      | Neq -> v1 <> v2
      | Lt -> v1 < v2
      | Gt -> v1 > v2
      | Leq -> v1 <= v2
      | Geq -> v1 >= v2)
  | And (a, b) -> je_cond env a && je_cond env b
  | Or (a, b) -> je_cond env a || je_cond env b
  | Not c -> not (je_cond env c)
  | BoolDotQ dq -> je_dotq env dq > 0.0

(* ══════════════════════════════════════════════════════════════
   Projectile JSON helper
   NOW includes azimuth and launch_delay
   ══════════════════════════════════════════════════════════════ *)

let proj_to_json name (p : projectile_val) =
  let x0, y0, z0, t0 = p.launch_from in
  jobj
    [
      ("id", Some (js name));
      ("angle", Some (jf p.angle));
      ("azimuth", Some (jf p.azimuth));
      ("speed", Some (jf p.speed));
      ("launch_from", Some (Printf.sprintf "[%s,%s,%s]" (jf x0) (jf y0) (jf z0)));
      ("launch_delay", Some (jf t0));
      ("mass", Some (match p.mass with Some m -> jf m | None -> "null"));
      ( "drag_coeff",
        Some (match p.drag_coeff with Some d -> jf d | None -> "null") );
      ( "cross_section",
        Some (match p.cross_section with Some c -> jf c | None -> "null") );
    ]

(* ══════════════════════════════════════════════════════════════
   Evaluate an inline SProjectile and register it in env
   ══════════════════════════════════════════════════════════════ *)

let eval_sprojectile env (p : Ast.sim_stmt) =
  match p with
  | SProjectile sp ->
      let a = je_expr env sp.angle in
      let az = match sp.azimuth with Some e -> je_expr env e | None -> 0.0 in
      let s = je_expr env sp.speed in
      let x, y, z, t =
        match sp.launch_from with
        | Some (ex, ey, ez, et) ->
            (je_expr env ex, je_expr env ey, je_expr env ez, je_expr env et)
        | None -> (0.0, 0.0, 0.0, 0.0)
      in
      let pv : projectile_val =
        {
          angle = a;
          azimuth = az;
          speed = s;
          launch_from = (x, y, z, t);
          mass = Option.map (je_expr env) sp.mass;
          drag_coeff = Option.map (je_expr env) sp.drag_coeff;
          cross_section = Option.map (je_expr env) sp.cross_section;
        }
      in
      add_projectile sp.name pv env
  | _ -> env

(* ══════════════════════════════════════════════════════════════
   simulate block → one JSON scenario object
   ══════════════════════════════════════════════════════════════ *)

let simulate_to_json env stmts sim_number =
  let gravity = ref 9.8 in
  let air_resistance = ref false in
  let air_density = ref default_air_density in
  let wind_x = ref 0.0 in
  let wind_y = ref 0.0 in
  let wind_z = ref 0.0 in
  (* NEW *)

  List.iter
    (function
      | SGravity e -> gravity := je_expr env e
      | SAirResistance b -> air_resistance := b
      | SAirDensity e -> air_density := je_expr env e
      | SWindX e -> wind_x := je_expr env e
      | SWindY e -> wind_y := je_expr env e
      | SWindZ e -> wind_z := je_expr env e (* NEW *)
      | _ -> ())
    stmts;

  let g = !gravity in
  let ar = !air_resistance in
  let dp : drag_params =
    {
      enabled = ar;
      air_density = !air_density;
      wind_x = !wind_x;
      wind_y = !wind_y;
      wind_z = !wind_z;
      (* NEW *)
    }
  in

  let proj_jsons = ref [] in
  let annotations = ref [] in
  let bounces = ref [] in
  let collisions = ref [] in
  let queries = ref [] in

  let add_query label value unit note =
    queries :=
      !queries
      @ [
          jobj
            [
              ("label", Some (js label));
              ("value", Some (jf value));
              ("unit", Some (js unit));
              ("note", match note with Some n -> Some (js n) | None -> None);
            ];
        ]
  in
  let add_query_str label value_str unit note =
    queries :=
      !queries
      @ [
          jobj
            [
              ("label", Some (js label));
              ("value", Some (js value_str));
              ("unit", Some (js unit));
              ("note", match note with Some n -> Some (js n) | None -> None);
            ];
        ]
  in

  let rec exec env_inner stmt =
    match stmt with
    | SGravity _ | SAirResistance _ | SAirDensity _ | SWindX _ | SWindY _
    | SWindZ _ ->
        env_inner (* NEW: SWindZ here *)
    | SProjectile sp -> eval_sprojectile env_inner (SProjectile sp)
    | SPlot pid ->
        let pv = get_projectile pid env_inner.projectiles in
        let x0, y0, z0, _ = pv.launch_from in
        let mass, cd, area = get_drag_params pv in
        let r =
          if ar then range_with_drag pv.angle pv.speed g x0 y0 dp mass cd area
          else range pv.angle pv.speed g +. x0
        in
        let mh =
          if ar then max_height_with_drag pv.angle pv.speed g y0 dp mass cd area
          else max_height pv.angle pv.speed g +. y0
        in
        let traj3d =
          simulate_trajectory_3d ~angle:pv.angle ~azimuth:pv.azimuth
            ~speed:pv.speed ~x0 ~y0 ~z0 ~gravity:g ~mass ~drag_coeff:cd
            ~cross_section:area ~drag_params:dp ()
        in
        let pts3d_json =
          jarr
            (List.map
               (fun (_t, x, y, z) ->
                 Printf.sprintf "[%s,%s,%s]" (jf x) (jf y) (jf z))
               traj3d)
        in
        proj_jsons := !proj_jsons @ [ proj_to_json pid pv ];
        annotations :=
          !annotations
          @ [
              jobj
                [
                  ("type", Some (js "range"));
                  ("p", Some (js pid));
                  ("value", Some (jf r));
                ];
              jobj
                [
                  ("type", Some (js "max_height"));
                  ("p", Some (js pid));
                  ("value", Some (jf mh));
                ];
              jobj
                [
                  ("type", Some (js "points3d"));
                  ("p", Some (js pid));
                  ("value", Some pts3d_json);
                ];
            ];
        env_inner
    | SRange pid ->
        let pv = get_projectile pid env_inner.projectiles in
        let x0, y0, _, _ = pv.launch_from in
        let mass, cd, area = get_drag_params pv in
        let r =
          if ar then range_with_drag pv.angle pv.speed g x0 y0 dp mass cd area
          else range pv.angle pv.speed g +. x0
        in
        add_query ("range " ^ pid) r "m" (if ar then Some "with drag" else None);
        env_inner
    | SMaxRange pid ->
        let pv = get_projectile pid env_inner.projectiles in
        let x0, y0, _, _ = pv.launch_from in
        let mass, cd, area = get_drag_params pv in
        if ar then begin
          let mr, opt_ang =
            max_range_with_drag pv.speed g x0 y0 dp mass cd area
          in
          add_query ("max_range " ^ pid) mr "m"
            (Some (Printf.sprintf "at %.1f° (with drag)" opt_ang))
        end
        else
          add_query ("max_range " ^ pid) (max_range pv.speed g) "m"
            (Some (Printf.sprintf "at %.1f°" max_range_angle));
        env_inner
    | SMaxHeight pid ->
        let pv = get_projectile pid env_inner.projectiles in
        let _, y0, _, _ = pv.launch_from in
        let mass, cd, area = get_drag_params pv in
        let mh =
          if ar then max_height_with_drag pv.angle pv.speed g y0 dp mass cd area
          else max_height pv.angle pv.speed g +. y0
        in
        add_query ("max_height " ^ pid) mh "m"
          (if ar then Some "with drag" else None);
        env_inner
    | SMaxRect pid ->
        let pv = get_projectile pid env_inner.projectiles in
        let x0, y0, _, _ = pv.launch_from in
        let area, _, _, _ = max_rectangle pv.angle pv.speed g x0 y0 in
        add_query ("max_rectangle " ^ pid) area "m²" None;
        env_inner
    | SMinVel (pid, x_e, h_e) ->
        let pv = get_projectile pid env_inner.projectiles in
        let tx = je_expr env_inner x_e in
        let th = je_expr env_inner h_e in
        let x0, y0, _, _ = pv.launch_from in
        let mv = min_vel pv.angle g tx th x0 y0 in
        add_query ("min_vel " ^ pid) mv "m/s"
          (Some (Printf.sprintf "tower (%.1f, %.1f)" tx th));
        env_inner
    | SCollide (p1, p2) ->
        let pv1 = get_projectile p1 env_inner.projectiles in
        let pv2 = get_projectile p2 env_inner.projectiles in
        let x01, y01, _, _ = pv1.launch_from in
        let x02, y02, _, _ = pv2.launch_from in
        let hit, t, cx, cy =
          collide ~angle1:pv1.angle ~speed1:pv1.speed ~x01 ~y01
            ~angle2:pv2.angle ~speed2:pv2.speed ~x02 ~y02 ~gravity:g
        in
        if hit then begin
          collisions :=
            !collisions
            @ [
                jobj
                  [
                    ("p1", Some (js p1));
                    ("p2", Some (js p2));
                    ("t", Some (jf t));
                    ("x", Some (jf cx));
                    ("y", Some (jf cy));
                  ];
              ];
          add_query_str
            ("collide " ^ p1 ^ "/" ^ p2)
            (Printf.sprintf "YES  t=%.2fs  (%.1f, %.1f)" t cx cy)
            "" None
        end
        else add_query_str ("collide " ^ p1 ^ "/" ^ p2) "NO" "" None;
        env_inner
    | SCollisionVel (p1, p2) ->
        let pv1 = get_projectile p1 env_inner.projectiles in
        let pv2 = get_projectile p2 env_inner.projectiles in
        let x01, y01, _, _ = pv1.launch_from in
        let x02, y02, _, _ = pv2.launch_from in
        let hit, t, _, _ =
          collide ~angle1:pv1.angle ~speed1:pv1.speed ~x01 ~y01
            ~angle2:pv2.angle ~speed2:pv2.speed ~x02 ~y02 ~gravity:g
        in
        if hit then begin
          let (vx1, vy1), (vx2, vy2) =
            collision_vel ~angle1:pv1.angle ~speed1:pv1.speed ~angle2:pv2.angle
              ~speed2:pv2.speed ~gravity:g t
          in
          add_query_str ("collision_vel " ^ p1)
            (Printf.sprintf "(%.2f, %.2f)" vx1 vy1)
            "m/s" None;
          add_query_str ("collision_vel " ^ p2)
            (Printf.sprintf "(%.2f, %.2f)" vx2 vy2)
            "m/s" None
        end
        else
          add_query_str
            ("collision_vel " ^ p1 ^ "/" ^ p2)
            "no collision" "" None;
        env_inner
    | SMinDist (p1, p2) ->
        let pv1 = get_projectile p1 env_inner.projectiles in
        let pv2 = get_projectile p2 env_inner.projectiles in
        let x01, y01, _, _ = pv1.launch_from in
        let x02, y02, _, _ = pv2.launch_from in
        let md =
          min_dist ~angle1:pv1.angle ~speed1:pv1.speed ~x01 ~y01
            ~angle2:pv2.angle ~speed2:pv2.speed ~x02 ~y02 ~gravity:g
        in
        add_query ("min_dist " ^ p1 ^ "/" ^ p2) md "m" None;
        env_inner
    | SBounce (pid, n_e, r_e) ->
        let pv = get_projectile pid env_inner.projectiles in
        let n = int_of_float (je_expr env_inner n_e) in
        let rest = je_expr env_inner r_e in
        let x0, y0, _, _ = pv.launch_from in
        let arcs = bounce_arcs pv.angle pv.speed g rest n x0 y0 in
        let arc_jsons =
          List.map
            (fun (ax, ay, ang, spd) ->
              Printf.sprintf "[%s,%s,%s,%s]" (jf ax) (jf ay) (jf ang) (jf spd))
            arcs
        in
        bounces :=
          !bounces
          @ [ jobj [ ("p", Some (js pid)); ("arcs", Some (jarr arc_jsons)) ] ];
        let hop_lines =
          List.mapi
            (fun i (x, _, ang, spd) ->
              Printf.sprintf "hop %d: x0=%.2f  range=%.2f m" (i + 1) x
                (range ang spd g))
            arcs
        in
        add_query_str ("bounce " ^ pid)
          (Printf.sprintf "%d hops  rest=%.2f" n rest)
          ""
          (Some (String.concat " | " hop_lines));
        env_inner
    | SCheck c ->
        add_query_str "check"
          (if je_cond env_inner c then "PASS ✓" else "FAIL ✗")
          "" None;
        env_inner
    | SFor (var, s_e, e_e, step_e, body) ->
        let sv = je_expr env_inner s_e in
        let ev_ = je_expr env_inner e_e in
        let step = je_expr env_inner step_e in
        let rec loop i cur =
          if i > ev_ then cur
          else
            let ei = add_var var i cur in
            let after = List.fold_left exec ei body in
            loop (i +. step)
              {
                after with
                vars = List.filter (fun (n, _) -> n <> var) after.vars;
              }
        in
        loop sv env_inner
    | SRepeat (n_e, body) ->
        let n = int_of_float (je_expr env_inner n_e) in
        let rec loop i cur =
          if i <= 0 then cur else loop (i - 1) (List.fold_left exec cur body)
        in
        loop n env_inner
    | SWhile (c_e, body) ->
        let rec loop cur =
          if not (je_cond cur c_e) then cur
          else loop (List.fold_left exec cur body)
        in
        loop env_inner
  in

  ignore (List.fold_left exec env stmts);

  let label =
    let base = Printf.sprintf "Sim %d  (g=%.2f)" sim_number g in
    if ar then
      base
      ^ Printf.sprintf "  air  ρ=%.3f  wind=(%.1f,%.1f,%.1f)" dp.air_density
          dp.wind_x dp.wind_y dp.wind_z
    else base
  in

  jobj
    [
      ("mode", Some (js "simulate"));
      ("label", Some (js label));
      ("gravity", Some (jf g));
      ("air_resistance", Some (jb ar));
      ("air_density", Some (jf dp.air_density));
      ("wind_x", Some (jf dp.wind_x));
      ("wind_y", Some (jf dp.wind_y));
      ("wind_z", Some (jf dp.wind_z));
      (* NEW *)
      ("projectiles", Some (jarr !proj_jsons));
      ("annotations", Some (jarr !annotations));
      ("bounces", Some (jarr !bounces));
      ("collisions", Some (jarr !collisions));
      ("queries", Some (jarr !queries));
    ]

(* ══════════════════════════════════════════════════════════════
   fork block → one JSON scenario object per branch
   ══════════════════════════════════════════════════════════════ *)

let fork_branch_to_json env proj_name (pv : projectile_val) branch =
  let gravity = ref 9.8 in
  let bounce_args = ref None in

  let rec collect_settings env_inner s =
    match s with
    | SGravity e ->
        gravity := je_expr env_inner e;
        env_inner
    | SBounce (_, n_e, r_e) ->
        bounce_args := Some (n_e, r_e);
        env_inner
    | SProjectile sp -> eval_sprojectile env_inner (SProjectile sp)
    | SFor (var, s_e, e_e, step_e, body) ->
        let sv = je_expr env_inner s_e in
        let ev_ = je_expr env_inner e_e in
        let step = je_expr env_inner step_e in
        let rec loop i cur =
          if i > ev_ then cur
          else
            let ei = add_var var i cur in
            let after = List.fold_left collect_settings ei body in
            loop (i +. step)
              {
                after with
                vars = List.filter (fun (n, _) -> n <> var) after.vars;
              }
        in
        loop sv env_inner
    | SRepeat (n_e, body) ->
        let n = int_of_float (je_expr env_inner n_e) in
        let rec loop i cur =
          if i <= 0 then cur
          else loop (i - 1) (List.fold_left collect_settings cur body)
        in
        loop n env_inner
    | SWhile (c_e, body) ->
        let rec loop cur =
          if not (je_cond cur c_e) then cur
          else loop (List.fold_left collect_settings cur body)
        in
        loop env_inner
    | _ -> env_inner
  in
  ignore (List.fold_left collect_settings env branch.br_stmts);

  let g = !gravity in
  let x0, y0, _, _ = pv.launch_from in
  let r = range pv.angle pv.speed g +. x0 in
  let mh = max_height pv.angle pv.speed g +. y0 in

  let annotations =
    [
      jobj
        [
          ("type", Some (js "range"));
          ("p", Some (js proj_name));
          ("value", Some (jf r));
        ];
      jobj
        [
          ("type", Some (js "max_height"));
          ("p", Some (js proj_name));
          ("value", Some (jf mh));
        ];
    ]
  in

  let bounces =
    match !bounce_args with
    | None -> []
    | Some (n_e, r_e) ->
        let n = int_of_float (je_expr env n_e) in
        let rest = je_expr env r_e in
        let arcs = bounce_arcs pv.angle pv.speed g rest n x0 y0 in
        let arc_jsons =
          List.map
            (fun (ax, ay, ang, spd) ->
              Printf.sprintf "[%s,%s,%s,%s]" (jf ax) (jf ay) (jf ang) (jf spd))
            arcs
        in
        [ jobj [ ("p", Some (js proj_name)); ("arcs", Some (jarr arc_jsons)) ] ]
  in

  jobj
    [
      ("mode", Some (js "simulate"));
      ("label", Some (js (Printf.sprintf "Fork: %s  (g=%.2f)" branch.label g)));
      ("gravity", Some (jf g));
      ("air_resistance", Some "false");
      ("air_density", Some (jf default_air_density));
      ("wind_x", Some (jf 0.0));
      ("wind_y", Some (jf 0.0));
      ("wind_z", Some (jf 0.0));
      (* NEW *)
      ("projectiles", Some (jarr [ proj_to_json proj_name pv ]));
      ("annotations", Some (jarr annotations));
      ("bounces", Some (jarr bounces));
      ("collisions", Some (jarr []));
      ("queries", Some (jarr []));
    ]

(* ══════════════════════════════════════════════════════════════
   game block → one JSON scenario object
   ══════════════════════════════════════════════════════════════ *)

let game_to_json env planet level_e lives_e =
  let g = planet_gravity planet in
  let level = int_of_float (je_expr env level_e) in
  let lives = int_of_float (je_expr env lives_e) in
  jobj
    [
      ("mode", Some (js "game"));
      ("label", Some (js (Printf.sprintf "Game: %s  Lv%d" planet level)));
      ("planet", Some (js planet));
      ("gravity", Some (jf g));
      ("level", Some (string_of_int level));
      ("lives", Some (string_of_int lives));
      ("targets", Some (jarr []));
      ("walls", Some (jarr []));
    ]

(* ══════════════════════════════════════════════════════════════
   Main traversal
   ══════════════════════════════════════════════════════════════ *)

let rec collect env stmts sim_counter acc =
  match stmts with
  | [] -> (acc, env)
  | Projectile
      {
        name;
        angle;
        azimuth;
        speed;
        launch_from;
        mass;
        drag_coeff;
        cross_section;
      }
    :: rest ->
      let a = je_expr env angle in
      let az = match azimuth with Some e -> je_expr env e | None -> 0.0 in
      let s = je_expr env speed in
      let x, y, z, t =
        match launch_from with
        | None -> (0.0, 0.0, 0.0, 0.0)
        | Some (ex, ey, ez, et) ->
            (je_expr env ex, je_expr env ey, je_expr env ez, je_expr env et)
      in
      let pv : projectile_val =
        {
          angle = a;
          azimuth = az;
          speed = s;
          launch_from = (x, y, z, t);
          mass = Option.map (je_expr env) mass;
          drag_coeff = Option.map (je_expr env) drag_coeff;
          cross_section = Option.map (je_expr env) cross_section;
        }
      in
      collect (add_projectile name pv env) rest sim_counter acc
  | Simulate stmts2 :: rest ->
      let n = sim_counter + 1 in
      let scen = simulate_to_json env stmts2 n in
      collect env rest n (acc @ [ scen ])
  | Fork (proj_name, branches) :: rest ->
      let pv = get_projectile proj_name env.projectiles in
      let scens = List.map (fork_branch_to_json env proj_name pv) branches in
      collect env rest sim_counter (acc @ scens)
  | Game { planet; level; lives } :: rest ->
      let scen = game_to_json env planet level lives in
      collect env rest sim_counter (acc @ [ scen ])
  | Let (name, e) :: rest ->
      collect (add_var name (je_expr env e) env) rest sim_counter acc
  | Set (name, e) :: rest ->
      collect (update_var name (je_expr env e) env) rest sim_counter acc
  | For (var, start_e, end_e, step_e, body) :: rest ->
      let sv = je_expr env start_e in
      let ev_ = je_expr env end_e in
      let step = je_expr env step_e in
      let inner_acc, _env2, n2 =
        let rec loop i cur_env ctr accum =
          if i > ev_ then (accum, cur_env, ctr)
          else
            let env_i = add_var var i cur_env in
            let new_scens, env_after = collect env_i body ctr [] in
            loop (i +. step) env_after
              (ctr + List.length new_scens)
              (accum @ new_scens)
        in
        loop sv env sim_counter []
      in
      collect { env with vars = env.vars } rest n2 (acc @ inner_acc)
  | Repeat (n_e, body) :: rest ->
      let n = int_of_float (je_expr env n_e) in
      let inner_acc, env2, n2 =
        let rec loop i cur_env ctr accum =
          if i <= 0 then (accum, cur_env, ctr)
          else
            let new_scens, env_after = collect cur_env body ctr [] in
            loop (i - 1) env_after
              (ctr + List.length new_scens)
              (accum @ new_scens)
        in
        loop n env sim_counter []
      in
      collect env2 rest n2 (acc @ inner_acc)
  | While (cond, body) :: rest ->
      let inner_acc, env2, n2 =
        let rec loop cur_env ctr accum =
          if not (je_cond cur_env cond) then (accum, cur_env, ctr)
          else
            let new_scens, env_after = collect cur_env body ctr [] in
            loop env_after (ctr + List.length new_scens) (accum @ new_scens)
        in
        loop env sim_counter []
      in
      collect env2 rest n2 (acc @ inner_acc)
  | IfElse (cond, tbody, fbody_opt) :: rest ->
      let branch =
        if je_cond env cond then tbody
        else match fbody_opt with Some fb -> fb | None -> []
      in
      let inner_acc, env2 = collect env branch sim_counter [] in
      let n2 = sim_counter + List.length inner_acc in
      collect env2 rest n2 (acc @ inner_acc)

(* ══════════════════════════════════════════════════════════════
   Public entry point
   ══════════════════════════════════════════════════════════════ *)

let emit_json (program : Ast.program) : string =
  let ready_env, _ = Eval.eval_stmts_and_collect Env.empty_env program in
  let scenarios, _ = collect ready_env program 0 [] in
  jarr scenarios
