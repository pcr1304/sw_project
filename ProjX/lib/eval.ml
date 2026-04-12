open Ast
open Env
open Physics

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   JSON Helpers
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let json_of_points pts =
  `List (List.map (fun (x, y) -> `List [`Float x; `Float y]) pts)

let json_of_drag_params (p : drag_params) =
  `Assoc [ ("enabled", `Bool p.enabled);
           ("air_density", `Float p.air_density); 
           ("wind_x", `Float p.wind_x);
           ("wind_y", `Float p.wind_y)
         ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate expressions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec eval_expr env exp =
  match exp with
  | Num n -> n
  | Var name -> get_var name env.vars
  | Binop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
       | Add -> v1 +. v2
       | Sub -> v1 -. v2
       | Mul -> v1 *. v2
       | Div -> if v2 = 0.0 then failwith "Division by zero" else v1 /. v2)
  | DotQ dq -> eval_dot_query env dq

and eval_dot_query env dq =
  match dq with
  | DotRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = eval_gopt env g_opt in
      let (x0, _, _) = proj.launch_from in
      let r = range proj.angle proj.speed g in
      r +. x0
  | DotMaxRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = eval_gopt env g_opt in
      max_range proj.speed g
  | DotMaxHeight (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = eval_gopt env g_opt in
      let (_, y0, _) = proj.launch_from in
      max_height proj.angle proj.speed g +. y0
  | DotMaxRect (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = eval_gopt env g_opt in
      let (x0, y0, _) = proj.launch_from in
      let (area, _, _, _) = max_rectangle proj.angle proj.speed g x0 y0 in
      area
  | DotMinVel (p, x_e, h_e, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g = eval_gopt env g_opt in
      let tx = eval_expr env x_e in
      let th = eval_expr env h_e in
      let (x0, y0, _) = proj.launch_from in
      min_vel proj.angle g tx th x0 y0
  | DotCollide (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g = eval_gopt env g_opt in
      let (x01, y01, _) = proj1.launch_from in
      let (x02, y02, _) = proj2.launch_from in
      let (hit, _, _, _) = collide ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01 ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:g in
      if hit then 1.0 else 0.0
  | DotMinDist (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g = eval_gopt env g_opt in
      let (x01, y01, _) = proj1.launch_from in
      let (x02, y02, _) = proj2.launch_from in
      min_dist ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01 ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:g

and eval_gopt env g_opt =
  match g_opt with
  | Some e -> eval_expr env e
  | None -> 9.8

and eval_cond env c =
  match c with
  | Cmp (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
       | Eq -> v1 = v2 | Neq -> v1 <> v2 | Lt -> v1 < v2
       | Gt -> v1 > v2 | Leq -> v1 <= v2 | Geq -> v1 >= v2)
  | And (c1, c2) -> eval_cond env c1 && eval_cond env c2
  | Or (c1, c2) -> eval_cond env c1 || eval_cond env c2
  | Not c1 -> not (eval_cond env c1)
  | BoolDotQ dq -> eval_dot_query env dq > 0.0

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate block (Returns JSON)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_simulate env stmts =
  let gravity = ref 9.8 in
  let air_resistance = ref false in
  let air_density = ref default_air_density in
  let wind_x = ref 0.0 in
  let wind_y = ref 0.0 in

  (* First pass: collect physics settings at top level only *)
  let rec collect_settings = function
    | [] -> ()
    | s :: rest ->
        (match s with
         | SGravity e -> gravity := eval_expr env e
         | SAirResistance b -> air_resistance := b
         | SAirDensity e -> air_density := eval_expr env e
         | SWindX e -> wind_x := eval_expr env e
         | SWindY e -> wind_y := eval_expr env e
         | _ -> ());
        collect_settings rest
  in
  collect_settings stmts;

  let drag_params = {
    enabled = !air_resistance;
    air_density = !air_density;
    wind_x = !wind_x;
    wind_y = !wind_y;
  } in
  let session_data = ref [] in

  (* Recursive executor: handles all sim_stmts including loops *)
  let rec exec_sim env_inner s =
    match s with
    | SGravity _ | SAirResistance _ | SAirDensity _
    | SWindX _ | SWindY _ -> env_inner  (* already collected above *)

    | SProjectile { name; angle; speed; launch_from; mass; drag_coeff; cross_section } ->
        let a = eval_expr env_inner angle in
        let s_val = eval_expr env_inner speed in
        let (x, y, t) = match launch_from with
          | Some (ex, ey, et) ->
              (eval_expr env_inner ex, eval_expr env_inner ey, eval_expr env_inner et)
          | None -> (0.0, 0.0, 0.0)
        in
        let proj = {
          angle = a;
          speed = s_val;
          launch_from = (x, y, t);
          mass = Option.map (eval_expr env_inner) mass;
          drag_coeff = Option.map (eval_expr env_inner) drag_coeff;
          cross_section = Option.map (eval_expr env_inner) cross_section;
        } in
        add_projectile name proj env_inner

    | SPlot p ->
        let proj = get_projectile p env_inner.projectiles in
        let (x0, y0, _) = proj.launch_from in
        let pts, r, mh =
          if !air_resistance then
            let (m, cd, a) = get_drag_params proj in
            (Physics.arc_points_with_drag proj.angle proj.speed !gravity x0 y0 50 drag_params m cd a,
             Physics.range_with_drag proj.angle proj.speed !gravity x0 y0 drag_params m cd a,
             Physics.max_height_with_drag proj.angle proj.speed !gravity y0 drag_params m cd a)
          else
            (Physics.arc_points proj.angle proj.speed !gravity x0 y0 50,
             Physics.range proj.angle proj.speed !gravity +. x0,
             Physics.max_height proj.angle proj.speed !gravity +. y0)
        in
        session_data := `Assoc [ ("plot", `Assoc [
          ("name", `String p);
          ("range", `Float r);
          ("max_height", `Float mh);
          ("points", json_of_points pts)
        ])] :: !session_data;
        env_inner

    | SRange p ->
        let proj = get_projectile p env_inner.projectiles in
        let (x0, y0, _) = proj.launch_from in
        let r =
          if !air_resistance then
            let (m, cd, a) = get_drag_params proj in
            range_with_drag proj.angle proj.speed !gravity x0 y0 drag_params m cd a
          else
            range proj.angle proj.speed !gravity +. x0
        in
        session_data := `Assoc [("range", `Assoc [("name", `String p); ("value", `Float r)])] :: !session_data;
        env_inner

    | SMaxRange p ->
        let proj = get_projectile p env_inner.projectiles in
        let r = max_range proj.speed !gravity in
        session_data := `Assoc [("max_range", `Assoc [("name", `String p); ("value", `Float r)])] :: !session_data;
        env_inner

    | SMaxHeight p ->
        let proj = get_projectile p env_inner.projectiles in
        let (_, y0, _) = proj.launch_from in
        let mh = max_height proj.angle proj.speed !gravity +. y0 in
        session_data := `Assoc [("max_height", `Assoc [("name", `String p); ("value", `Float mh)])] :: !session_data;
        env_inner

    | SMaxRect p ->
        let proj = get_projectile p env_inner.projectiles in
        let (x0, y0, _) = proj.launch_from in
        let (area, _, _, _) = max_rectangle proj.angle proj.speed !gravity x0 y0 in
        session_data := `Assoc [("max_rect", `Assoc [("name", `String p); ("value", `Float area)])] :: !session_data;
        env_inner

    | SMinVel (p, x_e, h_e) ->
        let proj = get_projectile p env_inner.projectiles in
        let tx = eval_expr env_inner x_e in
        let th = eval_expr env_inner h_e in
        let (x0, y0, _) = proj.launch_from in
        let mv = min_vel proj.angle !gravity tx th x0 y0 in
        session_data := `Assoc [("min_vel", `Assoc [("name", `String p); ("value", `Float mv)])] :: !session_data;
        env_inner

    | SCollide (p1, p2) ->
        let proj1 = get_projectile p1 env_inner.projectiles in
        let proj2 = get_projectile p2 env_inner.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let (hit, _, _, _) = collide ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:!gravity in
        session_data := `Assoc [("collide", `Assoc [
          ("p1", `String p1); ("p2", `String p2);
          ("result", `String (if hit then "HIT" else "MISS"))
        ])] :: !session_data;
        env_inner

    | SCollisionVel (p1, p2) ->
        let proj1 = get_projectile p1 env_inner.projectiles in
        let proj2 = get_projectile p2 env_inner.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let (_, vx, vy, _) = collide ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:!gravity in
        session_data := `Assoc [("collision_vel", `Assoc [
          ("p1", `String p1); ("p2", `String p2);
          ("vx", `Float vx); ("vy", `Float vy)
        ])] :: !session_data;
        env_inner

    | SMinDist (p1, p2) ->
        let proj1 = get_projectile p1 env_inner.projectiles in
        let proj2 = get_projectile p2 env_inner.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let d = min_dist ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02 ~gravity:!gravity in
        session_data := `Assoc [("min_dist", `Assoc [
          ("p1", `String p1); ("p2", `String p2); ("value", `Float d)
        ])] :: !session_data;
        env_inner

    | SBounce (p, n_e, r_e) ->
        let proj = get_projectile p env_inner.projectiles in
        let n = int_of_float (eval_expr env_inner n_e) in
        let r_val = eval_expr env_inner r_e in
        let (x0, y0, _) = proj.launch_from in
        let arcs = bounce_arcs proj.angle proj.speed !gravity r_val n x0 y0 in
        let json_arcs = List.map (fun (x, y, ang, spd) ->
          json_of_points (Physics.arc_points ang spd !gravity x y 50)
        ) arcs in
        session_data := `Assoc [("bounce", `Assoc [("name", `String p); ("arcs", `List json_arcs)])] :: !session_data;
        env_inner

    | SCheck c ->
        let res = eval_cond env_inner c in
        session_data := `Assoc [("check", `Assoc [("status", `String (if res then "PASS" else "FAIL"))])] :: !session_data;
        env_inner

    | SFor (var, s_e, e_e, step_e, body) ->
        let start_v = eval_expr env_inner s_e in
        let end_v   = eval_expr env_inner e_e in
        let step_v  = eval_expr env_inner step_e in
        let rec loop i cur_env =
          if i > end_v then cur_env
          else begin
            let loop_env = add_var var i cur_env in
            let final_env = List.fold_left exec_sim loop_env body in
            loop (i +. step_v) final_env
          end
        in
        loop start_v env_inner

    | SRepeat (n_e, body) ->
        let n = int_of_float (eval_expr env_inner n_e) in
        let rec loop i cur_env =
          if i <= 0 then cur_env
          else begin
            let final_env = List.fold_left exec_sim cur_env body in
            loop (i - 1) final_env
          end
        in
        loop n env_inner

    | SWhile (cond_e, body) ->
        let rec loop cur_env =
          if eval_cond cur_env cond_e then begin
            let final_env = List.fold_left exec_sim cur_env body in
            loop final_env
          end else cur_env
        in
        loop env_inner
  in

  ignore (List.fold_left exec_sim env stmts);

  `Assoc [ ("type", `String "simulate");
           ("gravity", `Float !gravity); 
           ("drag", json_of_drag_params drag_params); 
           ("actions", `List (List.rev !session_data))
         ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Fork block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_fork env name branches =
  let proj = get_projectile name env.projectiles in
  let branch_json = List.map (fun br ->
    (* Each branch runs its own mini-simulate, collecting actions *)
    let gravity = ref 9.8 in
    let air_resistance = ref false in
    let air_density = ref default_air_density in
    let wind_x = ref 0.0 in
    let wind_y = ref 0.0 in

    (* Collect settings from this branch's stmts *)
    List.iter (fun s -> match s with
      | SGravity e       -> gravity := eval_expr env e
      | SAirResistance b -> air_resistance := b
      | SAirDensity e    -> air_density := eval_expr env e
      | SWindX e         -> wind_x := eval_expr env e
      | SWindY e         -> wind_y := eval_expr env e
      | _                -> ()
    ) br.br_stmts;

    let drag_params = {
      enabled = !air_resistance;
      air_density = !air_density;
      wind_x = !wind_x;
      wind_y = !wind_y;
    } in
    let branch_data = ref [] in

    (* Recursive executor for branch stmts *)
    let rec exec_br env_inner s =
      match s with
      | SGravity _ | SAirResistance _ | SAirDensity _
      | SWindX _ | SWindY _ -> env_inner

      | SProjectile { name=pname; angle; speed; launch_from; mass; drag_coeff; cross_section } ->
          let a = eval_expr env_inner angle in
          let s_val = eval_expr env_inner speed in
          let (x, y, t) = match launch_from with
            | Some (ex, ey, et) ->
                (eval_expr env_inner ex, eval_expr env_inner ey, eval_expr env_inner et)
            | None -> (0.0, 0.0, 0.0)
          in
          let proj = {
            angle = a;
            speed = s_val;
            launch_from = (x, y, t);
            mass = Option.map (eval_expr env_inner) mass;
            drag_coeff = Option.map (eval_expr env_inner) drag_coeff;
            cross_section = Option.map (eval_expr env_inner) cross_section;
          } in
          add_projectile pname proj env_inner

      | SPlot p ->
          let (x0, y0, _) = proj.launch_from in
          let pts, r, mh =
            if !air_resistance then
              let (m, cd, a) = get_drag_params proj in
              (Physics.arc_points_with_drag proj.angle proj.speed !gravity x0 y0 50 drag_params m cd a,
               Physics.range_with_drag proj.angle proj.speed !gravity x0 y0 drag_params m cd a,
               Physics.max_height_with_drag proj.angle proj.speed !gravity y0 drag_params m cd a)
            else
              (Physics.arc_points proj.angle proj.speed !gravity x0 y0 50,
               Physics.range proj.angle proj.speed !gravity +. x0,
               Physics.max_height proj.angle proj.speed !gravity +. y0)
          in
          ignore p;
          branch_data := `Assoc [("plot", `Assoc [
            ("name", `String p); ("range", `Float r);
            ("max_height", `Float mh); ("points", json_of_points pts)
          ])] :: !branch_data;
          env_inner

      | SRange _ ->
          let (x0, y0, _) = proj.launch_from in
          let r =
            if !air_resistance then
              let (m, cd, a) = get_drag_params proj in
              range_with_drag proj.angle proj.speed !gravity x0 y0 drag_params m cd a
            else
              range proj.angle proj.speed !gravity +. x0
          in
          branch_data := `Assoc [("range", `Assoc [("name", `String name); ("value", `Float r)])] :: !branch_data;
          env_inner

      | SBounce (p, n_e, r_e) ->
          let n = int_of_float (eval_expr env_inner n_e) in
          let r_val = eval_expr env_inner r_e in
          let (x0, y0, _) = proj.launch_from in
          let arcs = bounce_arcs proj.angle proj.speed !gravity r_val n x0 y0 in
          let json_arcs = List.map (fun (x, y, ang, spd) ->
            json_of_points (Physics.arc_points ang spd !gravity x y 50)
          ) arcs in
          branch_data := `Assoc [("bounce", `Assoc [("name", `String p); ("arcs", `List json_arcs)])] :: !branch_data;
          env_inner

      | SCheck c ->
          let res = eval_cond env_inner c in
          branch_data := `Assoc [("check", `Assoc [("status", `String (if res then "PASS" else "FAIL"))])] :: !branch_data;
          env_inner

      | SFor (var, s_e, e_e, step_e, body) ->
          let start_v = eval_expr env_inner s_e in
          let end_v   = eval_expr env_inner e_e in
          let step_v  = eval_expr env_inner step_e in
          let rec loop i cur_env =
            if i > end_v then cur_env
            else begin
              let loop_env = add_var var i cur_env in
              let final_env = List.fold_left exec_br loop_env body in
              loop (i +. step_v) final_env
            end
          in
          loop start_v env_inner

      | SRepeat (n_e, body) ->
          let n = int_of_float (eval_expr env_inner n_e) in
          let rec loop i cur_env =
            if i <= 0 then cur_env
            else begin 
              let final_env = List.fold_left exec_br cur_env body in 
              loop (i - 1) final_env 
            end
          in
          loop n env_inner

      | SWhile (cond_e, body) ->
          let rec loop cur_env =
            if eval_cond cur_env cond_e then begin
              let final_env = List.fold_left exec_br cur_env body in 
              loop final_env
            end else cur_env
          in
          loop env_inner

      | _ -> env_inner
    in

    ignore (List.fold_left exec_br env br.br_stmts);

    (* Compute range for summary using gravity collected from this branch *)
    let r = range proj.angle proj.speed !gravity in
    let pts = Physics.arc_points proj.angle proj.speed !gravity 0.0 0.0 50 in
    `Assoc [
      ("label",   `String br.label);
      ("gravity", `Float !gravity);
      ("range",   `Float r);
      ("points",  json_of_points pts);
      ("actions", `List (List.rev !branch_data))
    ]
  ) branches in
  `Assoc [ ("type", `String "fork");
           ("projectile", `String name); 
           ("branches", `List branch_json)
         ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Top-level eval_stmt
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_stmt env stmt =
  match stmt with
  | Let (name, e) -> (add_var name (eval_expr env e) env, None)
  | Set (name, e) -> (update_var name (eval_expr env e) env, None)
  | Projectile p ->
      let a = eval_expr env p.angle in
      let s = eval_expr env p.speed in
      let (x,y,t) = match p.launch_from with Some (ex,ey,et) -> (eval_expr env ex, eval_expr env ey, eval_expr env et) | None -> (0.0,0.0,0.0) in
      let proj = { angle=a; speed=s; launch_from=(x,y,t); mass=Option.map (eval_expr env) p.mass; drag_coeff=Option.map (eval_expr env) p.drag_coeff; cross_section=Option.map (eval_expr env) p.cross_section } in
      (add_projectile p.name proj env, None)
  | Simulate stmts -> (env, Some (eval_simulate env stmts))
  | Fork (n, b) -> (env, Some (eval_fork env n b))
  | Game g ->
      let json = `Assoc [("type", `String "game"); ("planet", `String g.planet); ("level", `Float (eval_expr env g.level))] in
      (env, Some json)
  | For (v, s, e, st, body) ->
      let start_v = eval_expr env s in
      let end_v = eval_expr env e in
      let step_v = eval_expr env st in
      let rec loop i e_acc jsons =
        if i > end_v then (e_acc, List.rev jsons)
        else
          let env_with_var = add_var v i e_acc in
          let (final_env, loop_jsons) = eval_stmts_and_collect env_with_var body in
          loop (i +. step_v) final_env (loop_jsons @ jsons)
      in
      let (final_env, all_jsons) = loop start_v env [] in
      (final_env, Some (`List all_jsons))
  | IfElse (c, t, f) -> 
      let (new_env, jsons) = if eval_cond env c then eval_stmts_and_collect env t else (match f with Some b -> eval_stmts_and_collect env b | None -> (env, [])) in 
      (new_env, Some (`List jsons))
  | While (cond, body) ->
      let rec loop e_acc jsons =
        if eval_cond e_acc cond then
          let (next_env, next_jsons) = eval_stmts_and_collect e_acc body in
          loop next_env (jsons @ next_jsons)
        else (e_acc, jsons)
      in
      let (final_env, all_jsons) = loop env [] in
      (final_env, Some (`List all_jsons))
  | Repeat (e, body) -> 
      let n = int_of_float (eval_expr env e) in 
      let rec loop i e_acc jsons = 
        if i <= 0 then (e_acc, List.rev jsons) 
        else 
          let (next_env, next_jsons) = eval_stmts_and_collect e_acc body in 
          loop (i - 1) next_env (next_jsons @ jsons) 
      in 
      let (final_env, all_jsons) = loop n env [] in 
      (final_env, Some (`List all_jsons))

and eval_stmts_and_collect env stmts =
  List.fold_left (fun (e_acc, j_acc) s ->
    let (next_env, j_opt) = eval_stmt e_acc s in
    match j_opt with
    | Some j -> (next_env, j :: j_acc)
    | None -> (next_env, j_acc)
  ) (env, []) stmts
  |> (fun (e, j) -> (e, List.rev j))

let eval_program prog =
  let (_, json_list) = eval_stmts_and_collect empty_env prog in
  let final_json = `List json_list in
  print_endline (Yojson.Safe.pretty_to_string final_json)