(* ── ProjX v3 Evaluator ── *)

open Ast
open Env
open Physics

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate expressions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec eval_expr env exp =
  match exp with
  | Num n -> n

  | Var name ->
      get_var name env.vars

  | Binop (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
       | Add -> v1 +. v2
       | Sub -> v1 -. v2
       | Mul -> v1 *. v2
       | Div ->
           if v2 = 0.0 then failwith "Division by zero"
           else v1 /. v2)

  | DotQ dq ->
      eval_dot_query env dq

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Dot query evaluation
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_dot_query env dq =
  match dq with

  | DotRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g    = eval_gopt env g_opt in
      let (x0, _, _) = proj.launch_from in
      let r = range proj.angle proj.speed g in
      Printf.printf "[range.%s] = %.4f m\n" p (r +. x0);
      r

  | DotMaxRange (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g    = eval_gopt env g_opt in
      let mr   = max_range proj.speed g in
      Printf.printf "[max_range.%s] = %.4f m at 45°\n" p mr;
      mr

  | DotMaxHeight (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g    = eval_gopt env g_opt in
      let (_, y0, _) = proj.launch_from in
      let mh   = max_height proj.angle proj.speed g +. y0 in
      Printf.printf "[max_height.%s] = %.4f m\n" p mh;
      mh

  | DotMaxRect (p, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g    = eval_gopt env g_opt in
      let (x0, y0, _) = proj.launch_from in
      let (area, _, _, _) = max_rectangle proj.angle proj.speed g x0 y0 in
      Printf.printf "[max_rectangle.%s] = %.4f m²\n" p area;
      area

  | DotMinVel (p, x_e, h_e, g_opt) ->
      let proj = get_projectile p env.projectiles in
      let g    = eval_gopt env g_opt in
      let tx   = eval_expr env x_e in
      let th   = eval_expr env h_e in
      let (x0, y0, _) = proj.launch_from in
      let mv   = min_vel proj.angle g tx th x0 y0 in
      Printf.printf "[min_vel.%s] = %.4f m/s\n" p mv;
      mv

  | DotCollide (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g     = eval_gopt env g_opt in
      let (x01, y01, _) = proj1.launch_from in
      let (x02, y02, _) = proj2.launch_from in
      let (hit, _, _, _) = collide
        ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
        ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02
        ~gravity:g
      in
      Printf.printf "[collide.(%s,%s)] = %b\n" p1 p2 hit;
      if hit then 1.0 else 0.0

  | DotMinDist (p1, p2, g_opt) ->
      let proj1 = get_projectile p1 env.projectiles in
      let proj2 = get_projectile p2 env.projectiles in
      let g     = eval_gopt env g_opt in
      let (x01, y01, _) = proj1.launch_from in
      let (x02, y02, _) = proj2.launch_from in
      let md = min_dist
        ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
        ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02
        ~gravity:g
      in
      Printf.printf "[min_dist.(%s,%s)] = %.4f m\n" p1 p2 md;
      md

(* resolve optional gravity — use passed value or default 9.8 *)
and eval_gopt env g_opt =
  match g_opt with
  | Some e -> eval_expr env e
  | None   -> 9.8

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate conditions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_cond env c =
  match c with
  | Cmp (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
       | Eq  -> v1 =  v2
       | Neq -> v1 <> v2
       | Lt  -> v1 <  v2
       | Gt  -> v1 >  v2
       | Leq -> v1 <= v2
       | Geq -> v1 >= v2)

  | And (c1, c2) -> eval_cond env c1 && eval_cond env c2
  | Or  (c1, c2) -> eval_cond env c1 || eval_cond env c2
  | Not c1       -> not (eval_cond env c1)

  | BoolDotQ dq  ->
      let v = eval_dot_query env dq in
      v > 0.0

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_simulate env stmts =
  (* first pass — get gravity *)
  let gravity = List.fold_left (fun acc s ->
    match s with
    | SGravity e -> eval_expr env e
    | _          -> acc
  ) 9.8 stmts in

  Printf.printf "\n── simulate (g = %.2f) ──\n" gravity;
  let range_annos = ref [] in
  let max_h_annos = ref [] in
  let bnc_annos = ref [] in
  let col_annos = ref [] in

  (* second pass — execute each statement *)
  List.iter (fun s ->
    match s with
    | SGravity _ -> ()   (* already handled *)

    | SPlot p ->
        let proj = get_projectile p env.projectiles in
        let (x0, y0, _) = proj.launch_from in
        let r  = range proj.angle proj.speed gravity in
        let mh = max_height proj.angle proj.speed gravity in
        Printf.printf "plot %s: range=%.2f m  max_height=%.2f m\n" p r mh;
        let pts = arc_points proj.angle proj.speed gravity x0 y0 50 in
        ignore pts  (* passed to canvas later *)

    | SRange p ->
        let proj = get_projectile p env.projectiles in
        let (x0, _, _) = proj.launch_from in
        let r = range proj.angle proj.speed gravity +. x0 in
        range_annos := (p, r) :: !range_annos;
        Printf.printf "range %s = %.4f m\n" p r

    | SMaxRange p ->
        let proj = get_projectile p env.projectiles in
        let mr   = max_range proj.speed gravity in
        Printf.printf "max_range %s = %.4f m (at %.1f°)\n"
          p mr max_range_angle

    | SMaxHeight p ->
        let proj = get_projectile p env.projectiles in
        let (_, y0, _) = proj.launch_from in
        let mh = max_height proj.angle proj.speed gravity +. y0 in
        max_h_annos := (p, mh) :: !max_h_annos;
        Printf.printf "max_height %s = %.4f m\n" p mh

    | SMaxRect p ->
        let proj = get_projectile p env.projectiles in
        let (x0, y0, _) = proj.launch_from in
        let (area, _, _, _) = max_rectangle proj.angle proj.speed gravity x0 y0 in
        Printf.printf "max_rectangle %s = %.4f m²\n" p area

    | SMinVel (p, x_e, h_e) ->
        let proj = get_projectile p env.projectiles in
        let tx   = eval_expr env x_e in
        let th   = eval_expr env h_e in
        let (x0, y0, _) = proj.launch_from in
        let mv   = min_vel proj.angle gravity tx th x0 y0 in
        Printf.printf "min_vel %s tower (%.1f, %.1f) = %.4f m/s\n" p tx th mv

    | SCollide (p1, p2) ->
        let proj1 = get_projectile p1 env.projectiles in
        let proj2 = get_projectile p2 env.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let (hit, t, cx, cy) = collide
          ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02
          ~gravity
        in
        if hit then begin
          col_annos := (p1, p2, t, cx, cy) :: !col_annos;
          Printf.printf "collide %s %s: YES at t=%.2fs (%.1f, %.1f)\n"
            p1 p2 t cx cy
        end else
          Printf.printf "collide %s %s: NO\n" p1 p2

    | SCollisionVel (p1, p2) ->
        let proj1 = get_projectile p1 env.projectiles in
        let proj2 = get_projectile p2 env.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let (hit, t, _, _) = collide
          ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02
          ~gravity
        in
        if hit then begin
          let ((vx1, vy1), (vx2, vy2)) = collision_vel
            ~angle1:proj1.angle ~speed1:proj1.speed
            ~angle2:proj2.angle ~speed2:proj2.speed
            ~gravity t
          in
          Printf.printf "collision_vel %s: (%.2f, %.2f) m/s\n" p1 vx1 vy1;
          Printf.printf "collision_vel %s: (%.2f, %.2f) m/s\n" p2 vx2 vy2
        end else
          Printf.printf "collision_vel %s %s: no collision\n" p1 p2

    | SMinDist (p1, p2) ->
        let proj1 = get_projectile p1 env.projectiles in
        let proj2 = get_projectile p2 env.projectiles in
        let (x01, y01, _) = proj1.launch_from in
        let (x02, y02, _) = proj2.launch_from in
        let md = min_dist
          ~angle1:proj1.angle ~speed1:proj1.speed ~x01 ~y01
          ~angle2:proj2.angle ~speed2:proj2.speed ~x02 ~y02
          ~gravity
        in
        Printf.printf "min_dist %s %s = %.4f m\n" p1 p2 md

    | SBounce (p, n_e, r_e) ->
        let proj = get_projectile p env.projectiles in
        let n    = int_of_float (eval_expr env n_e) in
        let r    = eval_expr env r_e in
        let (x0, y0, _) = proj.launch_from in
        let arcs = bounce_arcs proj.angle proj.speed gravity r n x0 y0 in
        bnc_annos := (p, arcs) :: !bnc_annos;
        Printf.printf "bounce %s: %d hops restitution=%.2f\n" p n r;
        List.iteri (fun i (x, _, ang, spd) ->
          let rng = range ang spd gravity in
          Printf.printf "  hop %d: x0=%.2f range=%.2f\n" (i+1) x rng
        ) arcs

    | SCheck c ->
        let result = eval_cond env c in
        Printf.printf "check: %s\n" (if result then "PASS ✓" else "FAIL ✗")

  ) stmts;
  let label = Printf.sprintf "Sim (g=%.1f)" gravity in
  emitted_scenarios := !emitted_scenarios @ [SimScenario (label, gravity, env.projectiles, !range_annos, !max_h_annos, !bnc_annos, !col_annos)];
  Printf.printf "── end simulate ──\n\n"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Fork block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_fork env name branches =
  let proj = get_projectile name env.projectiles in
  Printf.printf "\n── fork %s ──\n" name;
  List.iter (fun br ->
    let g  = eval_expr env br.br_gravity in
    let r  = range proj.angle proj.speed g in
    let mh = max_height proj.angle proj.speed g in
    let bnc_annos = ref [] in
    Printf.printf "branch \"%s\" (g=%.2f): range=%.2f m  max_height=%.2f m\n"
      br.label g r mh;
    (match br.br_bounce with
     | None -> ()
     | Some (n_e, r_e) ->
         let n    = int_of_float (eval_expr env n_e) in
         let rest = eval_expr env r_e in
         let (x0, y0, _) = proj.launch_from in
         let arcs = bounce_arcs proj.angle proj.speed g rest n x0 y0 in
         bnc_annos := [(name, arcs)];
         Printf.printf "  bounce: %d hops restitution=%.2f\n" n rest;
         List.iteri (fun i (x, _, ang, spd) ->
           let rng = range ang spd g in
           Printf.printf "    hop %d: x0=%.2f range=%.2f\n" (i+1) x rng
         ) arcs);
    let label = Printf.sprintf "Fork: %s (g=%.1f)" br.label g in
    emitted_scenarios := !emitted_scenarios @ [SimScenario (label, g, [(name, proj)], [(name, r)], [(name, mh)], !bnc_annos, [])]
  ) branches;
  Printf.printf "── end fork ──\n\n"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate statements
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_stmt env stmt =
  match stmt with

  | Let (name, e) ->
      let value = eval_expr env e in
      Printf.printf "let %s = %.4f\n" name value;
      add_var name value env

  | Set (name, e) ->
      let value = eval_expr env e in
      Printf.printf "set %s = %.4f\n" name value;
      update_var name value env

  | Projectile { name; angle; speed; launch_from } ->
      let a = eval_expr env angle in
      let s = eval_expr env speed in
      let (x, y, t) =
        match launch_from with
        | None            -> (0.0, 0.0, 0.0)
        | Some (ex, ey, et) ->
            (eval_expr env ex,
             eval_expr env ey,
             eval_expr env et)
      in
      let proj = { angle = a; speed = s; launch_from = (x, y, t) } in
      Printf.printf "projectile %s: angle=%.1f° speed=%.1f m/s\n" name a s;
      add_projectile name proj env

  | Simulate stmts ->
      eval_simulate env stmts;
      env

  | Fork (name, branches) ->
      eval_fork env name branches;
      env

  | Game { planet; level; lives } ->
      let g   = planet_gravity planet in
      let lvl = eval_expr env level in
      let lvs = eval_expr env lives in
      Printf.printf "\n── game mode ──\n";
      Printf.printf "planet=%s (g=%.2f)  level=%.0f  lives=%.0f\n"
        planet g lvl lvs;
      let label = Printf.sprintf "Game: %s Lv%.0f" planet lvl in
      emitted_scenarios := !emitted_scenarios @ [GameScenario (label, planet, g, lvl, lvs)];
      Printf.printf "── end game ──\n\n";
      env

  | IfElse (cond, tblock, fblock_opt) ->
      if eval_cond env cond then
        eval_stmts env tblock
      else
        (match fblock_opt with
         | None    -> env
         | Some fb -> eval_stmts env fb)

  | While (cond, body) ->
      let rec loop env =
        if eval_cond env cond then loop (eval_stmts env body)
        else env
      in
      loop env

  | Repeat (e, body) ->
      let n = int_of_float (eval_expr env e) in
      let rec loop i env =
        if i <= 0 then env
        else loop (i - 1) (eval_stmts env body)
      in
      loop n env

  | For (var, start_e, end_e, step_e, body) ->
      let start_v = eval_expr env start_e in
      let end_v   = eval_expr env end_e in
      let step_v  = eval_expr env step_e in
      let rec loop i env =
        if i > end_v then env
        else
          let env = add_var var i env in
          let env = eval_stmts env body in
          loop (i +. step_v) env
      in
      loop start_v env

and eval_stmts env stmts =
  List.fold_left eval_stmt env stmts

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Entry point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let eval_program prog =
  ignore (eval_stmts empty_env prog)
