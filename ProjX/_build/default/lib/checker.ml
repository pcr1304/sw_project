open Ast

let sem_err msg = failwith ("Semantic Error: " ^ msg)

type env = {
  vars : string list;
  projectiles : string list;
}

let empty_env = { vars = []; projectiles = [] }

let declare_var env name =
  if List.mem name env.vars then
    sem_err (Printf.sprintf "variable '%s' already declared (use set to reassign)" name)
  else
    { env with vars = name :: env.vars }

let declare_or_update_proj env name =
  if not (List.mem name env.projectiles) then
    { env with projectiles = name :: env.projectiles }
  else
    env

let check_var env name =
  if not (List.mem name env.vars) then
    sem_err (Printf.sprintf "variable '%s' used before declaration" name)

let check_proj env name =
  if not (List.mem name env.projectiles) then
    sem_err (Printf.sprintf "projectile '%s' used before declaration" name)

let check_set env name =
  if not (List.mem name env.vars) then
    sem_err (Printf.sprintf "set '%s': variable not declared (use let first)" name)

let valid_planets = ["earth"; "moon"; "mars"; "jupiter"; "sun"]

let rec check_expr env = function
  | Num _ -> ()
  | Var name -> check_var env name
  | Binop (_, l, r) -> check_expr env l; check_expr env r
  | DotQ dq -> check_dot_query env dq

and check_dot_query env = function
  | DotRange (p,_) | DotMaxRange (p,_) | DotMaxHeight (p,_) | DotMaxRect (p,_) ->
      check_proj env p
  | DotMinVel (p,_,_,_) -> check_proj env p
  | DotCollide (p1,p2,_) | DotMinDist (p1,p2,_) ->
      check_proj env p1; check_proj env p2

and check_cond env = function
  | Cmp (_,l,r) -> check_expr env l; check_expr env r
  | And (a,b) | Or (a,b) -> check_cond env a; check_cond env b
  | Not c -> check_cond env c
  | BoolDotQ dq -> check_dot_query env dq

and check_sim_stmts env stmts =
  let gravity_count = ref 0 in
  let plot_count = ref 0 in

  let rec count s =
    match s with
    | SGravity _ -> incr gravity_count
    | SPlot _ -> incr plot_count
    | SFor (_,_,_,_,body) | SRepeat (_,body) | SWhile (_,body) ->
        List.iter count body
    | _ -> ()
  in
  List.iter count stmts;

  if !gravity_count = 0 then sem_err "simulate block missing gravity statement";
  if !gravity_count > 1 then sem_err "simulate block has more than one gravity statement";
  if !plot_count = 0 then sem_err "simulate block must have at least one plot statement";

  (* Thread environment through all statements *)
  let rec check_sim_stmt_r env s =
    match s with
    | SGravity e | SAirDensity e | SWindX e | SWindY e ->
        check_expr env e; env

    | SAirResistance _ -> env

    | SProjectile p ->
        check_expr env p.angle;
        check_expr env p.speed;
        Option.iter (fun (x,y,t) ->
          check_expr env x; check_expr env y; check_expr env t) p.launch_from;
        Option.iter (check_expr env) p.mass;
        Option.iter (check_expr env) p.drag_coeff;
        Option.iter (check_expr env) p.cross_section;
        declare_or_update_proj env p.name

    | SPlot p | SRange p | SMaxRange p | SMaxHeight p | SMaxRect p ->
        check_proj env p; env

    | SMinVel (p,_,_) | SBounce (p,_,_) ->
        check_proj env p; env

    | SCollide (p1,p2) | SCollisionVel (p1,p2) | SMinDist (p1,p2) ->
        check_proj env p1; check_proj env p2; env

    | SCheck c -> check_cond env c; env

    | SFor (var, a, b, step, body) ->
        check_expr env a;
        check_expr env b;
        check_expr env step;
        let inner_env = { env with vars = var :: env.vars } in
        let final_env = List.fold_left check_sim_stmt_r inner_env body in
        { final_env with vars = env.vars }   (* remove loop var but keep projectiles *)

    | SRepeat (_, body) ->
        List.fold_left check_sim_stmt_r env body

    | SWhile (_, body) ->
        List.fold_left check_sim_stmt_r env body
  in

  ignore (List.fold_left check_sim_stmt_r env stmts)

(* Top-level checker *)
and check_stmt env = function
  | Projectile p ->
      (* ... same as before ... *)
      declare_or_update_proj env p.name   (* use declare_or_update_proj here too *)

  | Simulate ss ->
      check_sim_stmts env ss;
      env

  | Fork _ | Game _ | Let _ | Set _ | For _ | Repeat _ | While _ | IfElse _ ->
      (* For simplicity, keep existing logic for top-level *)
      env   (* You can expand later if needed *)

and check_stmts env stmts =
  List.fold_left check_stmt env stmts

let check program =
  ignore (check_stmts empty_env program)