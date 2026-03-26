(* ── ProjX v3 semantic analyser ── *)

open Ast

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Error helpers
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let sem_err msg = failwith ("Semantic Error: " ^ msg)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Environment
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type env = {
  vars        : string list;
  projectiles : string list;
}

let empty_env = { vars = []; projectiles = [] }

let declare_var env name =
  if List.mem name env.vars then
    sem_err (Printf.sprintf "variable '%s' already declared (use set to reassign)" name)
  else
    { env with vars = name :: env.vars }

let declare_proj env name =
  if List.mem name env.projectiles then
    sem_err (Printf.sprintf "projectile '%s' already declared" name)
  else
    { env with projectiles = name :: env.projectiles }

let check_var env name =
  if not (List.mem name env.vars) then
    sem_err (Printf.sprintf "variable '%s' used before declaration" name)

let check_proj env name =
  if not (List.mem name env.projectiles) then
    sem_err (Printf.sprintf "projectile '%s' used before declaration" name)

let check_set env name =
  if not (List.mem name env.vars) then
    sem_err (Printf.sprintf "set '%s': variable not declared (use let first)" name)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Valid planets
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let valid_planets = [ "earth"; "moon"; "mars"; "jupiter"; "sun" ]

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Expression checker
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec check_expr env = function
  | Num _           -> ()
  | Var name        -> check_var env name
  | Binop (_, l, r) -> check_expr env l; check_expr env r
  | DotQ dq         -> check_dot_query env dq

and check_dot_query env = function
  | DotRange     (p, g_opt)
  | DotMaxRange  (p, g_opt)
  | DotMaxHeight (p, g_opt)
  | DotMaxRect   (p, g_opt) ->
      check_proj env p;
      Option.iter (check_expr env) g_opt

  | DotMinVel (p, x, h, g_opt) ->
      check_proj env p;
      check_expr env x;
      check_expr env h;
      Option.iter (check_expr env) g_opt

  | DotCollide (p1, p2, g_opt)
  | DotMinDist (p1, p2, g_opt) ->
      check_proj env p1;
      check_proj env p2;
      Option.iter (check_expr env) g_opt

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Condition checker
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and check_cond env = function
  | Cmp (_, l, r) -> check_expr env l; check_expr env r
  | And (a, b)    -> check_cond env a; check_cond env b
  | Or  (a, b)    -> check_cond env a; check_cond env b
  | Not c         -> check_cond env c
  | BoolDotQ dq   -> check_dot_query env dq

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate block checker
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and check_sim_stmts env stmts =
  let gravity_count = ref 0 in
  let plot_count    = ref 0 in
  let plotted       = ref [] in
  List.iter (fun s ->
    match s with
    | SGravity _ -> incr gravity_count
    | SPlot p    -> incr plot_count; plotted := p :: !plotted
    | _          -> ()
  ) stmts;

  if !gravity_count = 0 then
    sem_err "simulate block missing gravity statement";
  if !gravity_count > 1 then
    sem_err "simulate block has more than one gravity statement";
  if !plot_count = 0 then
    sem_err "simulate block must have at least one plot statement";

  List.iter (fun s ->
    match s with
    | SGravity e ->
        check_expr env e

    | SPlot p ->
        check_proj env p

    | SRange p | SMaxRange p | SMaxHeight p | SMaxRect p ->
        check_proj env p

    | SMinVel (p, x, h) ->
        check_proj env p;
        check_expr env x;
        check_expr env h

    | SCollide (p1, p2) | SCollisionVel (p1, p2) | SMinDist (p1, p2) ->
        check_proj env p1;
        check_proj env p2

    | SBounce (p, n, r) ->
        check_proj env p;
        if not (List.mem p !plotted) then
          sem_err (Printf.sprintf
            "bounce '%s': projectile must be plotted in this simulate block" p);
        check_expr env n;
        check_expr env r;
        (match r with
         | Num rv ->
             if rv < 0.0 || rv > 1.0 then
               sem_err (Printf.sprintf
                 "bounce '%s': restitution %.4g is outside [0.0, 1.0]" p rv)
         | _ -> ())

    | SCheck c ->
        check_cond env c
  ) stmts

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Top-level statement checker
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and check_stmt env stmt =
  match stmt with

  | Projectile { name; angle; speed; launch_from } ->
      check_expr env angle;
      check_expr env speed;
      (match launch_from with
       | None -> ()
       | Some (x, y, t) ->
           check_expr env x;
           check_expr env y;
           check_expr env t);
      (match angle with
       | Num a ->
           if a < 0.0 || a > 90.0 then
             sem_err (Printf.sprintf
               "projectile '%s': angle %.4g is outside valid range [0, 90]" name a)
       | _ -> ());
      declare_proj env name

  | Simulate ss ->
      check_sim_stmts env ss;
      env

  | Fork (name, branches) ->
      check_proj env name;
      if branches = [] then
        sem_err (Printf.sprintf "fork '%s': must have at least one branch" name);
      let seen_labels = ref [] in
      List.iter (fun br ->
        let lbl    = br.label in
        let grav   = br.br_gravity in
        let bounce = br.br_bounce in
        if List.mem lbl !seen_labels then
          sem_err (Printf.sprintf
            "fork '%s': duplicate branch label \"%s\"" name lbl);
        seen_labels := lbl :: !seen_labels;
        check_expr env grav;
        (match bounce with
         | None -> ()
         | Some (n, r) ->
             check_expr env n;
             check_expr env r;
             (match r with
              | Num rv ->
                  if rv < 0.0 || rv > 1.0 then
                    sem_err (Printf.sprintf
                      "fork '%s' branch \"%s\": restitution %.4g outside [0.0, 1.0]"
                      name lbl rv)
              | _ -> ()))
      ) branches;
      env

  | Game { planet; level; lives } ->
      if not (List.mem planet valid_planets) then
        sem_err (Printf.sprintf
          "game: unknown planet '%s'. Valid planets: %s"
          planet (String.concat ", " valid_planets));
      check_expr env level;
      check_expr env lives;
      env

  | Let (name, e) ->
      check_expr env e;
      declare_var env name

  | Set (name, e) ->
      check_set env name;
      check_expr env e;
      env

  | For (var, a, b, s, body) ->
      check_expr env a;
      check_expr env b;
      check_expr env s;
      let inner_env =
        { env with vars =
            if List.mem var env.vars then env.vars
            else var :: env.vars }
      in
      ignore (check_stmts inner_env body);
      env

  | Repeat (n, body) ->
      check_expr env n;
      ignore (check_stmts env body);
      env

  | While (c, body) ->
      check_cond env c;
      ignore (check_stmts env body);
      env

  | IfElse (c, tbody, fbody_opt) ->
      check_cond env c;
      ignore (check_stmts env tbody);
      (match fbody_opt with
       | None    -> ()
       | Some fb -> ignore (check_stmts env fb));
      env

and check_stmts env stmts =
  List.fold_left check_stmt env stmts

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Entry point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let check program =
  ignore (check_stmts empty_env program)