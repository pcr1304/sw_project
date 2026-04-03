(* ── ProjX Evaluator ── *)

open Ast
open Env

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
       | Div -> v1 /. v2)

  | DotQ dq ->
      (match dq with
       | DotRange (p, _) ->
           Printf.printf "[DotQuery] Range of %s requested\n" p;
           0.0

       | DotMaxRange (p, _) ->
           Printf.printf "[DotQuery] MaxRange of %s requested\n" p;
           0.0

       | DotMaxHeight (p, _) ->
           Printf.printf "[DotQuery] MaxHeight of %s requested\n" p;
           0.0

       | DotMaxRect (p, _) ->
           Printf.printf "[DotQuery] MaxRect of %s requested\n" p;
           0.0

       | DotMinVel (p, _, _, _) ->
           Printf.printf "[DotQuery] MinVel of %s requested\n" p;
           0.0

       | DotCollide (p1, p2, _) ->
           Printf.printf "[DotQuery] Collision check %s and %s\n" p1 p2;
           0.0

       | DotMinDist (p1, p2, _) ->
           Printf.printf "[DotQuery] MinDist between %s and %s\n" p1 p2;
           0.0
      )


(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate conditions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec eval_cond env c =
  match c with
  | Cmp (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
       | Eq  -> v1 = v2
       | Neq -> v1 <> v2
       | Lt  -> v1 < v2
       | Gt  -> v1 > v2
       | Leq -> v1 <= v2
       | Geq -> v1 >= v2)

  | And (c1, c2) ->
      (eval_cond env c1) && (eval_cond env c2)

  | Or (c1, c2) ->
      (eval_cond env c1) || (eval_cond env c2)

  | Not c1 ->
      not (eval_cond env c1)

  | BoolDotQ dq ->
      let v = eval_expr env (DotQ dq) in
      v > 0.0


(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate statements
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec eval_stmt env stmt =
  match stmt with

  (* let x = expr *)
  | Let (name, e) ->
      let value = eval_expr env e in
      add_var name value env

  (* set x = expr *)
  | Set (name, e) ->
      let value = eval_expr env e in
      update_var name value env


  (* projectile *)
  | Projectile { name; angle; speed; launch_from } ->
      let a = eval_expr env angle in
      let s = eval_expr env speed in

      let (x, y, t) =
        match launch_from with
        | None -> (0.0, 0.0, 0.0)
        | Some (ex, ey, et) ->
            (eval_expr env ex,
             eval_expr env ey,
             eval_expr env et)
      in

      let proj = {
        angle = a;
        speed = s;
        launch_from = (x, y, t);
      } in

      Printf.printf "Projectile %s created\n" name;
      add_projectile name proj env

  (* if-else *)
  | IfElse (cond, tblock, fblock_opt) ->
      if eval_cond env cond then
        eval_stmts env tblock
      else
        (match fblock_opt with
         | None -> env
         | Some fb -> eval_stmts env fb)

  (* while loop *)
  | While (cond, body) ->
      let rec loop env =
        if eval_cond env cond then
          loop (eval_stmts env body)
        else env
      in
      loop env

  (* repeat loop *)
  | Repeat (e, body) ->
      let n = int_of_float (eval_expr env e) in
      let rec loop i env =
        if i <= 0 then env
        else loop (i - 1) (eval_stmts env body)
      in
      loop n env

  (* for loop *)
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

  (* simulate (structure only) *)
  | Simulate stmts ->
      print_endline "Simulate block started";

      List.iter (function
        | SGravity e ->
            let g = eval_expr env e in
            Printf.printf "Gravity set to %.2f\n" g

        | SPlot p ->
            Printf.printf "Plotting projectile %s\n" p

        | _ ->
            print_endline "Simulate feature not implemented yet"
      ) stmts;

      print_endline "Simulate block finished";
      env

  (* fork *)
  | Fork (name, _) ->
      Printf.printf "Fork executed on %s\n" name;
      env

  (* game *)
  | Game { planet; level; lives } ->
      let lvl = eval_expr env level in
      let lvs = eval_expr env lives in
      Printf.printf "Game on %s | level = %.2f | lives = %.2f\n"
        planet lvl lvs;
      env


(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Evaluate list of statements
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and eval_stmts env stmts =
  List.fold_left eval_stmt env stmts


(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Entry point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let eval_program prog =
  let _ = eval_stmts empty_env prog in
  ()