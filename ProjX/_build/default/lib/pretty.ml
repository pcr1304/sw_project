(* ── ProjX v3 pretty printer ── *)

open Ast

(* ── indentation helper ── *)
let ind n = String.make (n * 2) ' '

(* ── binop ── *)
let str_binop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

(* ── cmpop ── *)
let str_cmpop = function
  | Eq  -> "==" | Neq -> "!=" | Lt -> "<"
  | Gt  -> ">"  | Leq -> "<=" | Geq -> ">="

(* ── expression ── *)
let rec print_expr = function
  | Num f   ->
      if Float.is_integer f then string_of_int (int_of_float f)
      else string_of_float f
  | Var s   -> s
  | Binop (op, l, r) ->
      "(" ^ print_expr l ^ " " ^ str_binop op ^ " " ^ print_expr r ^ ")"
  | DotQ dq -> print_dot_query dq

(* ── dot query ── *)
and print_dot_query = function
  | DotRange     (p, g)        -> "range."      ^ p ^ "(" ^ print_gopt g ^ ")"
  | DotMaxRange  (p, g)        -> "max_range."  ^ p ^ "(" ^ print_gopt g ^ ")"
  | DotMaxHeight (p, g)        -> "max_height." ^ p ^ "(" ^ print_gopt g ^ ")"
  | DotMaxRect   (p, g)        -> "max_rectangle." ^ p ^ "(" ^ print_gopt g ^ ")"
  | DotMinVel    (p, x, h, g)  ->
      "min_vel." ^ p ^ "(" ^ print_expr x ^ ", " ^ print_expr h
      ^ (match g with Some gv -> ", " ^ print_expr gv | None -> "") ^ ")"
  | DotCollide   (p1, p2, g)   ->
      "collide.(" ^ p1 ^ ", " ^ p2
      ^ (match g with Some gv -> ", " ^ print_expr gv | None -> "") ^ ")"
  | DotMinDist   (p1, p2, g)   ->
      "min_dist.(" ^ p1 ^ ", " ^ p2
      ^ (match g with Some gv -> ", " ^ print_expr gv | None -> "") ^ ")"

and print_gopt = function
  | Some g -> print_expr g
  | None   -> ""

(* ── condition ── *)
let rec print_cond = function
  | Cmp (op, l, r)  ->
      print_expr l ^ " " ^ str_cmpop op ^ " " ^ print_expr r
  | And (a, b)      -> "(" ^ print_cond a ^ " and " ^ print_cond b ^ ")"
  | Or  (a, b)      -> "(" ^ print_cond a ^ " or "  ^ print_cond b ^ ")"
  | Not c           -> "(not " ^ print_cond c ^ ")"
  | BoolDotQ dq     -> print_dot_query dq

(* ── simulate statement ── *)
let print_sim_stmt i s =
  match s with
  | SGravity e          -> ind i ^ "gravity      " ^ print_expr e
  | SPlot p             -> ind i ^ "plot         " ^ p
  | SRange p            -> ind i ^ "range        " ^ p
  | SMaxRange p         -> ind i ^ "max_range    " ^ p
  | SMaxHeight p        -> ind i ^ "max_height   " ^ p
  | SMaxRect p          -> ind i ^ "max_rectangle " ^ p
  | SMinVel (p, x, h)   ->
      ind i ^ "min_vel      " ^ p ^ " tower ("
      ^ print_expr x ^ ", " ^ print_expr h ^ ")"
  | SCollide (p1, p2)   -> ind i ^ "collide      " ^ p1 ^ " " ^ p2
  | SCollisionVel (p1,p2) -> ind i ^ "collision_vel " ^ p1 ^ " " ^ p2
  | SMinDist (p1, p2)   -> ind i ^ "min_dist     " ^ p1 ^ " " ^ p2
  | SBounce (p, n, r)   ->
      ind i ^ "bounce       " ^ p
      ^ " times " ^ print_expr n
      ^ " restitution " ^ print_expr r
  | SCheck c            -> ind i ^ "check        " ^ print_cond c

(* ── top-level statement ── *)
let rec print_stmt i s =
  match s with
  | Projectile { name; angle; speed; launch_from } ->
      let lf = match launch_from with
        | None -> ""
        | Some (x, y, t) ->
            "\n" ^ ind (i+1) ^ "launch_from ("
            ^ print_expr x ^ ", " ^ print_expr y ^ ", " ^ print_expr t ^ ")"
      in
      ind i ^ "projectile " ^ name ^ " {\n"
      ^ ind (i+1) ^ "angle " ^ print_expr angle ^ "\n"
      ^ ind (i+1) ^ "speed " ^ print_expr speed
      ^ lf ^ "\n"
      ^ ind i ^ "}"

  | Simulate ss ->
      ind i ^ "simulate {\n"
      ^ String.concat "\n" (List.map (print_sim_stmt (i+1)) ss) ^ "\n"
      ^ ind i ^ "}"

  | Fork (name, branches) ->
      ind i ^ "fork " ^ name ^ " {\n"
      ^ String.concat "\n" (List.map (print_branch (i+1)) branches) ^ "\n"
      ^ ind i ^ "}"

  | Game { planet; level; lives } ->
      ind i ^ "game {\n"
      ^ ind (i+1) ^ "planet      " ^ planet ^ "\n"
      ^ ind (i+1) ^ "level       " ^ print_expr level ^ "\n"
      ^ ind (i+1) ^ "lives       " ^ print_expr lives ^ "\n"
      ^ ind i ^ "}"

  | Let (name, e) -> ind i ^ "let " ^ name ^ " = " ^ print_expr e
  | Set (name, e) -> ind i ^ "set " ^ name ^ " = " ^ print_expr e

  | For (var, a, b, s, body) ->
      ind i ^ "for " ^ var
      ^ " from " ^ print_expr a
      ^ " to "   ^ print_expr b
      ^ " step "  ^ print_expr s ^ " {\n"
      ^ print_stmts (i+1) body ^ "\n"
      ^ ind i ^ "}"

  | Repeat (n, body) ->
      ind i ^ "repeat " ^ print_expr n ^ " {\n"
      ^ print_stmts (i+1) body ^ "\n"
      ^ ind i ^ "}"

  | While (c, body) ->
      ind i ^ "while " ^ print_cond c ^ " {\n"
      ^ print_stmts (i+1) body ^ "\n"
      ^ ind i ^ "}"

  | IfElse (c, tbody, fbody_opt) ->
      ind i ^ "if " ^ print_cond c ^ " {\n"
      ^ print_stmts (i+1) tbody ^ "\n"
      ^ ind i ^ "}"
      ^ (match fbody_opt with
         | None    -> ""
         | Some fb -> " else {\n" ^ print_stmts (i+1) fb ^ "\n" ^ ind i ^ "}")

and print_branch i br =
  ind i ^ "branch \"" ^ br.label ^ "\" {\n"
  ^ ind (i+1) ^ "gravity " ^ print_expr br.br_gravity ^ "\n"
  ^ (match br.br_bounce with
     | None -> ""
     | Some (n, r) ->
         ind (i+1) ^ "bounce times " ^ print_expr n
         ^ " restitution " ^ print_expr r ^ "\n")
  ^ ind i ^ "}"

and print_stmts i stmts =
  String.concat "\n" (List.map (print_stmt i) stmts)

(* ── entry point ── *)
let print_program prog = print_stmts 0 prog