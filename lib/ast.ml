(* ── ProjX v4 AST — 3D upgrade ── *)

type binop = Add | Sub | Mul | Div
and cmpop = Eq | Neq | Lt | Gt | Leq | Geq

and expr =
  | Num of float
  | Var of string
  | Binop of binop * expr * expr
  | DotQ of dot_query

and dot_query =
  | DotRange of string * expr option
  | DotMaxRange of string * expr option
  | DotMaxHeight of string * expr option
  | DotMaxRect of string * expr option
  | DotMinVel of string * expr * expr * expr option
  | DotCollide of string * string * expr option
  | DotMinDist of string * string * expr option

and cond =
  | Cmp of cmpop * expr * expr
  | And of cond * cond
  | Or of cond * cond
  | Not of cond
  | BoolDotQ of dot_query

(* ── simulate statements ── *)
type sim_stmt =
  | SGravity of expr
  | SAirResistance of bool
  | SAirDensity of expr
  | SWindX of expr
  | SWindY of expr
  | SWindZ of expr (* NEW: wind along Z axis *)
  | SFor of string * expr * expr * expr * sim_stmt list
  | SRepeat of expr * sim_stmt list
  | SWhile of cond * sim_stmt list
  | SPlot of string
  | SRange of string
  | SMaxRange of string
  | SMaxHeight of string
  | SMaxRect of string
  | SMinVel of string * expr * expr
  | SCollide of string * string
  | SCollisionVel of string * string
  | SMinDist of string * string
  | SBounce of string * expr * expr
  | SCheck of cond
  | SProjectile of {
      name : string;
      angle : expr; (* elevation angle, degrees *)
      azimuth : expr option; (* NEW: horizontal aim angle, degrees. Default 0 *)
      speed : expr;
      launch_from : (expr * expr * expr * expr) option;
          (* x, y, z, t — z optional defaults 0 *)
      mass : expr option;
      drag_coeff : expr option;
      cross_section : expr option;
    }

(* ── fork branch ── *)
type branch = { label : string; br_stmts : sim_stmt list }

(* ── top-level statements ── *)
type stmt =
  | Projectile of {
      name : string;
      angle : expr; (* elevation angle, degrees *)
      azimuth : expr option; (* NEW: horizontal aim angle, degrees. Default 0 *)
      speed : expr;
      launch_from : (expr * expr * expr * expr) option;
          (* x, y, z, t — z optional defaults 0 *)
      mass : expr option;
      drag_coeff : expr option;
      cross_section : expr option;
    }
  | Simulate of sim_stmt list
  | Fork of string * branch list
  | Game of { planet : string; level : expr; lives : expr }
  | Let of string * expr
  | Set of string * expr
  | For of string * expr * expr * expr * stmt list
  | Repeat of expr * stmt list
  | While of cond * stmt list
  | IfElse of cond * stmt list * stmt list option

(* ── whole program ── *)
type program = stmt list
