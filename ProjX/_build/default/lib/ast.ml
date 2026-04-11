(* ── ProjX v3 AST with Air Resistance ── *)

(* ── all mutually recursive types in one block ── *)
type binop = Add | Sub | Mul | Div

and cmpop = Eq | Neq | Lt | Gt | Leq | Geq

and expr =
  | Num    of float
  | Var    of string
  | Binop  of binop * expr * expr
  | DotQ   of dot_query

and dot_query =
  | DotRange        of string * expr option
  | DotMaxRange     of string * expr option
  | DotMaxHeight    of string * expr option
  | DotMaxRect      of string * expr option
  | DotMinVel       of string * expr * expr * expr option
  | DotCollide      of string * string * expr option
  | DotMinDist      of string * string * expr option

and cond =
  | Cmp      of cmpop * expr * expr
  | And      of cond * cond
  | Or       of cond * cond
  | Not      of cond
  | BoolDotQ of dot_query

(* ── simulate statements ── *)
type sim_stmt =
  | SGravity       of expr
  | SAirResistance of bool
  | SAirDensity    of expr
  | SWindX         of expr
  | SWindY         of expr
  | SPlot          of string
  | SRange         of string
  | SMaxRange      of string
  | SMaxHeight     of string
  | SMaxRect       of string
  | SMinVel        of string * expr * expr
  | SCollide       of string * string
  | SCollisionVel  of string * string
  | SMinDist       of string * string
  | SBounce        of string * expr * expr
  | SCheck         of cond

(* ── fork branch ── *)
type branch = {
  label       : string;
  br_gravity  : expr;
  br_bounce   : (expr * expr) option;
}

(* ── top-level statements ── *)
type stmt =
  | Projectile of {
      name           : string;
      angle          : expr;
      speed          : expr;
      launch_from    : (expr * expr * expr) option;
      mass           : expr option;
      drag_coeff     : expr option;
      cross_section  : expr option;
    }
  | Simulate   of sim_stmt list
  | Fork       of string * branch list
  | Game       of {
      planet : string;
      level  : expr;
      lives  : expr;
    }
  | Let        of string * expr
  | Set        of string * expr
  | For        of string * expr * expr * expr * stmt list
  | Repeat     of expr * stmt list
  | While      of cond * stmt list
  | IfElse     of cond * stmt list * stmt list option

(* ── whole program ── *)
type program = stmt list