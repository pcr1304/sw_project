(* ── ProjX v3 Environment ── *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Projectile value after evaluation
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type projectile_val = {
  angle       : float;
  speed       : float;
  launch_from : (float * float * float);  (* x, y, t *)
}

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Runtime environment
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type env = {
  vars        : (string * float) list;
  projectiles : (string * projectile_val) list;
}

let empty_env = {
  vars        = [];
  projectiles = [];
}

type bounce_val = {
  times : int;
  restitution : float;
}

type scenario =
  | SimScenario of string * float * (string * projectile_val) list * (string * float) list * (string * float) list
    (* label, gravity, projectiles, range_annotations, max_height_annotations *)
  | GameScenario of string * string * float * float * float
    (* label, planet, gravity, level, lives *)

let emitted_scenarios : scenario list ref = ref []


(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Variable functions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* remove a variable by name *)
let rec remove_var name = function
  | []              -> []
  | (n, v) :: rest  ->
      if n = name then rest
      else (n, v) :: remove_var name rest

(* add or replace a variable — used for let and for-loop variables *)
let add_var name value env =
  { env with vars = (name, value) :: remove_var name env.vars }

(* get a variable value — fails if not declared *)
let rec get_var name = function
  | []             -> failwith (Printf.sprintf "Variable not found: '%s'" name)
  | (n, v) :: rest ->
      if n = name then v
      else get_var name rest

(* update an existing variable — fails if not declared (used for set) *)
let update_var name value env =
  let rec update = function
    | []             -> failwith (Printf.sprintf "Variable '%s' not declared — use let first" name)
    | (n, v) :: rest ->
        if n = name then (name, value) :: rest
        else (n, v) :: update rest
  in
  { env with vars = update env.vars }

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Projectile functions
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* remove a projectile by name *)
let rec remove_proj name = function
  | []              -> []
  | (n, p) :: rest  ->
      if n = name then rest
      else (n, p) :: remove_proj name rest

(* add or replace a projectile *)
let add_projectile name proj env =
  { env with projectiles = (name, proj) :: remove_proj name env.projectiles }

(* get a projectile — fails if not declared *)
let rec get_projectile name = function
  | []             -> failwith (Printf.sprintf "Projectile not found: '%s'" name)
  | (n, p) :: rest ->
      if n = name then p
      else get_projectile name rest
