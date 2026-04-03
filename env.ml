(* ── ProjX Environment ── *)

(* projectile value after evaluation *)
type projectile_val = {
  angle : float;
  speed : float;
  launch_from : (float * float * float);   (* x, y, t *)
}

(* environment *)
type env = {
  vars : (string * float) list;
  projectiles : (string * projectile_val) list;
}

(* empty environment *)
let empty_env = {
  vars = [];
  projectiles = [];
}

(* ── VARIABLE FUNCTIONS ── *)

(* add variable (replace if already exists) *)
let rec remove_var name vars =
  match vars with
  | [] -> []
  | (n, v) :: rest ->
      if n = name then rest
      else (n, v) :: remove_var name rest

let add_var name value env =
  let new_vars = remove_var name env.vars in
  { env with vars = (name, value) :: new_vars }

(* get variable value *)
let rec get_var name vars =
  match vars with
  | [] -> failwith ("Variable not found: " ^ name)
  | (n, v) :: rest ->
      if n = name then v
      else get_var name rest

(* update variable *)
let update_var name value env =
  let rec update vars =
    match vars with
    | [] -> failwith ("Variable not found: " ^ name)
    | (n, v) :: rest ->
        if n = name then (name, value) :: rest
        else (n, v) :: update rest
  in
  { env with vars = update env.vars }

(* ── PROJECTILE FUNCTIONS ── *)

(* remove projectile *)
let rec remove_proj name projs =
  match projs with
  | [] -> []
  | (n, p) :: rest ->
      if n = name then rest
      else (n, p) :: remove_proj name rest

(* add projectile *)
let add_projectile name proj env =
  let new_projs = remove_proj name env.projectiles in
  { env with projectiles = (name, proj) :: new_projs }

(* get projectile *)
let rec get_projectile name projs =
  match projs with
  | [] -> failwith ("Projectile not found: " ^ name)
  | (n, p) :: rest ->
      if n = name then p
      else get_projectile name rest