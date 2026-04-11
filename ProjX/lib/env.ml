(* ── ProjX Environment with Air Resistance ── *)

type projectile_val = {
  angle          : float;
  speed          : float;
  launch_from    : (float * float * float);
  mass           : float option;
  drag_coeff     : float option;
  cross_section  : float option;
}

type env = {
  vars : (string * float) list;
  projectiles : (string * projectile_val) list;
}

let empty_env = {
  vars = [];
  projectiles = [];
}

(* ── VARIABLE FUNCTIONS ── *)

let rec remove_var name vars =
  match vars with
  | [] -> []
  | (n, v) :: rest ->
      if n = name then rest
      else (n, v) :: remove_var name rest

let add_var name value env =
  let new_vars = remove_var name env.vars in
  { env with vars = (name, value) :: new_vars }

let rec get_var name vars =
  match vars with
  | [] -> failwith ("Variable not found: " ^ name)
  | (n, v) :: rest ->
      if n = name then v
      else get_var name rest

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

let rec remove_proj name projs =
  match projs with
  | [] -> []
  | (n, p) :: rest ->
      if n = name then rest
      else (n, p) :: remove_proj name rest

let add_projectile name proj env =
  let new_projs = remove_proj name env.projectiles in
  { env with projectiles = (name, proj) :: new_projs }

let rec get_projectile name projs =
  match projs with
  | [] -> failwith ("Projectile not found: " ^ name)
  | (n, p) :: rest ->
      if n = name then p
      else get_projectile name rest