(* ── ProjX v3 Physics with Air Resistance ── *)

let pi = 4.0 *. atan 1.0

let deg_to_rad deg = deg *. pi /. 180.0

let default_air_density = 1.225

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Default drag parameters
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let default_mass = 0.5
let default_drag_coeff = 0.47
let default_cross_section = 0.002

let get_drag_params proj =
  let mass = match proj.Env.mass with
    | Some m -> m
    | None -> default_mass
  in
  let drag_coeff = match proj.Env.drag_coeff with
    | Some d -> d
    | None -> default_drag_coeff
  in
  let cross_section = match proj.Env.cross_section with
    | Some c -> c
    | None -> default_cross_section
  in
  (mass, drag_coeff, cross_section)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Air resistance parameters
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type drag_params = {
  enabled       : bool;
  air_density   : float;
  wind_x        : float;
  wind_y        : float;
}

let no_drag = {
  enabled = false;
  air_density = default_air_density;
  wind_x = 0.0;
  wind_y = 0.0;
}

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Basic projectile components
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let vx angle speed =
  speed *. cos (deg_to_rad angle)

let vy angle speed =
  speed *. sin (deg_to_rad angle)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Drag force calculation
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let drag_force vx vy air_density drag_coeff cross_section =
  let v_mag = sqrt (vx *. vx +. vy *. vy) in
  if v_mag = 0.0 then (0.0, 0.0)
  else
    let f_mag = 0.5 *. air_density *. v_mag *. v_mag *. drag_coeff *. cross_section in
    let fx = -. f_mag *. (vx /. v_mag) in
    let fy = -. f_mag *. (vy /. v_mag) in
    (fx, fy)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Numerical integration (RK4)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type state = {
  x  : float;
  y  : float;
  vx : float;
  vy : float;
}

let derivatives state gravity mass drag_params drag_coeff cross_section =
  let rel_vx = state.vx -. drag_params.wind_x in
  let rel_vy = state.vy -. drag_params.wind_y in
  
  if drag_params.enabled then
    let (fx, fy) = drag_force rel_vx rel_vy 
                     drag_params.air_density drag_coeff cross_section in
    let ax = fx /. mass in
    let ay = -. gravity +. (fy /. mass) in
    { x = state.vx; y = state.vy; vx = ax; vy = ay }
  else
    { x = state.vx; y = state.vy; vx = 0.0; vy = -. gravity }

let rk4_step state dt gravity mass drag_params drag_coeff cross_section =
  let deriv = derivatives state gravity mass drag_params drag_coeff cross_section in
  
  let k1 = {
    x  = deriv.x *. dt;
    y  = deriv.y *. dt;
    vx = deriv.vx *. dt;
    vy = deriv.vy *. dt;
  } in
  
  let state2 = {
    x  = state.x  +. k1.x  /. 2.0;
    y  = state.y  +. k1.y  /. 2.0;
    vx = state.vx +. k1.vx /. 2.0;
    vy = state.vy +. k1.vy /. 2.0;
  } in
  let deriv2 = derivatives state2 gravity mass drag_params drag_coeff cross_section in
  let k2 = {
    x  = deriv2.x  *. dt;
    y  = deriv2.y  *. dt;
    vx = deriv2.vx *. dt;
    vy = deriv2.vy *. dt;
  } in
  
  let state3 = {
    x  = state.x  +. k2.x  /. 2.0;
    y  = state.y  +. k2.y  /. 2.0;
    vx = state.vx +. k2.vx /. 2.0;
    vy = state.vy +. k2.vy /. 2.0;
  } in
  let deriv3 = derivatives state3 gravity mass drag_params drag_coeff cross_section in
  let k3 = {
    x  = deriv3.x  *. dt;
    y  = deriv3.y  *. dt;
    vx = deriv3.vx *. dt;
    vy = deriv3.vy *. dt;
  } in
  
  let state4 = {
    x  = state.x  +. k3.x;
    y  = state.y  +. k3.y;
    vx = state.vx +. k3.vx;
    vy = state.vy +. k3.vy;
  } in
  let deriv4 = derivatives state4 gravity mass drag_params drag_coeff cross_section in
  let k4 = {
    x  = deriv4.x  *. dt;
    y  = deriv4.y  *. dt;
    vx = deriv4.vx *. dt;
    vy = deriv4.vy *. dt;
  } in
  
  {
    x  = state.x  +. (k1.x  +. 2.0 *. k2.x  +. 2.0 *. k3.x  +. k4.x)  /. 6.0;
    y  = state.y  +. (k1.y  +. 2.0 *. k2.y  +. 2.0 *. k3.y  +. k4.y)  /. 6.0;
    vx = state.vx +. (k1.vx +. 2.0 *. k2.vx +. 2.0 *. k3.vx +. k4.vx) /. 6.0;
    vy = state.vy +. (k1.vy +. 2.0 *. k2.vy +. 2.0 *. k3.vy +. k4.vy) /. 6.0;
  }

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate trajectory
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let simulate_trajectory 
    ?(max_time=100.0) 
    ?(dt=0.01)
    ?(drag_params=no_drag)
    ~angle ~speed ~x0 ~y0 ~gravity 
    ~mass ~drag_coeff ~cross_section () =
  
  let initial_state = {
    x  = x0;
    y  = y0;
    vx = vx angle speed;
    vy = vy angle speed;
  } in
  
  let rec simulate t state acc =
    if t > max_time || state.y < 0.0 then
      List.rev ((t, state.x, state.y, state.vx, state.vy) :: acc)
    else
      let new_state = rk4_step state dt gravity mass drag_params drag_coeff cross_section in
      simulate (t +. dt) new_state ((t, state.x, state.y, state.vx, state.vy) :: acc)
  in
  
  simulate 0.0 initial_state []

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Core queries
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let range angle speed gravity =
  let v2     = speed *. speed in
  let sin2th = sin (deg_to_rad (2.0 *. angle)) in
  v2 *. sin2th /. gravity

let range_with_drag angle speed gravity x0 y0 drag_params mass drag_coeff cross_section =
  let trajectory = simulate_trajectory 
    ~drag_params ~angle ~speed ~x0 ~y0 ~gravity ~mass ~drag_coeff ~cross_section () in
  let rec find_landing = function
    | [] -> x0
    | (_, x, y, _, _) :: rest ->
        if y <= 0.0 then x
        else find_landing rest
  in
  find_landing trajectory

let time_of_flight angle speed gravity =
  2.0 *. vy angle speed /. gravity

let max_height angle speed gravity =
  let v = vy angle speed in
  v *. v /. (2.0 *. gravity)

let max_height_with_drag angle speed gravity y0 drag_params mass drag_coeff cross_section =
  let trajectory = simulate_trajectory 
    ~drag_params ~angle ~speed ~x0:0.0 ~y0 ~gravity ~mass ~drag_coeff ~cross_section () in
  let rec find_max acc = function
    | [] -> acc
    | (_, _, y, _, _) :: rest ->
        find_max (Float.max acc y) rest
  in
  find_max y0 trajectory

let x_at angle speed t x0 =
  x0 +. vx angle speed *. t

let y_at angle speed gravity t y0 =
  y0 +. vy angle speed *. t -. 0.5 *. gravity *. t *. t

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Max range
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let max_range speed gravity =
  speed *. speed /. gravity

let max_range_angle = 45.0

let max_range_with_drag speed gravity x0 y0 drag_params mass drag_coeff cross_section =
  let best_range = ref 0.0 in
  let best_angle = ref 45.0 in
  for ang = 1 to 89 do
    let angle = float_of_int ang in
    let r = range_with_drag angle speed gravity x0 y0 drag_params mass drag_coeff cross_section in
    if r > !best_range then begin
      best_range := r;
      best_angle := angle
    end
  done;
  (!best_range, !best_angle)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Max rectangle
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let max_rectangle angle speed gravity x0 y0 =
  let r = range angle speed gravity in
  let tof = time_of_flight angle speed gravity in
  let steps = 1000 in
  let dt = tof /. float_of_int steps in
  let best = ref 0.0 in
  let best_x1 = ref x0 in
  let best_x2 = ref (x0 +. r) in
  let best_h  = ref 0.0 in
  for i = 0 to steps - 1 do
    let t1 = float_of_int i *. dt in
    let t2 = tof -. t1 in
    if t2 > t1 then begin
      let x1 = x_at angle speed t1 x0 in
      let x2 = x_at angle speed t2 x0 in
      let h  = Float.min
                 (y_at angle speed gravity t1 y0)
                 (y_at angle speed gravity t2 y0) in
      let w  = x2 -. x1 in
      let area = w *. h in
      if area > !best then begin
        best    := area;
        best_x1 := x1;
        best_x2 := x2;
        best_h  := h
      end
    end
  done;
  (!best, !best_x1, !best_x2, !best_h)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Min velocity
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let min_vel angle gravity tx th x0 y0 =
  let cos_a = cos (deg_to_rad angle) in
  let sin_a = sin (deg_to_rad angle) in
  let tx_rel = tx -. x0 in
  let tan_a  = sin_a /. cos_a in
  let denom  = tx_rel *. tan_a -. (th -. y0) in
  if denom <= 0.0 then infinity
  else
    sqrt (gravity *. tx_rel *. tx_rel
          /. (2.0 *. cos_a *. cos_a *. denom))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Collision detection
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let collide
    ~angle1 ~speed1 ~x01 ~y01
    ~angle2 ~speed2 ~x02 ~y02
    ~gravity =
  let tof1 = time_of_flight angle1 speed1 gravity in
  let tof2 = time_of_flight angle2 speed2 gravity in
  let tof  = Float.min tof1 tof2 in
  let steps = 2000 in
  let dt = tof /. float_of_int steps in
  let threshold = 1.0 in
  let result = ref (false, 0.0, 0.0, 0.0) in
  let found  = ref false in
  let i = ref 0 in
  while !i < steps && not !found do
    let t  = float_of_int !i *. dt in
    let x1 = x_at angle1 speed1 t x01 in
    let y1 = y_at angle1 speed1 gravity t y01 in
    let x2 = x_at angle2 speed2 t x02 in
    let y2 = y_at angle2 speed2 gravity t y02 in
    let dx = x1 -. x2 in
    let dy = y1 -. y2 in
    let dist = sqrt (dx *. dx +. dy *. dy) in
    if dist < threshold then begin
      result := (true, t, (x1 +. x2) /. 2.0, (y1 +. y2) /. 2.0);
      found  := true
    end;
    incr i
  done;
  !result

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Min distance
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let min_dist
    ~angle1 ~speed1 ~x01 ~y01
    ~angle2 ~speed2 ~x02 ~y02
    ~gravity =
  let tof1 = time_of_flight angle1 speed1 gravity in
  let tof2 = time_of_flight angle2 speed2 gravity in
  let tof  = Float.min tof1 tof2 in
  let steps = 2000 in
  let dt    = tof /. float_of_int steps in
  let best  = ref infinity in
  for i = 0 to steps do
    let t  = float_of_int i *. dt in
    let x1 = x_at angle1 speed1 t x01 in
    let y1 = y_at angle1 speed1 gravity t y01 in
    let x2 = x_at angle2 speed2 t x02 in
    let y2 = y_at angle2 speed2 gravity t y02 in
    let dx = x1 -. x2 in
    let dy = y1 -. y2 in
    let d  = sqrt (dx *. dx +. dy *. dy) in
    if d < !best then best := d
  done;
  !best

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Collision velocities
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let collision_vel
    ~angle1 ~speed1
    ~angle2 ~speed2
    ~gravity t =
  let vx1 = vx angle1 speed1 in
  let vy1 = vy angle1 speed1 -. gravity *. t in
  let vx2 = vx angle2 speed2 in
  let vy2 = vy angle2 speed2 -. gravity *. t in
  ((vx1, vy1), (vx2, vy2))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Bounce arcs
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let bounce_arcs angle speed gravity restitution times x0 y0 =
  let rec loop n ang spd x y acc =
    if n = 0 then List.rev acc
    else
      let r   = range ang spd gravity in
      let new_x = x +. r in
      let new_spd = spd *. restitution in
      let new_ang = ang in
      loop (n - 1) new_ang new_spd new_x y
        ((x, y, ang, spd) :: acc)
  in
  loop times angle speed x0 y0 []

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Arc sample points
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let arc_points angle speed gravity x0 y0 steps =
  let tof = time_of_flight angle speed gravity in
  let dt  = tof /. float_of_int steps in
  List.init (steps + 1) (fun i ->
    let t = float_of_int i *. dt in
    (x_at angle speed t x0,
     y_at angle speed gravity t y0)
  )

let arc_points_with_drag angle speed gravity x0 y0 steps drag_params mass drag_coeff cross_section =
  let trajectory = simulate_trajectory 
    ~drag_params ~angle ~speed ~x0 ~y0 ~gravity ~mass ~drag_coeff ~cross_section () in
  let total = List.length trajectory in
  let skip = max 1 (total / steps) in
  let rec sample i = function
    | [] -> []
    | (_, x, y, _, _) :: rest ->
        if i mod skip = 0 then (x, y) :: sample (i + 1) rest
        else sample (i + 1) rest
  in
  sample 0 trajectory

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Planet gravity
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let planet_gravity = function
  | "earth"   -> 9.8
  | "moon"    -> 1.62
  | "mars"    -> 3.72
  | "jupiter" -> 24.8
  | "sun"     -> 274.0
  | p         -> failwith (Printf.sprintf "Unknown planet: %s" p)