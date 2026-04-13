(* ── ProjX v4 Physics — 3D upgrade ── *)

let pi = 4.0 *. atan 1.0
let deg_to_rad deg = deg *. pi /. 180.0
let default_air_density = 1.225
let default_mass = 0.5
let default_drag_coeff = 0.47
let default_cross_section = 0.002

let get_drag_params proj =
  let mass = match proj.Env.mass with Some m -> m | None -> default_mass in
  let drag_coeff =
    match proj.Env.drag_coeff with Some d -> d | None -> default_drag_coeff
  in
  let cross_section =
    match proj.Env.cross_section with
    | Some c -> c
    | None -> default_cross_section
  in
  (mass, drag_coeff, cross_section)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Air resistance parameters
   drag_params now carries wind_z for 3D
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type drag_params = {
  enabled : bool;
  air_density : float;
  wind_x : float;
  wind_y : float;
  wind_z : float; (* NEW: wind component along Z axis *)
}

let no_drag =
  {
    enabled = false;
    air_density = default_air_density;
    wind_x = 0.0;
    wind_y = 0.0;
    wind_z = 0.0;
  }

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Basic projectile velocity components (2D, still used for 2D queries)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let vx_2d angle speed = speed *. cos (deg_to_rad angle)
let vy_2d angle speed = speed *. sin (deg_to_rad angle)

(* Backwards-compat aliases used in the rest of the 2D code *)
let vx = vx_2d
let vy = vy_2d

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   2D drag force (kept for 2D simulation)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let drag_force vx vy air_density drag_coeff cross_section =
  let v_mag = sqrt ((vx *. vx) +. (vy *. vy)) in
  if v_mag = 0.0 then (0.0, 0.0)
  else
    let f_mag =
      0.5 *. air_density *. v_mag *. v_mag *. drag_coeff *. cross_section
    in
    (-.f_mag *. (vx /. v_mag), -.f_mag *. (vy /. v_mag))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   2D RK4 state & integrator (unchanged — used by all 2D queries)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type state = { x : float; y : float; vx : float; vy : float }

let derivatives state gravity mass drag_params drag_coeff cross_section =
  let rel_vx = state.vx -. drag_params.wind_x in
  let rel_vy = state.vy -. drag_params.wind_y in
  if drag_params.enabled then
    let fx, fy =
      drag_force rel_vx rel_vy drag_params.air_density drag_coeff cross_section
    in
    let ax = fx /. mass in
    let ay = -.gravity +. (fy /. mass) in
    { x = state.vx; y = state.vy; vx = ax; vy = ay }
  else { x = state.vx; y = state.vy; vx = 0.0; vy = -.gravity }

let rk4_step state dt gravity mass drag_params drag_coeff cross_section =
  let deriv =
    derivatives state gravity mass drag_params drag_coeff cross_section
  in
  let k1 =
    {
      x = deriv.x *. dt;
      y = deriv.y *. dt;
      vx = deriv.vx *. dt;
      vy = deriv.vy *. dt;
    }
  in
  let s2 =
    {
      x = state.x +. (k1.x /. 2.);
      y = state.y +. (k1.y /. 2.);
      vx = state.vx +. (k1.vx /. 2.);
      vy = state.vy +. (k1.vy /. 2.);
    }
  in
  let d2 = derivatives s2 gravity mass drag_params drag_coeff cross_section in
  let k2 =
    { x = d2.x *. dt; y = d2.y *. dt; vx = d2.vx *. dt; vy = d2.vy *. dt }
  in
  let s3 =
    {
      x = state.x +. (k2.x /. 2.);
      y = state.y +. (k2.y /. 2.);
      vx = state.vx +. (k2.vx /. 2.);
      vy = state.vy +. (k2.vy /. 2.);
    }
  in
  let d3 = derivatives s3 gravity mass drag_params drag_coeff cross_section in
  let k3 =
    { x = d3.x *. dt; y = d3.y *. dt; vx = d3.vx *. dt; vy = d3.vy *. dt }
  in
  let s4 =
    {
      x = state.x +. k3.x;
      y = state.y +. k3.y;
      vx = state.vx +. k3.vx;
      vy = state.vy +. k3.vy;
    }
  in
  let d4 = derivatives s4 gravity mass drag_params drag_coeff cross_section in
  let k4 =
    { x = d4.x *. dt; y = d4.y *. dt; vx = d4.vx *. dt; vy = d4.vy *. dt }
  in
  {
    x = state.x +. ((k1.x +. (2. *. k2.x) +. (2. *. k3.x) +. k4.x) /. 6.);
    y = state.y +. ((k1.y +. (2. *. k2.y) +. (2. *. k3.y) +. k4.y) /. 6.);
    vx = state.vx +. ((k1.vx +. (2. *. k2.vx) +. (2. *. k3.vx) +. k4.vx) /. 6.);
    vy = state.vy +. ((k1.vy +. (2. *. k2.vy) +. (2. *. k3.vy) +. k4.vy) /. 6.);
  }

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   NEW: 3D state type and integrator
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

type state3d = {
  x3 : float;
  y3 : float;
  z3 : float;
  vx3 : float;
  vy3 : float;
  vz3 : float;
}

(* Returns acceleration components for a 3D state.
   Gravity acts only on y (vertical). Drag uses 3D velocity magnitude. *)
let derivatives3d (s : state3d) gravity mass (dp : drag_params) drag_coeff
    cross_section =
  if dp.enabled then begin
    let rvx = s.vx3 -. dp.wind_x in
    let rvy = s.vy3 -. dp.wind_y in
    let rvz = s.vz3 -. dp.wind_z in
    let v_mag = sqrt ((rvx *. rvx) +. (rvy *. rvy) +. (rvz *. rvz)) in
    if v_mag = 0.0 then
      {
        x3 = s.vx3;
        y3 = s.vy3;
        z3 = s.vz3;
        vx3 = 0.;
        vy3 = -.gravity;
        vz3 = 0.;
      }
    else begin
      let f_mag =
        0.5 *. dp.air_density *. v_mag *. v_mag *. drag_coeff *. cross_section
      in
      let ax = -.f_mag *. rvx /. v_mag /. mass in
      let ay = -.gravity -. (f_mag *. rvy /. v_mag /. mass) in
      let az = -.f_mag *. rvz /. v_mag /. mass in
      { x3 = s.vx3; y3 = s.vy3; z3 = s.vz3; vx3 = ax; vy3 = ay; vz3 = az }
    end
  end
  else
    { x3 = s.vx3; y3 = s.vy3; z3 = s.vz3; vx3 = 0.; vy3 = -.gravity; vz3 = 0. }

let rk4_step3d (s : state3d) dt gravity mass dp drag_coeff cross_section =
  let drv d =
    {
      x3 = d.x3 *. dt;
      y3 = d.y3 *. dt;
      z3 = d.z3 *. dt;
      vx3 = d.vx3 *. dt;
      vy3 = d.vy3 *. dt;
      vz3 = d.vz3 *. dt;
    }
  in
  let add_k st k sc =
    {
      x3 = st.x3 +. (k.x3 *. sc);
      y3 = st.y3 +. (k.y3 *. sc);
      z3 = st.z3 +. (k.z3 *. sc);
      vx3 = st.vx3 +. (k.vx3 *. sc);
      vy3 = st.vy3 +. (k.vy3 *. sc);
      vz3 = st.vz3 +. (k.vz3 *. sc);
    }
  in
  let d1 = derivatives3d s gravity mass dp drag_coeff cross_section in
  let k1 = drv d1 in
  let d2 =
    derivatives3d (add_k s k1 0.5) gravity mass dp drag_coeff cross_section
  in
  let k2 = drv d2 in
  let d3 =
    derivatives3d (add_k s k2 0.5) gravity mass dp drag_coeff cross_section
  in
  let k3 = drv d3 in
  let d4 =
    derivatives3d (add_k s k3 1.0) gravity mass dp drag_coeff cross_section
  in
  let k4 = drv d4 in
  {
    x3 = s.x3 +. ((k1.x3 +. (2. *. k2.x3) +. (2. *. k3.x3) +. k4.x3) /. 6.);
    y3 = s.y3 +. ((k1.y3 +. (2. *. k2.y3) +. (2. *. k3.y3) +. k4.y3) /. 6.);
    z3 = s.z3 +. ((k1.z3 +. (2. *. k2.z3) +. (2. *. k3.z3) +. k4.z3) /. 6.);
    vx3 = s.vx3 +. ((k1.vx3 +. (2. *. k2.vx3) +. (2. *. k3.vx3) +. k4.vx3) /. 6.);
    vy3 = s.vy3 +. ((k1.vy3 +. (2. *. k2.vy3) +. (2. *. k3.vy3) +. k4.vy3) /. 6.);
    vz3 = s.vz3 +. ((k1.vz3 +. (2. *. k2.vz3) +. (2. *. k3.vz3) +. k4.vz3) /. 6.);
  }

(* ── simulate_trajectory_3d ─────────────────────────────────────────
   Returns a list of (t, x, y, z) tuples.
   azimuth = horizontal aim angle in degrees (0 = along X axis,
             90 = along Z axis, like a compass bearing but for X/Z).
   ─────────────────────────────────────────────────────────────────── *)
let simulate_trajectory_3d ?(max_time = 100.0) ?(dt = 0.016)
    ?(drag_params = no_drag) ~angle ~azimuth ~speed ~x0 ~y0 ~z0 ~gravity ~mass
    ~drag_coeff ~cross_section () =
  let elev_rad = deg_to_rad angle in
  let az_rad = deg_to_rad azimuth in
  let init =
    {
      x3 = x0;
      y3 = y0;
      z3 = z0;
      vx3 = speed *. cos elev_rad *. cos az_rad;
      vy3 = speed *. sin elev_rad;
      vz3 = speed *. cos elev_rad *. sin az_rad;
    }
  in
  let rec sim t st acc =
    if t > max_time || st.y3 < 0.0 then List.rev acc
    else
      let next =
        rk4_step3d st dt gravity mass drag_params drag_coeff cross_section
      in
      sim (t +. dt) next ((t, st.x3, st.y3, st.z3) :: acc)
  in
  sim 0.0 init []

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   2D simulate_trajectory (unchanged)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let simulate_trajectory ?(max_time = 100.0) ?(dt = 0.01)
    ?(drag_params = no_drag) ~angle ~speed ~x0 ~y0 ~gravity ~mass ~drag_coeff
    ~cross_section () =
  let initial_state =
    { x = x0; y = y0; vx = vx angle speed; vy = vy angle speed }
  in
  let rec simulate t state acc =
    if t > max_time || state.y < 0.0 then
      List.rev ((t, state.x, state.y, state.vx, state.vy) :: acc)
    else
      let new_state =
        rk4_step state dt gravity mass drag_params drag_coeff cross_section
      in
      simulate (t +. dt) new_state
        ((t, state.x, state.y, state.vx, state.vy) :: acc)
  in
  simulate 0.0 initial_state []

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   All 2D core queries (unchanged)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let range angle speed gravity =
  let v2 = speed *. speed in
  let sin2th = sin (deg_to_rad (2.0 *. angle)) in
  v2 *. sin2th /. gravity

let range_with_drag angle speed gravity x0 y0 drag_params mass drag_coeff
    cross_section =
  let traj =
    simulate_trajectory ~drag_params ~angle ~speed ~x0 ~y0 ~gravity ~mass
      ~drag_coeff ~cross_section ()
  in
  let rec find_landing = function
    | [] -> x0
    | (_, x, y, _, _) :: rest -> if y <= 0.0 then x else find_landing rest
  in
  find_landing traj

let time_of_flight angle speed gravity = 2.0 *. vy angle speed /. gravity

let max_height angle speed gravity =
  let v = vy angle speed in
  v *. v /. (2.0 *. gravity)

let max_height_with_drag angle speed gravity y0 drag_params mass drag_coeff
    cross_section =
  let traj =
    simulate_trajectory ~drag_params ~angle ~speed ~x0:0.0 ~y0 ~gravity ~mass
      ~drag_coeff ~cross_section ()
  in
  let rec find_max acc = function
    | [] -> acc
    | (_, _, y, _, _) :: rest -> find_max (Float.max acc y) rest
  in
  find_max y0 traj

let x_at angle speed t x0 = x0 +. (vx angle speed *. t)

let y_at angle speed gravity t y0 =
  y0 +. (vy angle speed *. t) -. (0.5 *. gravity *. t *. t)

let max_range speed gravity = speed *. speed /. gravity
let max_range_angle = 45.0

let max_range_with_drag speed gravity x0 y0 drag_params mass drag_coeff
    cross_section =
  let best_range = ref 0.0 in
  let best_angle = ref 45.0 in
  for ang = 1 to 89 do
    let angle = float_of_int ang in
    let r =
      range_with_drag angle speed gravity x0 y0 drag_params mass drag_coeff
        cross_section
    in
    if r > !best_range then begin
      best_range := r;
      best_angle := angle
    end
  done;
  (!best_range, !best_angle)

let max_rectangle angle speed gravity x0 y0 =
  let r = range angle speed gravity in
  let tof = time_of_flight angle speed gravity in
  let steps = 1000 in
  let dt = tof /. float_of_int steps in
  let best = ref 0.0 in
  let best_x1 = ref x0 in
  let best_x2 = ref (x0 +. r) in
  let best_h = ref 0.0 in
  for i = 0 to steps - 1 do
    let t1 = float_of_int i *. dt in
    let t2 = tof -. t1 in
    if t2 > t1 then begin
      let x1 = x_at angle speed t1 x0 in
      let x2 = x_at angle speed t2 x0 in
      let h =
        Float.min
          (y_at angle speed gravity t1 y0)
          (y_at angle speed gravity t2 y0)
      in
      let w = x2 -. x1 in
      let area = w *. h in
      if area > !best then begin
        best := area;
        best_x1 := x1;
        best_x2 := x2;
        best_h := h
      end
    end
  done;
  (!best, !best_x1, !best_x2, !best_h)

let min_vel angle gravity tx th x0 y0 =
  let cos_a = cos (deg_to_rad angle) in
  let sin_a = sin (deg_to_rad angle) in
  let tx_rel = tx -. x0 in
  let tan_a = sin_a /. cos_a in
  let denom = (tx_rel *. tan_a) -. (th -. y0) in
  if denom <= 0.0 then infinity
  else sqrt (gravity *. tx_rel *. tx_rel /. (2.0 *. cos_a *. cos_a *. denom))

let collide ~angle1 ~speed1 ~x01 ~y01 ~angle2 ~speed2 ~x02 ~y02 ~gravity =
  let tof1 = time_of_flight angle1 speed1 gravity in
  let tof2 = time_of_flight angle2 speed2 gravity in
  let tof = Float.min tof1 tof2 in
  let steps = 2000 in
  let dt = tof /. float_of_int steps in
  let threshold = 1.0 in
  let result = ref (false, 0.0, 0.0, 0.0) in
  let found = ref false in
  let i = ref 0 in
  while !i < steps && not !found do
    let t = float_of_int !i *. dt in
    let x1 = x_at angle1 speed1 t x01 in
    let y1 = y_at angle1 speed1 gravity t y01 in
    let x2 = x_at angle2 speed2 t x02 in
    let y2 = y_at angle2 speed2 gravity t y02 in
    let dx = x1 -. x2 in
    let dy = y1 -. y2 in
    if sqrt ((dx *. dx) +. (dy *. dy)) < threshold then begin
      result := (true, t, (x1 +. x2) /. 2., (y1 +. y2) /. 2.);
      found := true
    end;
    incr i
  done;
  !result

let min_dist ~angle1 ~speed1 ~x01 ~y01 ~angle2 ~speed2 ~x02 ~y02 ~gravity =
  let tof1 = time_of_flight angle1 speed1 gravity in
  let tof2 = time_of_flight angle2 speed2 gravity in
  let tof = Float.min tof1 tof2 in
  let steps = 2000 in
  let dt = tof /. float_of_int steps in
  let best = ref infinity in
  for i = 0 to steps do
    let t = float_of_int i *. dt in
    let x1 = x_at angle1 speed1 t x01 in
    let y1 = y_at angle1 speed1 gravity t y01 in
    let x2 = x_at angle2 speed2 t x02 in
    let y2 = y_at angle2 speed2 gravity t y02 in
    let dx = x1 -. x2 in
    let dy = y1 -. y2 in
    let d = sqrt ((dx *. dx) +. (dy *. dy)) in
    if d < !best then best := d
  done;
  !best

let collision_vel ~angle1 ~speed1 ~angle2 ~speed2 ~gravity t =
  let vx1 = vx angle1 speed1 in
  let vy1 = vy angle1 speed1 -. (gravity *. t) in
  let vx2 = vx angle2 speed2 in
  let vy2 = vy angle2 speed2 -. (gravity *. t) in
  ((vx1, vy1), (vx2, vy2))

let bounce_arcs angle speed gravity restitution times x0 y0 =
  let rec loop n ang spd x y acc =
    if n = 0 then List.rev acc
    else
      let r = range ang spd gravity in
      let new_x = x +. r in
      loop (n - 1) ang (spd *. restitution) new_x y ((x, y, ang, spd) :: acc)
  in
  loop times angle speed x0 y0 []

let arc_points angle speed gravity x0 y0 steps =
  let tof = time_of_flight angle speed gravity in
  let dt = tof /. float_of_int steps in
  List.init (steps + 1) (fun i ->
      let t = float_of_int i *. dt in
      (x_at angle speed t x0, y_at angle speed gravity t y0))

let arc_points_with_drag angle speed gravity x0 y0 steps drag_params mass
    drag_coeff cross_section =
  let traj =
    simulate_trajectory ~drag_params ~angle ~speed ~x0 ~y0 ~gravity ~mass
      ~drag_coeff ~cross_section ()
  in
  let total = List.length traj in
  let skip = max 1 (total / steps) in
  let rec sample i = function
    | [] -> []
    | (_, x, y, _, _) :: rest ->
        if i mod skip = 0 then (x, y) :: sample (i + 1) rest
        else sample (i + 1) rest
  in
  sample 0 traj

let planet_gravity = function
  | "earth" -> 9.8
  | "moon" -> 1.62
  | "mars" -> 3.72
  | "jupiter" -> 24.8
  | "sun" -> 274.0
  | p -> failwith (Printf.sprintf "Unknown planet: %s" p)
