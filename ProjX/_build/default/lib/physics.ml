(* ── ProjX v3 Physics ── *)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Constants
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let pi = 4.0 *. atan 1.0

let deg_to_rad deg = deg *. pi /. 180.0

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Basic projectile components
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* horizontal velocity component *)
let vx angle speed =
  speed *. cos (deg_to_rad angle)

(* vertical velocity component *)
let vy angle speed =
  speed *. sin (deg_to_rad angle)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Core queries
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* horizontal range: R = v² sin(2θ) / g *)
let range angle speed gravity =
  let v2     = speed *. speed in
  let sin2th = sin (deg_to_rad (2.0 *. angle)) in
  v2 *. sin2th /. gravity

(* time of flight: T = 2 vy / g *)
let time_of_flight angle speed gravity =
  2.0 *. vy angle speed /. gravity

(* max height: H = vy² / (2g) *)
let max_height angle speed gravity =
  let v = vy angle speed in
  v *. v /. (2.0 *. gravity)

(* position at time t *)
let x_at angle speed t x0 =
  x0 +. vx angle speed *. t

let y_at angle speed gravity t y0 =
  y0 +. vy angle speed *. t -. 0.5 *. gravity *. t *. t

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Max range (optimal angle sweep)
   sweeps 1°–89° and returns best
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let max_range speed gravity =
  (* optimal angle is always 45° → v²/g *)
  speed *. speed /. gravity

let max_range_angle = 45.0

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Max rectangle inscribed under arc
   area = (1/2) * R * H_peak * (some factor)
   solved numerically by sampling
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
   Min velocity to clear tower
   tower at x=tx with height th
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let min_vel angle gravity tx th x0 y0 =
  (* time to reach tower x: t = (tx - x0) / vx *)
  let cos_a = cos (deg_to_rad angle) in
  let sin_a = sin (deg_to_rad angle) in
  (* need: y(t) >= th
     v*sin(a)*t - 0.5*g*t² >= th - y0
     t = tx / (v*cos(a))
     substituting and solving for v:
     v >= sqrt( g * tx² / (2 * cos²(a) * (tx*tan(a) - th + y0)) ) *)
  let tx_rel = tx -. x0 in
  let tan_a  = sin_a /. cos_a in
  let denom  = tx_rel *. tan_a -. (th -. y0) in
  if denom <= 0.0 then infinity  (* impossible to clear *)
  else
    sqrt (gravity *. tx_rel *. tx_rel
          /. (2.0 *. cos_a *. cos_a *. denom))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Collision detection (two projectiles)
   returns (collides, time, x, y)
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
  let threshold = 1.0 in  (* metres — close enough = collision *)
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
   Minimum distance between two projectiles
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
   returns (vx1,vy1) and (vx2,vy2) at collision time
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
   returns list of (x0, y0, angle, speed) per hop
   energy reduces by restitution each bounce
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let bounce_arcs angle speed gravity restitution times x0 y0 =
  let rec loop n ang spd x y acc =
    if n = 0 then List.rev acc
    else
      let r   = range ang spd gravity in
      let new_x = x +. r in
      (* after bounce: vy flips and scales by restitution, vx unchanged *)
      let new_spd = spd *. restitution in
      let new_ang = ang in  (* symmetric bounce off flat ground *)
      loop (n - 1) new_ang new_spd new_x y
        ((x, y, ang, spd) :: acc)
  in
  loop times angle speed x0 y0 []

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Arc sample points (for canvas drawing)
   returns list of (x, y) points
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let arc_points angle speed gravity x0 y0 steps =
  let tof = time_of_flight angle speed gravity in
  let dt  = tof /. float_of_int steps in
  List.init (steps + 1) (fun i ->
    let t = float_of_int i *. dt in
    (x_at angle speed t x0,
     y_at angle speed gravity t y0)
  )

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Planet gravity lookup
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let planet_gravity = function
  | "earth"   -> 9.8
  | "moon"    -> 1.62
  | "mars"    -> 3.72
  | "jupiter" -> 24.8
  | "sun"     -> 274.0
  | p         -> failwith (Printf.sprintf "Unknown planet: %s" p)
