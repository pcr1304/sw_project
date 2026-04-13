(* ── ProjX v4 Parser — 3D upgrade ── *)

open Tokenizer
open Ast

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Token stream helpers
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let peek = function
  | tok :: _ -> tok
  | [] -> { kind = END; text = ""; lit_val = "null" }

let advance = function _ :: rest -> rest | [] -> []

let expect kind tokens =
  match tokens with
  | tok :: rest when tok.kind = kind -> rest
  | tok :: _ ->
      failwith
        (Printf.sprintf "Parse Error: expected %s but got '%s'" (str_tok kind)
           tok.text)
  | [] ->
      failwith
        (Printf.sprintf "Parse Error: expected %s but got end of input"
           (str_tok kind))

let expect_idf tokens =
  match tokens with
  | tok :: rest when tok.kind = IDF -> (tok.text, rest)
  | tok :: _ ->
      failwith
        (Printf.sprintf "Parse Error: expected identifier but got '%s'" tok.text)
  | [] -> failwith "Parse Error: expected identifier but got end of input"

let expect_str tokens =
  match tokens with
  | tok :: rest when tok.kind = STR -> (tok.lit_val, rest)
  | tok :: _ ->
      failwith
        (Printf.sprintf "Parse Error: expected string but got '%s'" tok.text)
  | [] -> failwith "Parse Error: expected string but got end of input"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Expression parser (+ - * /)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_expr tokens =
  let lhs, tokens = parse_term tokens in
  parse_expr_rest lhs tokens

and parse_expr_rest lhs tokens =
  match (peek tokens).kind with
  | PLUS ->
      let tokens = advance tokens in
      let rhs, tokens = parse_term tokens in
      parse_expr_rest (Binop (Add, lhs, rhs)) tokens
  | MINUS ->
      let tokens = advance tokens in
      let rhs, tokens = parse_term tokens in
      parse_expr_rest (Binop (Sub, lhs, rhs)) tokens
  | _ -> (lhs, tokens)

and parse_term tokens =
  let lhs, tokens = parse_factor tokens in
  parse_term_rest lhs tokens

and parse_term_rest lhs tokens =
  match (peek tokens).kind with
  | STAR ->
      let tokens = advance tokens in
      let rhs, tokens = parse_factor tokens in
      parse_term_rest (Binop (Mul, lhs, rhs)) tokens
  | SLASH ->
      let tokens = advance tokens in
      let rhs, tokens = parse_factor tokens in
      parse_term_rest (Binop (Div, lhs, rhs)) tokens
  | _ -> (lhs, tokens)

and parse_factor tokens =
  match (peek tokens).kind with
  | INT | FLOAT ->
      let tok = peek tokens in
      (Num (float_of_string tok.lit_val), advance tokens)
  | MINUS -> (
      let tokens = advance tokens in
      match (peek tokens).kind with
      | INT | FLOAT ->
          let tok = peek tokens in
          let value = -.float_of_string tok.lit_val in
          (Num value, advance tokens)
      | LEFT_PAR ->
          let tokens = advance tokens in
          let e, tokens = parse_expr tokens in
          let tokens = expect RIGHT_PAR tokens in
          (Binop (Sub, Num 0.0, e), tokens)
      | _ ->
          let e, tokens = parse_factor tokens in
          (Binop (Sub, Num 0.0, e), tokens))
  | PLUS ->
      let tokens = advance tokens in
      parse_factor tokens
  | IDF | RANGE | MAX_RANGE | MAX_HEIGHT | MAX_RECTANGLE | MIN_VEL | COLLIDE
  | MIN_DIST ->
      let tok = peek tokens in
      let rest1 = advance tokens in
      if (peek rest1).kind = DOT then parse_dot_query_as_expr tokens
      else (Var tok.text, rest1)
  | LEFT_PAR ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      let tokens = expect RIGHT_PAR tokens in
      (e, tokens)
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf "Parse Error: unexpected token '%s' in expression"
           tok.text)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Dot-query parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

and parse_dot_query_as_expr tokens =
  let dq, tokens = parse_dot_query tokens in
  (DotQ dq, tokens)

and parse_dot_query tokens =
  let tok = peek tokens in
  let name = tok.text in
  let tokens = advance tokens in
  let tokens = expect DOT tokens in

  match name with
  | "range" | "max_range" | "max_height" | "max_rectangle" ->
      let proj, tokens = expect_idf tokens in
      let tokens = expect LEFT_PAR tokens in
      let g_opt, tokens =
        if (peek tokens).kind = RIGHT_PAR then (None, tokens)
        else
          let g, tokens = parse_expr tokens in
          (Some g, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      let dq =
        match name with
        | "range" -> DotRange (proj, g_opt)
        | "max_range" -> DotMaxRange (proj, g_opt)
        | "max_height" -> DotMaxHeight (proj, g_opt)
        | "max_rectangle" -> DotMaxRect (proj, g_opt)
        | _ -> assert false
      in
      (dq, tokens)
  | "min_vel" ->
      let proj, tokens = expect_idf tokens in
      let tokens = expect LEFT_PAR tokens in
      let x, tokens = parse_expr tokens in
      let tokens = expect COMMA tokens in
      let h, tokens = parse_expr tokens in
      let g_opt, tokens =
        if (peek tokens).kind = COMMA then
          let tokens = advance tokens in
          let g, tokens = parse_expr tokens in
          (Some g, tokens)
        else (None, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      (DotMinVel (proj, x, h, g_opt), tokens)
  | "collide" | "min_dist" ->
      let tokens = expect LEFT_PAR tokens in
      let p1, tokens = expect_idf tokens in
      let tokens = expect COMMA tokens in
      let p2, tokens = expect_idf tokens in
      let g_opt, tokens =
        if (peek tokens).kind = COMMA then
          let tokens = advance tokens in
          let g, tokens = parse_expr tokens in
          (Some g, tokens)
        else (None, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      let dq =
        if name = "collide" then DotCollide (p1, p2, g_opt)
        else DotMinDist (p1, p2, g_opt)
      in
      (dq, tokens)
  | _ -> failwith (Printf.sprintf "Parse Error: unknown dot-query '%s'" name)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Condition parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_cond tokens =
  let lhs, tokens = parse_cond_atom tokens in
  parse_cond_rest lhs tokens

and parse_cond_rest lhs tokens =
  match (peek tokens).kind with
  | AND ->
      let tokens = advance tokens in
      let rhs, tokens = parse_cond_atom tokens in
      parse_cond_rest (And (lhs, rhs)) tokens
  | OR ->
      let tokens = advance tokens in
      let rhs, tokens = parse_cond_atom tokens in
      parse_cond_rest (Or (lhs, rhs)) tokens
  | _ -> (lhs, tokens)

and parse_cond_atom tokens =
  match (peek tokens).kind with
  | NOT ->
      let tokens = advance tokens in
      let c, tokens = parse_cond_atom tokens in
      (Not c, tokens)
  | IDF
    when let name = (peek tokens).text in
         let rest = advance tokens in
         (peek rest).kind = DOT && (name = "collide" || name = "min_dist") ->
      let dq, tokens = parse_dot_query tokens in
      (BoolDotQ dq, tokens)
  | _ ->
      let lhs, tokens = parse_expr tokens in
      let op, tokens = parse_cmpop tokens in
      let rhs, tokens = parse_expr tokens in
      (Cmp (op, lhs, rhs), tokens)

and parse_cmpop tokens =
  match (peek tokens).kind with
  | EQ -> (Eq, advance tokens)
  | NEQ -> (Neq, advance tokens)
  | LESS -> (Lt, advance tokens)
  | MORE -> (Gt, advance tokens)
  | LEQ -> (Leq, advance tokens)
  | GEQ -> (Geq, advance tokens)
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf "Parse Error: expected comparison operator but got '%s'"
           tok.text)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_sim_stmts tokens =
  match (peek tokens).kind with
  | RIGHT_CURL -> ([], tokens)
  | END -> failwith "Parse Error: unclosed simulate block"
  | _ ->
      let s, tokens = parse_sim_stmt tokens in
      let ss, tokens = parse_sim_stmts tokens in
      (s :: ss, tokens)

and parse_sim_stmt tokens =
  match (peek tokens).kind with
  | GRAVITY ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      (SGravity e, tokens)
  | AIR_RESISTANCE ->
      let tokens = advance tokens in
      let enabled =
        match (peek tokens).kind with
        | IDF -> (
            let name = (peek tokens).text in
            let tokens = advance tokens in
            match name with
            | "true" -> (true, tokens)
            | "false" -> (false, tokens)
            | _ -> failwith "Parse Error: air_resistance expects true/false")
        | INT | FLOAT ->
            let tok = peek tokens in
            let tokens = advance tokens in
            let v = float_of_string tok.lit_val in
            (v > 0.0, tokens)
        | _ -> failwith "Parse Error: air_resistance expects boolean or number"
      in
      let b, tokens = enabled in
      (SAirResistance b, tokens)
  | AIR_DENSITY ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      (SAirDensity e, tokens)
  | WIND_X ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      (SWindX e, tokens)
  | WIND_Y ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      (SWindY e, tokens)
  (* NEW: wind_z *)
  | WIND_Z ->
      let tokens = advance tokens in
      let e, tokens = parse_expr tokens in
      (SWindZ e, tokens)
  | PROJECTILE ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      let tokens = expect LEFT_CURL tokens in

      (* loop accumulates all projectile fields; azimuth is new and optional *)
      let rec loop tokens angle azimuth speed lf mass drag cs =
        match (peek tokens).kind with
        | RIGHT_CURL ->
            let tokens = advance tokens in
            let angle =
              match angle with
              | Some e -> e
              | None ->
                  failwith
                    (Printf.sprintf "Parse Error: projectile '%s' missing angle"
                       name)
            in
            let speed =
              match speed with
              | Some e -> e
              | None ->
                  failwith
                    (Printf.sprintf "Parse Error: projectile '%s' missing speed"
                       name)
            in
            ( SProjectile
                {
                  name;
                  angle;
                  azimuth;
                  speed;
                  launch_from = lf;
                  mass;
                  drag_coeff = drag;
                  cross_section = cs;
                },
              tokens )
        | ANGLE ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens (Some e) azimuth speed lf mass drag cs
        (* NEW: angle_azimuth field inside projectile block *)
        | ANGLE_AZIMUTH ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens angle (Some e) speed lf mass drag cs
        | SPEED ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens angle azimuth (Some e) lf mass drag cs
        | LAUNCH_FROM ->
            let tokens = advance tokens in
            let tokens = expect LEFT_PAR tokens in
            let x, tokens = parse_expr tokens in
            let tokens = expect COMMA tokens in
            let y, tokens = parse_expr tokens in
            let tokens = expect COMMA tokens in
            let e3, tokens = parse_expr tokens in
            (* 4th arg (t) is optional; if next token is COMMA parse it, else z=e3, t=0 *)
            let z, t, tokens =
              if (peek tokens).kind = COMMA then
                let tokens = advance tokens in
                let t, tokens = parse_expr tokens in
                (e3, t, tokens)
              else (e3, Num 0.0, tokens)
            in
            let tokens = expect RIGHT_PAR tokens in
            loop tokens angle azimuth speed (Some (x, y, z, t)) mass drag cs
        | MASS ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens angle azimuth speed lf (Some e) drag cs
        | DRAG_COEFFICIENT ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens angle azimuth speed lf mass (Some e) cs
        | CROSS_SECTION ->
            let tokens = advance tokens in
            let e, tokens = parse_expr tokens in
            loop tokens angle azimuth speed lf mass drag (Some e)
        | _ ->
            let tok = peek tokens in
            failwith
              (Printf.sprintf "Parse Error: unexpected '%s' in projectile block"
                 tok.text)
      in
      loop tokens None None None None None None None
  | PLOT ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      (SPlot name, tokens)
  | RANGE ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      (SRange name, tokens)
  | MAX_RANGE ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      (SMaxRange name, tokens)
  | MAX_HEIGHT ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      (SMaxHeight name, tokens)
  | MAX_RECTANGLE ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      (SMaxRect name, tokens)
  | MIN_VEL ->
      let tokens = advance tokens in
      let proj, tokens = expect_idf tokens in
      let tokens = expect TOWER tokens in
      let tokens = expect LEFT_PAR tokens in
      let x, tokens = parse_expr tokens in
      let tokens = expect COMMA tokens in
      let h, tokens = parse_expr tokens in
      let tokens = expect RIGHT_PAR tokens in
      (SMinVel (proj, x, h), tokens)
  | COLLIDE ->
      let tokens = advance tokens in
      let p1, tokens = expect_idf tokens in
      let p2, tokens = expect_idf tokens in
      (SCollide (p1, p2), tokens)
  | COLLISION_VEL ->
      let tokens = advance tokens in
      let p1, tokens = expect_idf tokens in
      let p2, tokens = expect_idf tokens in
      (SCollisionVel (p1, p2), tokens)
  | MIN_DIST ->
      let tokens = advance tokens in
      let p1, tokens = expect_idf tokens in
      let p2, tokens = expect_idf tokens in
      (SMinDist (p1, p2), tokens)
  | BOUNCE ->
      let tokens = advance tokens in
      let proj, tokens = expect_idf tokens in
      let tokens = expect TIMES tokens in
      let n, tokens = parse_expr tokens in
      let tokens = expect RESTITUTION tokens in
      let r, tokens = parse_expr tokens in
      (SBounce (proj, n, r), tokens)
  | CHECK ->
      let tokens = advance tokens in
      let c, tokens = parse_cond tokens in
      (SCheck c, tokens)
  | FOR ->
      let tokens = advance tokens in
      let var, tokens = expect_idf tokens in
      let tokens = expect FROM tokens in
      let a, tokens = parse_expr tokens in
      let tokens = expect TO tokens in
      let b, tokens = parse_expr tokens in
      let tokens = expect STEP tokens in
      let s, tokens = parse_expr tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_sim_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (SFor (var, a, b, s, body), tokens)
  | REPEAT ->
      let tokens = advance tokens in
      let n, tokens = parse_expr tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_sim_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (SRepeat (n, body), tokens)
  | WHILE ->
      let tokens = advance tokens in
      let c, tokens = parse_cond tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_sim_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (SWhile (c, body), tokens)
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf "Parse Error: unexpected token '%s' in simulate block"
           tok.text)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Top-level Projectile block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse_projectile tokens =
  let tokens = expect PROJECTILE tokens in
  let name, tokens = expect_idf tokens in
  let tokens = expect LEFT_CURL tokens in

  let rec loop tokens angle azimuth speed lf mass drag cs =
    match (peek tokens).kind with
    | RIGHT_CURL ->
        let tokens = advance tokens in
        let angle =
          match angle with
          | Some e -> e
          | None ->
              failwith
                (Printf.sprintf "Parse Error: projectile '%s' missing angle"
                   name)
        in
        let speed =
          match speed with
          | Some e -> e
          | None ->
              failwith
                (Printf.sprintf "Parse Error: projectile '%s' missing speed"
                   name)
        in
        ( Projectile
            {
              name;
              angle;
              azimuth;
              speed;
              launch_from = lf;
              mass;
              drag_coeff = drag;
              cross_section = cs;
            },
          tokens )
    | ANGLE ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens (Some e) azimuth speed lf mass drag cs
    (* NEW *)
    | ANGLE_AZIMUTH ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens angle (Some e) speed lf mass drag cs
    | SPEED ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens angle azimuth (Some e) lf mass drag cs
    | LAUNCH_FROM ->
        let tokens = advance tokens in
        let tokens = expect LEFT_PAR tokens in
        let x, tokens = parse_expr tokens in
        let tokens = expect COMMA tokens in
        let y, tokens = parse_expr tokens in
        let tokens = expect COMMA tokens in
        let e3, tokens = parse_expr tokens in
        let z, t, tokens =
          if (peek tokens).kind = COMMA then
            let tokens = advance tokens in
            let t, tokens = parse_expr tokens in
            (e3, t, tokens)
          else (e3, Num 0.0, tokens)
        in
        let tokens = expect RIGHT_PAR tokens in
        loop tokens angle azimuth speed (Some (x, y, z, t)) mass drag cs
    | MASS ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens angle azimuth speed lf (Some e) drag cs
    | DRAG_COEFFICIENT ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens angle azimuth speed lf mass (Some e) cs
    | CROSS_SECTION ->
        let tokens = advance tokens in
        let e, tokens = parse_expr tokens in
        loop tokens angle azimuth speed lf mass drag (Some e)
    | _ ->
        let tok = peek tokens in
        failwith
          (Printf.sprintf "Parse Error: unexpected '%s' in projectile block"
             tok.text)
  in
  loop tokens None None None None None None None

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Fork block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_branches tokens =
  match (peek tokens).kind with
  | RIGHT_CURL -> ([], tokens)
  | BRANCH ->
      let tokens = advance tokens in
      let label, tokens = expect_str tokens in
      let tokens = expect LEFT_CURL tokens in
      let stmts, tokens = parse_sim_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      let br = { label; br_stmts = stmts } in
      let rest, tokens = parse_branches tokens in
      (br :: rest, tokens)
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf "Parse Error: expected branch but got '%s'" tok.text)

let parse_fork tokens =
  let tokens = expect FORK tokens in
  let name, tokens = expect_idf tokens in
  let tokens = expect LEFT_CURL tokens in
  let branches, tokens = parse_branches tokens in
  let tokens = expect RIGHT_CURL tokens in
  (Fork (name, branches), tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Game block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse_game tokens =
  let tokens = expect GAME tokens in
  let tokens = expect LEFT_CURL tokens in
  let tokens = expect PLANET tokens in
  let planet, tokens = expect_idf tokens in
  let tokens = expect LEVEL tokens in
  let level, tokens = parse_expr tokens in
  let tokens = expect LIVES tokens in
  let lives, tokens = parse_expr tokens in
  let tokens = expect RIGHT_CURL tokens in
  (Game { planet; level; lives }, tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Top-level statement parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_stmt tokens =
  match (peek tokens).kind with
  | PROJECTILE -> parse_projectile tokens
  | SIMULATE ->
      let tokens = advance tokens in
      let tokens = expect LEFT_CURL tokens in
      let ss, tokens = parse_sim_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (Simulate ss, tokens)
  | FORK -> parse_fork tokens
  | GAME -> parse_game tokens
  | LET ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      let tokens = expect ASSIGN tokens in
      let e, tokens = parse_expr tokens in
      (Let (name, e), tokens)
  | SET ->
      let tokens = advance tokens in
      let name, tokens = expect_idf tokens in
      let tokens = expect ASSIGN tokens in
      let e, tokens = parse_expr tokens in
      (Set (name, e), tokens)
  | FOR ->
      let tokens = advance tokens in
      let var, tokens = expect_idf tokens in
      let tokens = expect FROM tokens in
      let a, tokens = parse_expr tokens in
      let tokens = expect TO tokens in
      let b, tokens = parse_expr tokens in
      let tokens = expect STEP tokens in
      let s, tokens = parse_expr tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (For (var, a, b, s, body), tokens)
  | REPEAT ->
      let tokens = advance tokens in
      let n, tokens = parse_expr tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (Repeat (n, body), tokens)
  | WHILE ->
      let tokens = advance tokens in
      let c, tokens = parse_cond tokens in
      let tokens = expect LEFT_CURL tokens in
      let body, tokens = parse_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      (While (c, body), tokens)
  | IF ->
      let tokens = advance tokens in
      let c, tokens = parse_cond tokens in
      let tokens = expect LEFT_CURL tokens in
      let tbody, tokens = parse_stmts tokens in
      let tokens = expect RIGHT_CURL tokens in
      let fbody_opt, tokens =
        if (peek tokens).kind = ELSE then
          let tokens = advance tokens in
          let tokens = expect LEFT_CURL tokens in
          let fb, tokens = parse_stmts tokens in
          let tokens = expect RIGHT_CURL tokens in
          (Some fb, tokens)
        else (None, tokens)
      in
      (IfElse (c, tbody, fbody_opt), tokens)
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf "Parse Error: unexpected token '%s' at top level"
           tok.text)

and parse_stmts tokens =
  match (peek tokens).kind with
  | RIGHT_CURL | END -> ([], tokens)
  | _ ->
      let s, tokens = parse_stmt tokens in
      let ss, tokens = parse_stmts tokens in
      (s :: ss, tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Entry point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse tokens =
  let program, tokens = parse_stmts tokens in
  (match (peek tokens).kind with
  | END -> ()
  | _ ->
      let tok = peek tokens in
      failwith
        (Printf.sprintf
           "Parse Error: unexpected token '%s' after end of program" tok.text));
  program
