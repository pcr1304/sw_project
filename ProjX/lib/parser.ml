(* ── ProjX v3 parser ── *)

open Tokenizer
open Ast

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Token stream helpers
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let peek = function
  | tok :: _ -> tok
  | []       -> { kind = END; text = ""; lit_val = "null" }

let advance = function
  | _ :: rest -> rest
  | []        -> []

let expect kind tokens =
  match tokens with
  | tok :: rest when tok.kind = kind -> rest
  | tok :: _ ->
      failwith (Printf.sprintf "Parse Error: expected %s but got '%s'"
                  (str_tok kind) tok.text)
  | [] ->
      failwith (Printf.sprintf "Parse Error: expected %s but got end of input"
                  (str_tok kind))

let expect_idf tokens =
  match tokens with
  | tok :: rest when tok.kind = IDF -> (tok.text, rest)
  | tok :: _ ->
      failwith (Printf.sprintf "Parse Error: expected identifier but got '%s'" tok.text)
  | [] -> failwith "Parse Error: expected identifier but got end of input"

let expect_str tokens =
  match tokens with
  | tok :: rest when tok.kind = STR -> (tok.lit_val, rest)
  | tok :: _ ->
      failwith (Printf.sprintf "Parse Error: expected string but got '%s'" tok.text)
  | [] -> failwith "Parse Error: expected string but got end of input"

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Expression parser  (+ - * /)
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* expr    = term  { ('+' | '-') term }
   term    = factor { ('*' | '/') factor }
   factor  = NUM | FLOAT | IDF | dot_query | '(' expr ')' *)

let rec parse_expr tokens =
  let (lhs, tokens) = parse_term tokens in
  parse_expr_rest lhs tokens

and parse_expr_rest lhs tokens =
  match (peek tokens).kind with
  | PLUS  ->
      let tokens = advance tokens in
      let (rhs, tokens) = parse_term tokens in
      parse_expr_rest (Binop (Add, lhs, rhs)) tokens
  | MINUS ->
      let tokens = advance tokens in
      let (rhs, tokens) = parse_term tokens in
      parse_expr_rest (Binop (Sub, lhs, rhs)) tokens
  | _ -> (lhs, tokens)

and parse_term tokens =
  let (lhs, tokens) = parse_factor tokens in
  parse_term_rest lhs tokens

and parse_term_rest lhs tokens =
  match (peek tokens).kind with
  | STAR  ->
      let tokens = advance tokens in
      let (rhs, tokens) = parse_factor tokens in
      parse_term_rest (Binop (Mul, lhs, rhs)) tokens
  | SLASH ->
      let tokens = advance tokens in
      let (rhs, tokens) = parse_factor tokens in
      parse_term_rest (Binop (Div, lhs, rhs)) tokens
  | _ -> (lhs, tokens)
 and parse_factor tokens =
  match (peek tokens).kind with
  | INT | FLOAT ->
      let tok = peek tokens in
      (Num (float_of_string tok.lit_val), advance tokens)
  | IDF
  | RANGE | MAX_RANGE | MAX_HEIGHT | MAX_RECTANGLE | MIN_VEL | COLLIDE | MIN_DIST ->
      let tok   = peek tokens in
      let rest1 = advance tokens in
      if (peek rest1).kind = DOT then
        parse_dot_query_as_expr tokens
      else
        (Var tok.text, rest1)
  | LEFT_PAR ->
      let tokens = advance tokens in
      let (e, tokens) = parse_expr tokens in
      let tokens = expect RIGHT_PAR tokens in
      (e, tokens)
  | tok ->
      failwith (Printf.sprintf "Parse Error: unexpected token '%s' in expression" (str_tok tok))
(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Dot-query parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(*  range.p()          max_range.p(g)
    min_vel.p(x,h)     min_vel.p(x,h,g)
    collide.(p1,p2)    collide.(p1,p2,g)
    min_dist.(p1,p2)   min_dist.(p1,p2,g)          *)

and parse_dot_query_as_expr tokens =
  let (dq, tokens) = parse_dot_query tokens in
  (DotQ dq, tokens)

and parse_dot_query tokens =
  let tok    = peek tokens in
  let name   = tok.text in
  let tokens = advance tokens in          (* consume query name *)
  let tokens = expect DOT tokens in       (* consume '.' *)

  match name with
  | "range" | "max_range" | "max_height" | "max_rectangle" ->
      (* range.p()  /  range.p(g) *)
      let (proj, tokens) = expect_idf tokens in
      let tokens         = expect LEFT_PAR tokens in
      let (g_opt, tokens) =
        if (peek tokens).kind = RIGHT_PAR then (None, tokens)
        else let (g, tokens) = parse_expr tokens in (Some g, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      let dq = match name with
        | "range"         -> DotRange     (proj, g_opt)
        | "max_range"     -> DotMaxRange  (proj, g_opt)
        | "max_height"    -> DotMaxHeight (proj, g_opt)
        | "max_rectangle" -> DotMaxRect   (proj, g_opt)
        | _               -> assert false
      in
      (dq, tokens)

  | "min_vel" ->
      (* min_vel.p(x, h)  /  min_vel.p(x, h, g) *)
      let (proj, tokens) = expect_idf tokens in
      let tokens         = expect LEFT_PAR tokens in
      let (x, tokens)    = parse_expr tokens in
      let tokens         = expect COMMA tokens in
      let (h, tokens)    = parse_expr tokens in
      let (g_opt, tokens) =
        if (peek tokens).kind = COMMA then
          let tokens      = advance tokens in
          let (g, tokens) = parse_expr tokens in
          (Some g, tokens)
        else (None, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      (DotMinVel (proj, x, h, g_opt), tokens)

  | "collide" | "min_dist" ->
      (* collide.(p1,p2)  /  collide.(p1,p2,g) *)
      let tokens          = expect LEFT_PAR tokens in
      let (p1, tokens)    = expect_idf tokens in
      let tokens          = expect COMMA tokens in
      let (p2, tokens)    = expect_idf tokens in
      let (g_opt, tokens) =
        if (peek tokens).kind = COMMA then
          let tokens      = advance tokens in
          let (g, tokens) = parse_expr tokens in
          (Some g, tokens)
        else (None, tokens)
      in
      let tokens = expect RIGHT_PAR tokens in
      let dq = if name = "collide"
               then DotCollide  (p1, p2, g_opt)
               else DotMinDist  (p1, p2, g_opt)
      in
      (dq, tokens)

  | _ ->
      failwith (Printf.sprintf "Parse Error: unknown dot-query '%s'" name)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Condition parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

(* cond = cond_atom { ('and'|'or') cond_atom }
   cond_atom = 'not' cond_atom | expr cmpop expr | dot_query (bool) | '(' cond ')' *)

let rec parse_cond tokens =
  let (lhs, tokens) = parse_cond_atom tokens in
  parse_cond_rest lhs tokens

and parse_cond_rest lhs tokens =
  match (peek tokens).kind with
  | AND ->
      let tokens        = advance tokens in
      let (rhs, tokens) = parse_cond_atom tokens in
      parse_cond_rest (And (lhs, rhs)) tokens
  | OR  ->
      let tokens        = advance tokens in
      let (rhs, tokens) = parse_cond_atom tokens in
      parse_cond_rest (Or (lhs, rhs)) tokens
  | _ -> (lhs, tokens)

and parse_cond_atom tokens =
  match (peek tokens).kind with
  | NOT ->
      let tokens        = advance tokens in
      let (c, tokens)   = parse_cond_atom tokens in
      (Not c, tokens)
  | IDF when
      (let name = (peek tokens).text in
       let rest = advance tokens in
       (peek rest).kind = DOT &&
       (name = "collide" || name = "min_dist")) ->
      (* boolean dot query *)
      let (dq, tokens) = parse_dot_query tokens in
      (BoolDotQ dq, tokens)
  | _ ->
      let (lhs, tokens) = parse_expr tokens in
      let (op,  tokens) = parse_cmpop tokens in
      let (rhs, tokens) = parse_expr tokens in
      (Cmp (op, lhs, rhs), tokens)

and parse_cmpop tokens =
  match (peek tokens).kind with
  | EQ   -> (Eq,  advance tokens)
  | NEQ  -> (Neq, advance tokens)
  | LESS -> (Lt,  advance tokens)
  | MORE -> (Gt,  advance tokens)
  | LEQ  -> (Leq, advance tokens)
  | GEQ  -> (Geq, advance tokens)
  | tok  ->
      failwith (Printf.sprintf "Parse Error: expected comparison operator but got '%s'" (str_tok tok))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Simulate block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_sim_stmts tokens =
  match (peek tokens).kind with
  | RIGHT_CURL -> ([], tokens)
  | END        -> failwith "Parse Error: unclosed simulate block"
  | _          ->
      let (s, tokens)  = parse_sim_stmt tokens in
      let (ss, tokens) = parse_sim_stmts tokens in
      (s :: ss, tokens)

and parse_sim_stmt tokens =
  match (peek tokens).kind with
  | GRAVITY ->
      let tokens    = advance tokens in
      let (e, tokens) = parse_expr tokens in
      (SGravity e, tokens)

  | PLOT ->
      let tokens       = advance tokens in
      let (name, tokens) = expect_idf tokens in
      (SPlot name, tokens)

  | RANGE ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      (SRange name, tokens)

  | MAX_RANGE ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      (SMaxRange name, tokens)

  | MAX_HEIGHT ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      (SMaxHeight name, tokens)

  | MAX_RECTANGLE ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      (SMaxRect name, tokens)

  | MIN_VEL ->
      let tokens         = advance tokens in
      let (proj, tokens) = expect_idf tokens in
      let tokens         = expect TOWER tokens in
      let tokens         = expect LEFT_PAR tokens in
      let (x, tokens)    = parse_expr tokens in
      let tokens         = expect COMMA tokens in
      let (h, tokens)    = parse_expr tokens in
      let tokens         = expect RIGHT_PAR tokens in
      (SMinVel (proj, x, h), tokens)

  | COLLIDE ->
      let tokens         = advance tokens in
      let (p1, tokens)   = expect_idf tokens in
      let (p2, tokens)   = expect_idf tokens in
      (SCollide (p1, p2), tokens)

  | COLLISION_VEL ->
      let tokens         = advance tokens in
      let (p1, tokens)   = expect_idf tokens in
      let (p2, tokens)   = expect_idf tokens in
      (SCollisionVel (p1, p2), tokens)

  | MIN_DIST ->
      let tokens         = advance tokens in
      let (p1, tokens)   = expect_idf tokens in
      let (p2, tokens)   = expect_idf tokens in
      (SMinDist (p1, p2), tokens)

  | BOUNCE ->
      let tokens         = advance tokens in
      let (proj, tokens) = expect_idf tokens in
      let tokens         = expect TIMES tokens in
      let (n, tokens)    = parse_expr tokens in
      let tokens         = expect RESTITUTION tokens in
      let (r, tokens)    = parse_expr tokens in
      (SBounce (proj, n, r), tokens)

  | CHECK ->
      let tokens       = advance tokens in
      let (c, tokens)  = parse_cond tokens in
      (SCheck c, tokens)

  | tok ->
      failwith (Printf.sprintf "Parse Error: unexpected token '%s' in simulate block" (str_tok tok))

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Projectile block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse_projectile tokens =
  let tokens           = expect PROJECTILE tokens in
  let (name, tokens)   = expect_idf tokens in
  let tokens           = expect LEFT_CURL tokens in
  (* parse angle / speed / launch_from in any order *)
  let rec loop tokens angle speed lf =
    match (peek tokens).kind with
    | RIGHT_CURL ->
        let tokens = advance tokens in
        let angle  = match angle with
          | Some e -> e
          | None   -> failwith (Printf.sprintf "Parse Error: projectile '%s' missing angle" name)
        in
        let speed  = match speed with
          | Some e -> e
          | None   -> failwith (Printf.sprintf "Parse Error: projectile '%s' missing speed" name)
        in
        (Projectile { name; angle; speed; launch_from = lf }, tokens)
    | ANGLE ->
        let tokens    = advance tokens in
        let (e, tokens) = parse_expr tokens in
        loop tokens (Some e) speed lf
    | SPEED ->
        let tokens    = advance tokens in
        let (e, tokens) = parse_expr tokens in
        loop tokens angle (Some e) lf
    | LAUNCH_FROM ->
        let tokens      = advance tokens in
        let tokens      = expect LEFT_PAR tokens in
        let (x, tokens) = parse_expr tokens in
        let tokens      = expect COMMA tokens in
        let (y, tokens) = parse_expr tokens in
        let tokens      = expect COMMA tokens in
        let (t, tokens) = parse_expr tokens in
        let tokens      = expect RIGHT_PAR tokens in
        loop tokens angle speed (Some (x, y, t))
    | tok ->
        failwith (Printf.sprintf "Parse Error: unexpected '%s' in projectile block" (str_tok tok))
  in
  loop tokens None None None

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Fork block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_branches tokens =
  match (peek tokens).kind with
  | RIGHT_CURL -> ([], tokens)
  | BRANCH     ->
      let tokens          = advance tokens in
      let (label, tokens) = expect_str tokens in
      let tokens          = expect LEFT_CURL tokens in
      let tokens          = expect GRAVITY tokens in
      let (g, tokens)     = parse_expr tokens in
      let (bounce_opt, tokens) =
        if (peek tokens).kind = BOUNCE then
          let tokens      = advance tokens in
          (* bounce inside fork has no projectile name — just times / restitution *)
          let tokens      = expect TIMES tokens in
          let (n, tokens) = parse_expr tokens in
          let tokens      = expect RESTITUTION tokens in
          let (r, tokens) = parse_expr tokens in
          (Some (n, r), tokens)
        else (None, tokens)
      in
      let tokens          = expect RIGHT_CURL tokens in
      let br = { label; br_gravity = g; br_bounce = bounce_opt } in
      let (rest, tokens)  = parse_branches tokens in
      (br :: rest, tokens)
  | tok ->
      failwith (Printf.sprintf "Parse Error: expected branch but got '%s'" (str_tok tok))

let parse_fork tokens =
  let tokens         = expect FORK tokens in
  let (name, tokens) = expect_idf tokens in
  let tokens         = expect LEFT_CURL tokens in
  let (branches, tokens) = parse_branches tokens in
  let tokens         = expect RIGHT_CURL tokens in
  (Fork (name, branches), tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Game block
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse_game tokens =
  let tokens           = expect GAME tokens in
  let tokens           = expect LEFT_CURL tokens in
  let tokens           = expect PLANET tokens in
  let (planet, tokens) = expect_idf tokens in
  let tokens           = expect LEVEL tokens in
  let (level,  tokens) = parse_expr tokens in
  let tokens           = expect LIVES tokens in
  let (lives,  tokens) = parse_expr tokens in
  let tokens           = expect RIGHT_CURL tokens in
  (Game { planet; level; lives }, tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Top-level statement parser
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let rec parse_stmt tokens =
  match (peek tokens).kind with
  | PROJECTILE -> parse_projectile tokens

  | SIMULATE ->
      let tokens        = advance tokens in
      let tokens        = expect LEFT_CURL tokens in
      let (ss, tokens)  = parse_sim_stmts tokens in
      let tokens        = expect RIGHT_CURL tokens in
      (Simulate ss, tokens)

  | FORK -> parse_fork tokens

  | GAME -> parse_game tokens

  | LET ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      let tokens         = expect ASSIGN tokens in
      let (e,    tokens) = parse_expr tokens in
      (Let (name, e), tokens)

  | SET ->
      let tokens         = advance tokens in
      let (name, tokens) = expect_idf tokens in
      let tokens         = expect ASSIGN tokens in
      let (e,    tokens) = parse_expr tokens in
      (Set (name, e), tokens)

  | FOR ->
      let tokens         = advance tokens in
      let (var,  tokens) = expect_idf tokens in
      let tokens         = expect FROM tokens in
      let (a,    tokens) = parse_expr tokens in
      let tokens         = expect TO tokens in
      let (b,    tokens) = parse_expr tokens in
      let tokens         = expect STEP tokens in
      let (s,    tokens) = parse_expr tokens in
      let tokens         = expect LEFT_CURL tokens in
      let (body, tokens) = parse_stmts tokens in
      let tokens         = expect RIGHT_CURL tokens in
      (For (var, a, b, s, body), tokens)

  | REPEAT ->
      let tokens         = advance tokens in
      let (n,    tokens) = parse_expr tokens in
      let tokens         = expect LEFT_CURL tokens in
      let (body, tokens) = parse_stmts tokens in
      let tokens         = expect RIGHT_CURL tokens in
      (Repeat (n, body), tokens)

  | WHILE ->
      let tokens         = advance tokens in
      let (c,    tokens) = parse_cond tokens in
      let tokens         = expect LEFT_CURL tokens in
      let (body, tokens) = parse_stmts tokens in
      let tokens         = expect RIGHT_CURL tokens in
      (While (c, body), tokens)

  | IF ->
      let tokens           = advance tokens in
      let (c,    tokens)   = parse_cond tokens in
      let tokens           = expect LEFT_CURL tokens in
      let (tbody, tokens)  = parse_stmts tokens in
      let tokens           = expect RIGHT_CURL tokens in
      let (fbody_opt, tokens) =
        if (peek tokens).kind = ELSE then
          let tokens        = advance tokens in
          let tokens        = expect LEFT_CURL tokens in
          let (fb, tokens)  = parse_stmts tokens in
          let tokens        = expect RIGHT_CURL tokens in
          (Some fb, tokens)
        else (None, tokens)
      in
      (IfElse (c, tbody, fbody_opt), tokens)

  | tok ->
      failwith (Printf.sprintf "Parse Error: unexpected token '%s' at top level" (str_tok tok))

and parse_stmts tokens =
  match (peek tokens).kind with
  | RIGHT_CURL | END -> ([], tokens)
  | _ ->
      let (s,  tokens) = parse_stmt tokens in
      let (ss, tokens) = parse_stmts tokens in
      (s :: ss, tokens)

(* ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Entry point
   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ *)

let parse tokens =
  let (program, tokens) = parse_stmts tokens in
  (match (peek tokens).kind with
   | END -> ()
   | tok -> failwith (Printf.sprintf "Parse Error: unexpected token '%s' after end of program" (str_tok tok)));
  program
