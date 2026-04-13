(* ── ProjX v4 Tokenizer — 3D upgrade ── *)

type token_kind =
  (* ── block keywords ── *)
  | PROJECTILE
  | SIMULATE
  | FORK
  | GAME
  (* ── projectile properties ── *)
  | ANGLE
  | SPEED
  | LAUNCH_FROM
  | MASS
  | DRAG_COEFFICIENT
  | CROSS_SECTION
  | ANGLE_AZIMUTH          (* NEW: horizontal aim direction for 3D *)
  (* ── simulate statements ── *)
  | GRAVITY
  | AIR_RESISTANCE
  | AIR_DENSITY
  | WIND_X
  | WIND_Y
  | WIND_Z                 (* NEW: wind along Z axis for 3D *)
  | PLOT
  | RANGE
  | MAX_RANGE
  | MAX_HEIGHT
  | MAX_RECTANGLE
  | MIN_VEL
  | COLLIDE
  | COLLISION_VEL
  | MIN_DIST
  | BOUNCE
  | TIMES
  | RESTITUTION
  | CHECK
  | TOWER
  (* ── fork keywords ── *)
  | BRANCH
  (* ── game keywords ── *)
  | PLANET
  | LEVEL
  | LIVES
  (* ── variables / control flow ── *)
  | LET
  | SET
  | FOR
  | FROM
  | TO
  | STEP
  | REPEAT
  | WHILE
  | IF
  | ELSE
  (* ── logical operators ── *)
  | AND
  | OR
  | NOT
  (* ── comparison operators ── *)
  | EQ        (* == *)
  | NEQ       (* != *)
  | LESS      (* <  *)
  | MORE      (* >  *)
  | LEQ       (* <= *)
  | GEQ       (* >= *)
  (* ── arithmetic operators ── *)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  (* ── punctuation ── *)
  | ASSIGN    (* =  *)
  | LEFT_CURL
  | RIGHT_CURL
  | LEFT_PAR
  | RIGHT_PAR
  | DOT
  | COMMA
  (* ── literals / identifiers ── *)
  | IDF
  | STR
  | INT
  | FLOAT
  (* ── end of input ── *)
  | END

let str_tok = function
  | PROJECTILE       -> "PROJECTILE"
  | SIMULATE         -> "SIMULATE"
  | FORK             -> "FORK"
  | GAME             -> "GAME"
  | ANGLE            -> "ANGLE"
  | SPEED            -> "SPEED"
  | LAUNCH_FROM      -> "LAUNCH_FROM"
  | MASS             -> "MASS"
  | DRAG_COEFFICIENT -> "DRAG_COEFFICIENT"
  | CROSS_SECTION    -> "CROSS_SECTION"
  | ANGLE_AZIMUTH    -> "ANGLE_AZIMUTH"
  | GRAVITY          -> "GRAVITY"
  | AIR_RESISTANCE   -> "AIR_RESISTANCE"
  | AIR_DENSITY      -> "AIR_DENSITY"
  | WIND_X           -> "WIND_X"
  | WIND_Y           -> "WIND_Y"
  | WIND_Z           -> "WIND_Z"
  | PLOT             -> "PLOT"
  | RANGE            -> "RANGE"
  | MAX_RANGE        -> "MAX_RANGE"
  | MAX_HEIGHT       -> "MAX_HEIGHT"
  | MAX_RECTANGLE    -> "MAX_RECTANGLE"
  | MIN_VEL          -> "MIN_VEL"
  | COLLIDE          -> "COLLIDE"
  | COLLISION_VEL    -> "COLLISION_VEL"
  | MIN_DIST         -> "MIN_DIST"
  | BOUNCE           -> "BOUNCE"
  | TIMES            -> "TIMES"
  | RESTITUTION      -> "RESTITUTION"
  | CHECK            -> "CHECK"
  | TOWER            -> "TOWER"
  | BRANCH           -> "BRANCH"
  | PLANET           -> "PLANET"
  | LEVEL            -> "LEVEL"
  | LIVES            -> "LIVES"
  | LET              -> "LET"
  | SET              -> "SET"
  | FOR              -> "FOR"
  | FROM             -> "FROM"
  | TO               -> "TO"
  | STEP             -> "STEP"
  | REPEAT           -> "REPEAT"
  | WHILE            -> "WHILE"
  | IF               -> "IF"
  | ELSE             -> "ELSE"
  | AND              -> "AND"
  | OR               -> "OR"
  | NOT              -> "NOT"
  | EQ               -> "EQ"
  | NEQ              -> "NEQ"
  | LESS             -> "LESS"
  | MORE             -> "MORE"
  | LEQ              -> "LEQ"
  | GEQ              -> "GEQ"
  | PLUS             -> "PLUS"
  | MINUS            -> "MINUS"
  | STAR             -> "STAR"
  | SLASH            -> "SLASH"
  | ASSIGN           -> "ASSIGN"
  | LEFT_CURL        -> "LEFT_CURL"
  | RIGHT_CURL       -> "RIGHT_CURL"
  | LEFT_PAR         -> "LEFT_PAR"
  | RIGHT_PAR        -> "RIGHT_PAR"
  | DOT              -> "DOT"
  | COMMA            -> "COMMA"
  | IDF              -> "IDF"
  | STR              -> "STR"
  | INT              -> "INT"
  | FLOAT            -> "FLOAT"
  | END              -> "END"

type token = { kind : token_kind; text : string; lit_val : string }

(* ── keyword table ── *)
let key_id s =
  match s with
  | "projectile"       -> (PROJECTILE,       "null")
  | "simulate"         -> (SIMULATE,         "null")
  | "fork"             -> (FORK,             "null")
  | "game"             -> (GAME,             "null")
  | "angle"            -> (ANGLE,            "null")
  | "speed"            -> (SPEED,            "null")
  | "launch_from"      -> (LAUNCH_FROM,      "null")
  | "mass"             -> (MASS,             "null")
  | "drag_coefficient" -> (DRAG_COEFFICIENT, "null")
  | "cross_section"    -> (CROSS_SECTION,    "null")
  | "angle_azimuth"    -> (ANGLE_AZIMUTH,    "null")
  | "gravity"          -> (GRAVITY,          "null")
  | "air_resistance"   -> (AIR_RESISTANCE,   "null")
  | "air_density"      -> (AIR_DENSITY,      "null")
  | "wind_x"           -> (WIND_X,           "null")
  | "wind_y"           -> (WIND_Y,           "null")
  | "wind_z"           -> (WIND_Z,           "null")
  | "plot"             -> (PLOT,             "null")
  | "range"            -> (RANGE,            "null")
  | "max_range"        -> (MAX_RANGE,        "null")
  | "max_height"       -> (MAX_HEIGHT,       "null")
  | "max_rectangle"    -> (MAX_RECTANGLE,    "null")
  | "min_vel"          -> (MIN_VEL,          "null")
  | "collide"          -> (COLLIDE,          "null")
  | "collision_vel"    -> (COLLISION_VEL,    "null")
  | "min_dist"         -> (MIN_DIST,         "null")
  | "bounce"           -> (BOUNCE,           "null")
  | "times"            -> (TIMES,            "null")
  | "restitution"      -> (RESTITUTION,      "null")
  | "check"            -> (CHECK,            "null")
  | "tower"            -> (TOWER,            "null")
  | "branch"           -> (BRANCH,           "null")
  | "planet"           -> (PLANET,           "null")
  | "level"            -> (LEVEL,            "null")
  | "lives"            -> (LIVES,            "null")
  | "let"              -> (LET,              "null")
  | "set"              -> (SET,              "null")
  | "for"              -> (FOR,              "null")
  | "from"             -> (FROM,             "null")
  | "to"               -> (TO,               "null")
  | "step"             -> (STEP,             "null")
  | "repeat"           -> (REPEAT,           "null")
  | "while"            -> (WHILE,            "null")
  | "if"               -> (IF,               "null")
  | "else"             -> (ELSE,             "null")
  | "and"              -> (AND,              "null")
  | "or"               -> (OR,               "null")
  | "not"              -> (NOT,              "null")
  | "true"             -> (IDF,              "null")
  | "false"            -> (IDF,              "null")
  | s                  -> (IDF,              s)

(* ── tokenizer ── *)
let is_digit c    = c >= '0' && c <= '9'
let is_alpha c    = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_alnum c    = is_alpha c || is_digit c

let make_tok kind text lit_val = { kind; text; lit_val }

let rec tokenize = function
  | [] -> [make_tok END "" "null"]

  (* skip whitespace *)
  | c :: rest when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
      tokenize rest

  (* single-line comment *)
  | '#' :: rest ->
      let rec skip = function
        | [] -> tokenize []
        | '\n' :: r -> tokenize r
        | _ :: r    -> skip r
      in skip rest

  (* string literal *)
  | '"' :: rest ->
      let rec read acc = function
        | [] -> failwith "Lex error: unterminated string"
        | '"' :: r -> (String.concat "" (List.rev_map (String.make 1) acc), r)
        | c :: r   -> read (c :: acc) r
      in
      let (s, r) = read [] rest in
      make_tok STR ("\"" ^ s ^ "\"") s :: tokenize r

  (* number *)
  | c :: _ as cs when is_digit c ->
      let rec read_num acc = function
        | d :: r when is_digit d -> read_num (acc ^ String.make 1 d) r
        | '.' :: d :: r when is_digit d ->
            read_frac (acc ^ ".") (d :: r) true
        | rest -> (acc, false, rest)
      and read_frac acc cs saw_dot =
        match cs with
        | d :: r when is_digit d -> read_frac (acc ^ String.make 1 d) r saw_dot
        | rest -> (acc, true, rest)
      in
      let (num_str, is_float, rest) = read_num "" cs in
      let kind = if is_float then FLOAT else INT in
      make_tok kind num_str num_str :: tokenize rest

  (* identifier / keyword *)
  | c :: _ as cs when is_alpha c ->
      let rec read acc = function
        | d :: r when is_alnum d -> read (acc ^ String.make 1 d) r
        | rest -> (acc, rest)
      in
      let (word, rest) = read "" cs in
      let (kind, lit) = key_id word in
      make_tok kind word lit :: tokenize rest

  (* two-char operators *)
  | '=' :: '=' :: rest -> make_tok EQ  "==" "null" :: tokenize rest
  | '!' :: '=' :: rest -> make_tok NEQ "!=" "null" :: tokenize rest
  | '<' :: '=' :: rest -> make_tok LEQ "<=" "null" :: tokenize rest
  | '>' :: '=' :: rest -> make_tok GEQ ">=" "null" :: tokenize rest

  (* single-char tokens *)
  | '=' :: rest -> make_tok ASSIGN    "="  "null" :: tokenize rest
  | '<' :: rest -> make_tok LESS      "<"  "null" :: tokenize rest
  | '>' :: rest -> make_tok MORE      ">"  "null" :: tokenize rest
  | '+' :: rest -> make_tok PLUS      "+"  "null" :: tokenize rest
  | '-' :: rest -> make_tok MINUS     "-"  "null" :: tokenize rest
  | '*' :: rest -> make_tok STAR      "*"  "null" :: tokenize rest
  | '/' :: rest -> make_tok SLASH     "/"  "null" :: tokenize rest
  | '{' :: rest -> make_tok LEFT_CURL "{"  "null" :: tokenize rest
  | '}' :: rest -> make_tok RIGHT_CURL "}" "null" :: tokenize rest
  | '(' :: rest -> make_tok LEFT_PAR  "("  "null" :: tokenize rest
  | ')' :: rest -> make_tok RIGHT_PAR ")"  "null" :: tokenize rest
  | '.' :: rest -> make_tok DOT       "."  "null" :: tokenize rest
  | ',' :: rest -> make_tok COMMA     ","  "null" :: tokenize rest

  | c :: _ ->
      failwith (Printf.sprintf "Lex error: unexpected character '%c'" c)
