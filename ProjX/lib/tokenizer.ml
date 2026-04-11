(* ── ProjX v3 Tokenizer with Air Resistance ── *)

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
  | MASS                    (* NEW *)
  | DRAG_COEFFICIENT        (* NEW *)
  | CROSS_SECTION          (* NEW *)
  (* ── simulate statements ── *)
  | GRAVITY
  | AIR_RESISTANCE         (* NEW *)
  | AIR_DENSITY            (* NEW *)
  | WIND_X                 (* NEW *)
  | WIND_Y                 (* NEW *)
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
  | GRAVITY          -> "GRAVITY"
  | AIR_RESISTANCE   -> "AIR_RESISTANCE"
  | AIR_DENSITY      -> "AIR_DENSITY"
  | WIND_X           -> "WIND_X"
  | WIND_Y           -> "WIND_Y"
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
  | "gravity"          -> (GRAVITY,          "null")
  | "air_resistance"   -> (AIR_RESISTANCE,   "null")
  | "air_density"      -> (AIR_DENSITY,      "null")
  | "wind_x"           -> (WIND_X,           "null")
  | "wind_y"           -> (WIND_Y,           "null")
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
  | "true"             -> (IDF,              "true")
  | "false"            -> (IDF,              "false")
  | _                  -> (IDF,              "null")
 
(* ── character helpers ── *)
let is_digit  d = d >= '0' && d <= '9'
let is_letter l = (l >= 'A' && l <= 'Z') || (l >= 'a' && l <= 'z') || l = '_'
 
(* ── lexing helpers ── *)
let rec return_num c a =
  match c with
  | h :: tail when is_digit h -> return_num tail (a ^ String.make 1 h)
  | '.' :: h2 :: tail when is_digit h2 ->
      return_float tail (a ^ "." ^ String.make 1 h2)
  | _ -> (a, c)
 
and return_float c a =
  match c with
  | h :: tail when is_digit h -> return_float tail (a ^ String.make 1 h)
  | _ -> (a, c)
 
let rec return_word c a =
  match c with
  | h :: tail when is_letter h || is_digit h ->
      return_word tail (a ^ String.make 1 h)
  | _ -> (a, c)
 
let rec read_str c a =
  match c with
  | '"' :: tail     -> (a, tail)
  | h   :: tail     -> read_str tail (a ^ String.make 1 h)
  | []              -> failwith "Lexing Error: unterminated string"
 
(* ── skip line comment ── *)
let rec skip_comment = function
  | '\n' :: tail -> tail
  | _    :: tail -> skip_comment tail
  | []           -> []
 
(* ── main tokeniser ── *)
let rec tokenize = function
  | [] -> [ { kind = END; text = ""; lit_val = "null" } ]
 
  (* whitespace *)
  | ' '  :: tail
  | '\n' :: tail
  | '\t' :: tail
  | '\r' :: tail -> tokenize tail
 
  (* line comment // *)
  | '/' :: '/' :: tail ->
      tokenize (skip_comment tail)
 
  (* two-char operators *)
  | '=' :: '=' :: tail ->
      { kind = EQ;    text = "=="; lit_val = "null" } :: tokenize tail
  | '!' :: '=' :: tail ->
      { kind = NEQ;   text = "!="; lit_val = "null" } :: tokenize tail
  | '<' :: '=' :: tail ->
      { kind = LEQ;   text = "<="; lit_val = "null" } :: tokenize tail
  | '>' :: '=' :: tail ->
      { kind = GEQ;   text = ">="; lit_val = "null" } :: tokenize tail
 
  (* single-char operators / punctuation *)
  | '=' :: tail ->
      { kind = ASSIGN;     text = "=";  lit_val = "null" } :: tokenize tail
  | '<' :: tail ->
      { kind = LESS;       text = "<";  lit_val = "null" } :: tokenize tail
  | '>' :: tail ->
      { kind = MORE;       text = ">";  lit_val = "null" } :: tokenize tail
  | '+' :: tail ->
      { kind = PLUS;       text = "+";  lit_val = "null" } :: tokenize tail
  | '-' :: tail ->
      { kind = MINUS;      text = "-";  lit_val = "null" } :: tokenize tail
  | '*' :: tail ->
      { kind = STAR;       text = "*";  lit_val = "null" } :: tokenize tail
  | '/' :: tail ->
      { kind = SLASH;      text = "/";  lit_val = "null" } :: tokenize tail
  | '{' :: tail ->
      { kind = LEFT_CURL;  text = "{";  lit_val = "null" } :: tokenize tail
  | '}' :: tail ->
      { kind = RIGHT_CURL; text = "}";  lit_val = "null" } :: tokenize tail
  | '(' :: tail ->
      { kind = LEFT_PAR;   text = "(";  lit_val = "null" } :: tokenize tail
  | ')' :: tail ->
      { kind = RIGHT_PAR;  text = ")";  lit_val = "null" } :: tokenize tail
  | '.' :: tail ->
      { kind = DOT;        text = ".";  lit_val = "null" } :: tokenize tail
  | ',' :: tail ->
      { kind = COMMA;      text = ",";  lit_val = "null" } :: tokenize tail
 
  (* string literal *)
  | '"' :: tail ->
      let (s, rest) = read_str tail "" in
      { kind = STR; text = "\"" ^ s ^ "\""; lit_val = s } :: tokenize rest
 
  (* identifier or keyword *)
  | h :: tail when is_letter h ->
      let (s, rest) = return_word (h :: tail) "" in
      let (k, v)   = key_id s in
      { kind = k; text = s; lit_val = v } :: tokenize rest
 
  (* numeric literal — int or float *)
  | h :: tail when is_digit h ->
      let (s, rest) = return_num (h :: tail) "" in
      let kind = if String.contains s '.' then FLOAT else INT in
      { kind; text = s; lit_val = s } :: tokenize rest
 
  (* illegal character *)
  | h :: tail ->
      Printf.printf "Lexing Error: illegal character '%c'\n" h;
      tokenize tail
 
(* ── debug printer ── *)
let rec print_tokens = function
  | [] -> ""
  | tok :: rest ->
      (str_tok tok.kind) ^ " " ^ tok.text ^ " " ^ tok.lit_val ^ "\n"
      ^ print_tokens rest