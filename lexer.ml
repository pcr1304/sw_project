{
open Token
exception Lexing_error of string
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '_')*

rule read = parse
  (* Whitespace *)
  | [' ' '\t' '\n' '\r'] { read lexbuf }

  (* Keywords *)
  | "projectile" { PROJECTILE }
  | "simulate" { SIMULATE }
  | "launch_from" { LAUNCH_FROM }
  | "angle" { ANGLE }
  | "speed" { SPEED }
  | "mass" { MASS }
  | "gravity" { GRAVITY }

  | "range" { RANGE }
  | "max_range" { MAX_RANGE }
  | "max_height" { MAX_HEIGHT }
  | "max_rectangle" { MAX_RECTANGLE }
  | "min_velocity" { MIN_VELOCITY }
  | "check" { CHECK }

  | "collide" { COLLIDE }
  | "collision_vel" { COLLISION_VEL }
  | "min_dist" { MIN_DIST }

  | "let" { LET }
  | "set" { SET }

  | "for" { FOR }
  | "from" { FROM }
  | "to" { TO }
  | "step" { STEP }
  | "repeat" { REPEAT }
  | "while" { WHILE }

  | "if" { IF }
  | "else" { ELSE }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }

  (* Game mode *)
  | "@mode" { MODE }
  | "@planet" { PLANET }
  | "@level" { LEVEL }
  | "@lives" { LIVES }

  | "earth" { EARTH }
  | "moon" { MOON }
  | "mars" { MARS }
  | "jupiter" { JUPITER }
  | "sun" { SUN }

  (* Operators *)
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }

  | "=" { ASSIGN }

  (* Comparisons *)
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | "<" { LT }
  | ">" { GT }

  (* Symbols *)
  | "{" { LCURL }
  | "}" { RCURL }
  | "(" { LPAR }
  | ")" { RPAR }
  | "," { COMMA }

  (* Numbers *)
  | digit+ '.' digit+ as f { FLOAT (float_of_string f) }
  | digit+ as i { INT (int_of_string i) }

  (* Identifiers *)
  | ident as id { IDENT id }

  (* EOF *)
  | eof { EOF }

  (* Error *)
  | _ as c {
      raise (Lexing_error ("Unexpected character: " ^ String.make 1 c))
    }
