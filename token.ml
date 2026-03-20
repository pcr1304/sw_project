type token =
  (* Keywords *)
  | PROJECTILE | SIMULATE | LAUNCH_FROM | ANGLE | SPEED | MASS | GRAVITY
  | RANGE | MAX_RANGE | MAX_HEIGHT | MAX_RECTANGLE | MIN_VELOCITY | CHECK
  | COLLIDE | COLLISION_VEL | MIN_DIST
  | LET | SET
  | FOR | FROM | TO | STEP | REPEAT | WHILE
  | IF | ELSE | AND | OR | NOT
  | MODE | PLANET | LEVEL | LIVES
  | EARTH | MOON | MARS | JUPITER | SUN

  (* Symbols *)
  | LCURL | RCURL | LPAR | RPAR
  | COMMA | ASSIGN
  | PLUS | MINUS | TIMES | DIV

  (* Comparison *)
  | EQ | NEQ | LT | GT | LE | GE

  (* Literals *)
  | INT of int
  | FLOAT of float
  | IDENT of string

  | EOF
