# ProjX

ProjX is a domain-specific language for modeling projectile motion, running physics simulations, comparing trajectories across environments, and generating an interactive browser-based visualization. It lets users describe ballistics scenarios with a compact `.px` syntax instead of writing low-level physics or rendering code.

The current implementation supports 2D/3D projectile setup, drag and wind configuration, collision and distance queries, bounce studies, branch-based scenario comparison, and a simple planetary game mode. Running a ProjX program produces JSON on stdout and a standalone HTML simulation dashboard you can open directly in a browser.

---

## Table of Contents

- [Motivation](#motivation)
- [Target Users](#target-users)
- [Keywords & Lexical Specification](#keywords--lexical-specification)
  - [Program Structure](#program-structure)
  - [Allowed Characters](#allowed-characters)
  - [Delimiters](#delimiters)
  - [Operators](#operators)
  - [Comments](#comments)
  - [Identifiers](#identifiers)
  - [Literals & Values](#literals--values)
  - [Variables & Expressions](#variables--expressions)
  - [Projectile Keywords](#projectile-keywords)
  - [Simulation Keywords](#simulation-keywords)
  - [Fork Keywords](#fork-keywords)
  - [Game Keywords](#game-keywords)
  - [Control Flow Keywords](#control-flow-keywords)
  - [Dot Queries](#dot-queries)
  - [Semantic Rules](#semantic-rules)
  - [Errors](#errors)
- [Sample Code](#sample-code)
- [Generated Output](#generated-output)
- [Prerequisites](#prerequisites)
- [Environment Setup](#environment-setup)
- [Build & Run Guide](#build--run-guide)
- [Running Tests](#running-tests)
- [Common Issues](#common-issues)
- [Quick Start](#quick-start)
- [Compilation Pipeline](#compilation-pipeline)
- [Project Structure](#project-structure)
- [Contributing](#contributing)

---

## Motivation

Physics simulations are often split across multiple tools: one for calculations, one for plots, and another for presentation. ProjX brings those steps together in one small language. You write a scenario once, and the compiler handles tokenizing, parsing, semantic checking, evaluation, JSON generation, and HTML visualization for you.

ProjX is especially useful when you want to:

- prototype projectile-motion scenarios quickly
- compare the same launch across different gravity settings
- inspect range, peak height, collisions, and bounce behavior
- generate a visual artifact without building a separate frontend

## Target Users

- Students learning projectile motion, kinematics, and simulation
- Developers building small physics demos or visual experiments
- Instructors who want readable scenario files for teaching
- Anyone interested in turning a concise physics script into an interactive HTML output

---

## Keywords & Lexical Specification

This section describes the current ProjX syntax implemented by `tokenizer.ml`, `parser.ml`, `checker.ml`, and `eval.ml`.

### Program Structure

A `.px` file is a sequence of top-level statements. The parser currently accepts these statement forms:

- `projectile`
- `simulate`
- `fork`
- `game`
- `let`
- `set`
- `for`
- `repeat`
- `while`
- `if ... else`

Example:

```px
let g = 9.8

projectile ball {
  angle 45
  speed 30
}

simulate {
  gravity g
  plot ball
}
```

Notes:

- There is no required top-level ordering, but variables and projectiles must be declared before use.
- `projectile` blocks can appear both at top level and inside `simulate` blocks.
- Blocks are delimited with `{ ... }`; statements are separated by whitespace and block boundaries, not semicolons.

### Allowed Characters

| Category | Characters | Description |
|----------|------------|-------------|
| Letters | `a-z`, `A-Z` | Used in identifiers and keywords |
| Digits | `0-9` | Used in integer and floating-point literals |
| Underscore | `_` | Allowed in identifiers and keywords such as `launch_from` |
| Whitespace | space, tab, newline, carriage return | Used to separate tokens |
| Quotes | `"` | Used for branch labels |
| Symbols | `{ } ( ) , . = + - * / < > ! #` | Used for blocks, operators, dot queries, and comments |

### Delimiters

| Symbol | Description |
|--------|-------------|
| `{ }` | Define blocks such as `projectile`, `simulate`, `fork`, `game`, and control-flow bodies |
| `( )` | Used in `launch_from`, `tower (...)`, and dot-query arguments |
| `,` | Separates tuple/query arguments |
| `.` | Introduces a dot query such as `range.ball()` |
| `"` | Wraps branch labels |

### Operators

| Type | Operators | Description |
|------|-----------|-------------|
| Arithmetic | `+ - * /` | Numeric expressions |
| Comparison | `< > <= >= == !=` | Used inside conditions |
| Logical | `and or not` | Used in `if`, `while`, and `check` conditions |
| Assignment | `=` | Used in `let` and `set` |

#### Expression Precedence

| Level | Operators |
|-------|-----------|
| 1 | Unary `+`, unary `-` |
| 2 | `*`, `/` |
| 3 | `+`, `-` |
| 4 | `<`, `>`, `<=`, `>=`, `==`, `!=` |
| 5 | `not` |
| 6 | `and`, `or` |

Notes:

- Arithmetic operators are left-associative.
- In the current parser, `and` and `or` are parsed left-to-right at the same precedence level.
- Parentheses are supported inside arithmetic expressions such as `(a + b) * 2`.

### Comments

| Syntax | Description |
|--------|-------------|
| `# comment` | Everything from `#` to the end of the line is ignored |

Example:

```px
# Earth gravity
let g = 9.8
```

### Identifiers

| Rule | Description |
|------|-------------|
| Start | Must begin with a letter or `_` |
| Rest | May contain letters, digits, or `_` |
| Restriction | Cannot be one of the reserved keywords listed below |

Examples:

- Valid: `ball`, `sweep1`, `_temp`, `drag_profile`
- Invalid: `3ball`, `launch-from`

### Literals & Values

| Kind | Examples | Notes |
|------|----------|-------|
| Integer | `0`, `5`, `42` | Accepted wherever numeric expressions are allowed |
| Float | `9.8`, `0.05`, `-3.2` | Negative values are parsed via unary minus |
| String | `"Earth"` | Used for `branch` labels |
| Boolean flags | `true`, `false` | Accepted by `air_resistance` |

Important:

- Variables are numeric at runtime.
- There are no user-declared string or boolean variables in the current implementation.
- Planet names such as `earth` and projectile names such as `ball` are identifiers, not strings.

### Variables & Expressions

ProjX supports numeric variables and arithmetic expressions.

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `let` | `let x = 10` | Declare a numeric variable |
| `set` | `set x = x + 1` | Reassign an existing variable |

Examples:

```px
let g = 9.8
let v0 = 25 + 5
set v0 = v0 * 2
```

Expressions can contain:

- numeric literals
- numeric variables
- arithmetic operators
- parenthesized arithmetic expressions
- dot queries such as `range.ball()` or `max_height.ball(9.8)`

### Projectile Keywords

`projectile` defines a projectile configuration either at top level or inside a `simulate` block.

```px
projectile cannon {
  angle 45
  angle_azimuth 30
  speed 70
  launch_from (20, 30, 40)
  mass 5.0
  drag_coefficient 0.3
  cross_section 0.05
}
```

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `projectile` | `projectile <name> { ... }` | Start a projectile block |
| `angle` | `angle <expr>` | Elevation angle in degrees |
| `angle_azimuth` | `angle_azimuth <expr>` | Horizontal aim angle in degrees; optional, defaults to `0` |
| `speed` | `speed <expr>` | Initial launch speed |
| `launch_from` | `launch_from (x, y, z)` or `launch_from (x, y, z, t)` | Initial position and optional launch delay |
| `mass` | `mass <expr>` | Optional mass, used in drag-aware simulation |
| `drag_coefficient` | `drag_coefficient <expr>` | Optional drag coefficient |
| `cross_section` | `cross_section <expr>` | Optional cross-sectional area |

Rules:

- `angle` is mandatory.
- `speed` is mandatory.
- Other projectile fields are optional.
- Projectile properties can appear in any order inside the block.

### Simulation Keywords

`simulate` runs a physics scenario and generates output.

```px
simulate {
  gravity 9.8
  air_resistance true
  air_density 1.225
  wind_x 0
  wind_y 4
  wind_z 2
  plot cannon
  range cannon
  max_height cannon
  check range.cannon() > 200
}
```

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `simulate` | `simulate { ... }` | Start a simulation block |
| `gravity` | `gravity <expr>` | Set gravity for the block |
| `air_resistance` | `air_resistance true` | Enable or disable drag calculations |
| `air_density` | `air_density <expr>` | Air density used when drag is enabled |
| `wind_x` | `wind_x <expr>` | Wind along the X axis |
| `wind_y` | `wind_y <expr>` | Wind along the Y axis |
| `wind_z` | `wind_z <expr>` | Wind along the Z axis |
| `plot` | `plot <projectile>` | Plot a projectile trajectory |
| `range` | `range <projectile>` | Report the projectile's range |
| `max_range` | `max_range <projectile>` | Report ideal maximum range |
| `max_height` | `max_height <projectile>` | Report maximum height |
| `max_rectangle` | `max_rectangle <projectile>` | Report the maximum area rectangle under the arc |
| `min_vel` | `min_vel <projectile> tower (x, h)` | Compute minimum launch velocity needed to clear a tower |
| `collide` | `collide <p1> <p2>` | Check whether two projectiles collide |
| `collision_vel` | `collision_vel <p1> <p2>` | Report collision velocity components |
| `min_dist` | `min_dist <p1> <p2>` | Report minimum distance between two trajectories |
| `bounce` | `bounce <projectile> times <n> restitution <r>` | Generate bounce arcs |
| `check` | `check <condition>` | Evaluate a condition and record pass/fail |
| `projectile` | `projectile <name> { ... }` | Inline projectile declaration inside the simulation |

Notes:

- `air_resistance` also accepts a numeric literal; values greater than `0` are treated as enabled.
- Inline `projectile` definitions inside a `simulate` block become available to later statements in that block.

### Fork Keywords

`fork` creates multiple simulation-style branches for an already declared projectile.

```px
fork cannon {
  branch "Earth" {
    gravity 9.8
    plot cannon
  }
  branch "Moon" {
    gravity 1.6
    plot cannon
  }
}
```

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `fork` | `fork <projectile> { ... }` | Start a branching comparison |
| `branch` | `branch "Label" { ... }` | Define one branch with its own simulation statements |

Notes:

- The projectile named after `fork` must already exist.
- Each branch body uses the same simulation-statement language as `simulate`.
- In practice, each branch should contain a `gravity` statement and at least one `plot`.

### Game Keywords

`game` creates a game scenario for the generated HTML UI.

```px
game {
  planet earth
  level 1
  lives 3
}
```

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `game` | `game { ... }` | Start a game block |
| `planet` | `planet earth` | Select planet/environment |
| `level` | `level <expr>` | Numeric game level |
| `lives` | `lives <expr>` | Number of lives |

Valid planets checked by the semantic checker:

- `earth`
- `moon`
- `mars`
- `jupiter`
- `sun`

Current parser rule:

- The fields must appear in the order `planet`, then `level`, then `lives`.

### Control Flow Keywords

Control flow is available both at top level and, for most cases, inside `simulate` blocks.

| Keyword | Syntax | Description |
|--------|--------|-------------|
| `for` | `for i from 1 to 10 step 1 { ... }` | Numeric loop with inclusive upper bound behavior in the evaluator |
| `from` | used in `for` | Loop start |
| `to` | used in `for` | Loop end |
| `step` | used in `for` | Loop increment |
| `repeat` | `repeat 5 { ... }` | Repeat a block a fixed number of times |
| `while` | `while x < 10 { ... }` | Loop while a condition holds |
| `if` | `if x > 0 { ... }` | Conditional execution |
| `else` | `else { ... }` | Alternate branch |
| `let` | `let x = 5` | Variable declaration |
| `set` | `set x = 7` | Variable reassignment |

Example:

```px
for a from 15 to 75 step 15 {
  projectile sweep {
    angle a
    speed 70
  }

  simulate {
    gravity 9.8
    plot sweep
  }
}
```

### Dot Queries

ProjX also supports query-style expressions that can be used in `let`, `set`, comparisons, and `check` conditions.

| Query | Syntax | Description |
|-------|--------|-------------|
| Range | `range.ball()` or `range.ball(g)` | Range of projectile `ball` |
| Max range | `max_range.ball()` or `max_range.ball(g)` | Maximum theoretical range |
| Max height | `max_height.ball()` or `max_height.ball(g)` | Maximum height |
| Max rectangle | `max_rectangle.ball()` or `max_rectangle.ball(g)` | Maximum rectangle area under the arc |
| Min velocity | `min_vel.ball(x, h)` or `min_vel.ball(x, h, g)` | Minimum velocity to clear a tower |
| Collision | `collide.(p1, p2)` or `collide.(p1, p2, g)` | Returns `1` for hit, `0` for miss |
| Min distance | `min_dist.(p1, p2)` or `min_dist.(p1, p2, g)` | Minimum distance between two projectiles |

Example:

```px
let r = range.ball()
let mh = max_height.ball(9.8)

if range.ball() > 100 and max_height.ball() > 20 {
  let ok = 1
}
```

### Semantic Rules

The checker currently enforces these important rules:

- Variables must be declared with `let` before they are used.
- `set` can only update an already declared variable.
- A variable cannot be declared twice in the same scope with `let`.
- A projectile must be declared before it is queried, plotted, forked, or compared.
- Every `simulate` block must contain exactly one `gravity` statement.
- Every `simulate` block must contain at least one `plot` statement.
- Every `fork` branch is validated like a simulation block, so the same gravity/plot expectations apply there.
- Every `projectile` block must include both `angle` and `speed`.
- `game` planets must be one of `earth`, `moon`, `mars`, `jupiter`, or `sun`.

### Errors

Typical errors you may encounter:

| Category | Example |
|----------|---------|
| Lex error | unexpected character, unterminated string |
| Parse error | missing `angle` or `speed`, malformed `launch_from`, unexpected token in a block |
| Semantic error | using an undeclared variable, referencing an unknown projectile, invalid planet, missing `gravity`/`plot` in a simulation |
| Runtime error | division by zero |

---

## Sample Code

The following `.px` program uses variables, a 3D projectile, a drag-enabled simulation, a fork comparison, a parameter sweep, and a game block.

```px
# ProjX sample

let g_earth = 9.8
let v0 = 70

projectile cannon {
  angle 45
  angle_azimuth 30
  speed v0
  launch_from (20, 30, 40)
  mass 5.0
  drag_coefficient 0.3
  cross_section 0.05
}

simulate {
  gravity g_earth
  air_resistance true
  air_density 1.225
  wind_y 4.0
  wind_z 5.0
  plot cannon
  range cannon
  max_range cannon
  max_height cannon
  check range.cannon() > 200
}

fork cannon {
  branch "Earth" {
    gravity 9.8
    plot cannon
  }
  branch "Moon" {
    gravity 1.6
    plot cannon
  }
  branch "Mars" {
    gravity 3.72
    plot cannon
  }
}

for a from 15 to 75 step 15 {
  projectile sweep {
    angle a
    angle_azimuth a
    speed v0
  }

  simulate {
    gravity g_earth
    plot sweep
    check range.sweep() > 100
  }
}

game {
  planet earth
  level 1
  lives 3
}
```

---

## Generated Output

Running `projx` currently produces two outputs:

- pretty-printed JSON on stdout
- a standalone HTML file written next to the input file, unless you pass an explicit output path

The generated HTML includes:

- tabbed simulation/fork/game sessions
- a metrics sidebar for query results and checks
- trajectory plots
- a 2D/3D toggle for rendered simulations
- a simple game view for `game` blocks

Asset lookup:

- planet images are searched under `<directory-of-input>/assets/`
- if an image is missing, the generated page falls back to procedural visuals

---

## Prerequisites

- [OCaml](https://ocaml.org/) 5.x recommended
- [opam](https://opam.ocaml.org/)
- [dune](https://dune.build/) 3.21 or newer
- [`yojson`](https://opam.ocaml.org/packages/yojson/) for JSON output

---

## Environment Setup

### 1. Install opam and OCaml

#### Ubuntu / Debian

```bash
sudo apt update
sudo apt install opam m4 pkg-config build-essential
opam init
eval $(opam env)
opam switch create 5.1.1
eval $(opam env)
```

#### macOS

```bash
brew install opam
opam init
eval $(opam env)
opam switch create 5.1.1
eval $(opam env)
```

### 2. Install project dependencies

```bash
opam install dune yojson
```

### 3. Verify installation

```bash
ocaml -version
dune --version
opam --version
```

---

## Build & Run Guide

### 1. Clone the repository

```bash
git clone https://github.com/username/ProjX.git
cd ProjX
```

### 2. Build the project

```bash
dune build
```

### 3. Run the sample program

```bash
dune exec projx -- input/queries.px
```

This will:

1. tokenize the `.px` source
2. parse it into the AST
3. run semantic checks
4. evaluate the program and print JSON to stdout
5. write `input/queries.html`

### 4. Write to a custom HTML file

```bash
dune exec projx -- input/queries.px output.html
```

### 5. Open the generated result

Open the generated `.html` file directly in any browser. No local server is required.

---

## Running Tests

The project includes tokenizer and parser test executables under `test/`.

### Run all tests

```bash
dune test
```

### What gets tested

- tokenizer keyword recognition
- identifiers, numbers, operators, and punctuation
- projectile, simulate, fork, and game parsing
- arithmetic expressions and precedence
- loops and conditionals
- query syntax such as `range.ball()`

---

## Common Issues

- `command not found: dune`
  - Install dune with `opam install dune`

- `Library "yojson" not found`
  - Install it with `opam install yojson`

- build artifacts look stale
  - Run `dune clean` followed by `dune build`

- semantic error about missing `gravity` or `plot`
  - Ensure every `simulate` block, and every `fork` branch, includes the required statements

- semantic error about `set`
  - Declare the variable first with `let`

---

## Quick Start

```bash
git clone https://github.com/username/ProjX.git
cd ProjX
opam install dune yojson
dune build
dune exec projx -- input/queries.px
```

Then open `input/queries.html` in your browser.

---

## Compilation Pipeline

```text
.px source file
      |
      v
Tokenizer (lib/tokenizer.ml)
  - converts characters into tokens
  - recognizes keywords, numbers, strings, operators, and comments

      |
      v
Parser (lib/parser.ml)
  - builds the AST
  - parses projectile blocks, simulate blocks, fork/game blocks,
    control flow, conditions, and dot queries

      |
      v
AST (lib/ast.ml)
  - represents expressions, conditions, projectiles,
    simulation statements, branches, and top-level statements

      |
      v
Semantic Checker (lib/checker.ml)
  - validates declaration order
  - enforces required simulation statements
  - validates game planets

      |
      v
Evaluator + Physics Engine (lib/eval.ml + lib/physics.ml)
  - computes trajectories, ranges, heights, collisions, and bounces

      |
      v
JSON Emitter (lib/json_emit.ml)
  - produces scenario data for the frontend

      |
      v
HTML Generator (bin/main.ml)
  - injects JSON into a standalone interactive page
```

### Module Dependency Chain

```text
tokenizer.ml
  -> parser.ml
     -> ast.ml
  -> checker.ml
  -> eval.ml
     -> env.ml
     -> physics.ml
  -> json_emit.ml
  -> bin/main.ml
```

---

## Project Structure

```text
.
├── bin/
│   ├── dune
│   └── main.ml           # CLI entry point and HTML template generation
├── input/
│   ├── queries.px        # sample ProjX program
│   ├── queries.html      # generated HTML example
│   └── assets/           # optional planet images for the generated UI
├── lib/
│   ├── ast.ml            # AST definitions
│   ├── checker.ml        # semantic validation
│   ├── env.ml            # runtime environment
│   ├── error.ml          # error helpers/types
│   ├── eval.ml           # evaluator
│   ├── json_emit.ml      # JSON generation for the frontend
│   ├── my_utils.ml       # utility helpers
│   ├── parser.ml         # recursive-descent parser
│   ├── physics.ml        # projectile physics/math engine
│   ├── pretty.ml         # AST pretty-printer
│   ├── projx.ml          # library module re-exports
│   ├── tokenizer.ml      # tokenizer / lexer
│   └── dune
├── test/
│   ├── dune
│   ├── test_ProjX.ml
│   ├── test_parser.ml
│   └── test_tokenizer.ml
├── dune-project
└── ProjX.opam
```

---

## Contributing

1. Fork the repository.
2. Create a feature branch.
3. Make your changes.
4. Run `dune build` and `dune test`.
5. Open a pull request.

If you extend the language, make sure to update:

- `lib/tokenizer.ml`
- `lib/parser.ml`
- `lib/ast.ml`
- `lib/checker.ml`
- `lib/eval.ml`
- `lib/json_emit.ml`
- tests in `test/`
- this `README.md`
