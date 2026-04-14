# Group - 34
**Software Engineering Project - Group 34**

[![OCaml](https://img.shields.io/badge/OCaml-4.14%2B-f66a0a?style=flat-square&logo=ocaml)](https://ocaml.org/)
[![Dune](https://img.shields.io/badge/Dune-Build_System-8c52ff?style=flat-square)](https://dune.build/)
[![HTML](https://img.shields.io/badge/Output-Interactive_HTML-e34f26?style=flat-square&logo=html5&logoColor=white)](https://developer.mozilla.org/en-US/docs/Web/HTML)
[![JavaScript](https://img.shields.io/badge/Frontend-JavaScript-f7df1e?style=flat-square&logo=javascript&logoColor=black)](https://developer.mozilla.org/en-US/docs/Web/JavaScript)

# ProjX

**ProjX** is a domain-specific language (DSL) for modeling, analyzing, and visualizing projectile motion through clean, programmable scripts.

Instead of manually deriving every trajectory, comparing environments by hand, or wiring together separate plotting tools, users can define projectiles, gravity, drag, wind, collisions, bounce behavior, and even game settings inside a compact `.px` program. ProjX then compiles that program into an interactive HTML lab that can be opened directly in the browser.

## Introduction

ProjX is designed for students, educators, and physics enthusiasts who want more than a calculator and less friction than a full game engine. It combines a small programming language with projectile-motion physics so that users can:

- define reusable projectile setups
- run analytical and numerical experiments
- compare multiple gravitational environments
- explore game-style planetary levels
- generate visual output from a single source file

The compiler is implemented in **OCaml**, while the final output is rendered as an **interactive HTML/JavaScript experience**.

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
- [Group 34: Team Contributions](#group-34-team-contributions)
-  [User Survey Summary](#user-survey-summary)



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

```text
# ProjX sample

let ang=10
simulate{
gravity 9.8
 for ang from 10 to 90 step 10{
    projectile p{
      angle ang
      speed 40 }
    plot p }
}

projectile probe {
  angle 45
  speed 70
  launch_from (0, 0, 0)
}

fork probe {
  branch "Earth (g=9.8)" {
    gravity 9.8
    plot probe
    max_height probe
    range probe
  }
  branch "Moon (g=1.62)" {
    gravity 1.62
    plot probe
    max_height probe
    range probe
  }
  branch "Mars (g=3.72)" {
    gravity 3.72
    plot probe
    max_height probe
    range probe
  }
  branch "Jupiter (g=24.79)" {
    gravity 24.79
    plot probe
    max_height probe
    range probe
  }
  branch "Sun (g=274)" {
    gravity 274
    plot probe
    max_height probe
    range probe
  }
}


game {
  planet jupiter
  level 1
  lives 3
}
```

---

## Generated Output(In input folder with HTML extension)


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
- [`OUnit2`](https://opam.ocaml.org/packages/ounit2/) for unit testing


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
├── .github/
│   └── workflows/             # GitHub Actions workflows
├── bin/
│   ├── dune                   # Build config for the executable
│   └── main.ml                # Entry point (Compiles .px into .html)
├── input/
│   ├── queries.px             # Sample ProjX program
│   ├── queries.html           # Example generated output
│   └── assets/*.png , *.jpeg         # Assets for planet-based game mode
├── lib/
│   ├── ast.ml                 # Abstract syntax tree definitions
│   ├── checker.ml             # Semantic checks and validation rules
│   ├── env.ml                 # Runtime/compiler environment
│   ├── error.ml               # Error record definitions
│   ├── eval.ml                # Expression and statement evaluator
│   ├── json_emit.ml           # JSON generation for browser output
│   ├── my_utils.ml            # Utility helpers
│   ├── parser.ml              # Recursive-descent parser
│   ├── physics.ml             # Projectile-motion and drag calculations
│   ├── pretty.ml              # Pretty-print helpers
│   ├── projx.ml               # Library entry module
│   └── tokenizer.ml           # Lexer/tokenizer
├── test/
│   ├── dune                   # Test config
│   ├── test_parser.ml         # Parser tests
│   ├── test_ProjX.ml          # Project-level tests
│   └── test_tokenizer.ml      # Tokenizer tests
├── .gitignore
├── .ocamlformat
├── ProjX.opam
├── README.md
└── dune-project
```

---


## Group 34: Team Contributions

| Team Member | Primary Contributions | Contribution % |
| :--- | :--- | :--- |
| **Pranathi** | **Core:** Implemented the tokenizer, parser, AST, checker, pretty-printing, and error-handling modules.<br>**QA:** Wrote test cases for the compiler pipeline. | **25 %** |
| **Bhuvan** | **Core:** Enhanced the tokenizer, parser, AST, and other supporting modules to integrate newly added language features.<br>**Frontend & Docs:** Upgraded the frontend with 3D visualization using Three.js after the user study and updated the `README.md`. | **25 %** |
| **Sai Chaitanya** | **Core:** Contributed to the checker, pretty-printing, environment, and evaluation modules.<br>**Docs:** Helped document the project and contributed to the `README.md`. | **15 %** |
| **Sanjay** | **Frontend:** Developed the frontend, including the 2D visualization system and game mode.<br>**Docs & Study:** Wrote the DSL user guide used during the survey process. | **20 %** |
| **Ridhi Chopra** | **User Study & Presentation:** Supported the user survey process, created the Google Form for data collection, and prepared the project presentation. | **15 %** |


## User Survey Summary

We conducted a user survey to evaluate ProjX in terms of usability, learnability, effectiveness, and overall user experience.

### Questions Asked in the Survey

Participants were asked to evaluate:

- Ease of understanding the ProjX syntax.
- Ease of writing and running simulations.
- Whether ProjX improved understanding of projectile motion.
- Whether simulation results matched expectations.
- Ability to create advanced or complex simulations.
- Overall satisfaction with the tool.
- Likelihood of recommending ProjX to others.
- Main challenges faced while using ProjX.
- Suggestions for new features or improvements.

### Key Results

- **83.7%** said ProjX improved their understanding of projectile motion.
- **71.4%** said simulation results matched their expectations.
- **100%** of users successfully ran simulations.
- **85.7%** rated the syntax as easy to use.
- **85.7%** created complex simulations successfully.
- **100%** reported positive satisfaction.
- **85.7%** said they would recommend ProjX.
- **0%** reported dissatisfaction or failure cases.

### User Feedback

- Low learning barrier for new users.
- Intuitive and readable syntax.
- Helpful for teaching projectile motion concepts.
- Useful for experiments and comparisons.
- Good balance between learning and practical use.

### Improvements Based on Feedback

- Added wind feature.
- Added 3D visualization.
- Improved frontend experience and usability.

### Conclusion

ProjX demonstrated strong usability, high learning impact, and excellent user acceptance.
##Contributing
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


