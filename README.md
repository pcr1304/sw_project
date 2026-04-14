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

### Motivation

Traditional physics tools are often split between formulas, plotting utilities, and UI-heavy simulators. ProjX was built to bring these together in one place:

- a readable DSL for writing experiments quickly
- built-in projectile-motion queries such as range and max height
- browser-ready visualization for demos, labs, and assignments
- a workflow that supports both classroom learning and playful exploration

### Key Features

- **Physics-first modeling:** Supports projectile motion, gravity changes, bounce behavior, collisions, minimum-distance checks, maximum rectangle queries, and minimum launch velocity estimation.
- **Air resistance and wind:** Includes `air_resistance`, `air_density`, `wind_x`, and `wind_y` for more realistic simulations.
- **Familiar control flow:** Supports `let`, `set`, `for`, `repeat`, `while`, and `if/else`.
- **Planet-aware game mode:** Lets users configure levels with `planet`, `level`, and `lives`.
- **Fork-based comparison:** Compare the same projectile across multiple branches such as Earth, Moon, Mars, and Jupiter.
- **Interactive output:** Compiles `.px` scripts into browser-friendly `.html` files with visual trajectory output.
- **Static semantic checks:** Detects undeclared variables, invalid projectile definitions, bad restitution values, missing `gravity`, and other usage errors before output is generated.

### Core Language Blocks in ProjX

ProjX includes several high-level blocks that define the overall structure of a program:

- **`projectile { ... }`** for defining launch angle, speed, origin, and drag-related properties
- **`simulate { ... }`** for plotting and querying projectile behavior
- **`fork { ... }`** for side-by-side environment comparison
- **`game { ... }`** for game-inspired planetary scenarios
- **`for`, `repeat`, `while`, `if`, and `else`** for programmable control flow

### Internal Technical Documentation

The implementation details for each compiler stage are available directly in the source:

- [Tokenizer / Lexer](lib/tokenizer.ml)
- [Parser and AST construction](lib/parser.ml)
- [Semantic checker](lib/checker.ml)
- [Physics engine](lib/physics.ml)
- [JSON/HTML emission](lib/json_emit.ml)

---

## The Architecture

### Project Directory System

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
│   └── *.png / *.jpeg         # Assets for planet-based game mode
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

## Example Code

This example defines a projectile, simulates it with drag and wind, and reports useful projectile metrics.

```px
projectile rocket {
  angle 45
  speed 50
  mass 0.5
  drag_coefficient 0.47
  cross_section 0.01
  launch_from (0, 0, 0)
}

simulate {
  gravity 9.8
  air_resistance true
  air_density 1.225
  wind_x 2.0
  wind_y 0
  plot rocket
  range rocket
  max_height rocket
}
```

### Output Result

Running a `.px` script produces an `.html` file that can be opened in a browser. A ready-made example is already included:

- [Sample script](input/queries.px)
- [Generated interactive output](input/queries.html)

---

## Installation and Usage

### 1. Install Core Dependencies

Before building ProjX, install:

- **OCaml**
- **opam**
- **Dune**
- **Yojson**
- **OUnit2** (for tests)

**Windows (recommended via WSL2)**

```bash
sudo apt update
sudo apt install opam m4 pkg-config
opam init
eval $(opam env)
opam install dune yojson ounit2
```

**macOS**

```bash
brew install opam
opam init
eval $(opam env)
opam install dune yojson ounit2
```

**Linux (Ubuntu/Debian)**

```bash
sudo apt update
sudo apt install opam m4 pkg-config
opam init
eval $(opam env)
opam install dune yojson ounit2
```

### 2. Clone the Repository

```bash
git clone <your-repository-url>
cd <repository-folder>
```

### 3. Build the Compiler

```bash
dune build
```

### 4. Run a ProjX Program

```bash
dune exec projx -- input/queries.px
```

This will generate:

```text
input/queries.html
```

Open the generated HTML file in a browser to interact with the output.

### 5. Run Tests

```bash
dune runtest
```

---

## Why Users Enjoy ProjX More Than Regular Physics Engines

### 1. Parameter Sweeps with Simple Loops

ProjX lets users test many launch values quickly using built-in loops.

```px
for a from 10 to 80 step 10 {
  projectile test {
    angle a
    speed 40
  }

  simulate {
    gravity 9.8
    plot test
    range test
  }
}
```

Perfect for experiments, optimization, and assignments without repeating manual work.

---

### 2. Rich Conditional Logic

ProjX includes programming features like:

- `if`
- `else`
- `for`
- `while`
- `repeat`

This helps users create smarter and more dynamic simulations directly inside the DSL.

```px
let g = 9.8

if g > 5 {
  projectile ball {
    angle 45
    speed 35
  }

  simulate {
    gravity g
    plot ball
  }
} else {
  projectile test {
    angle 25
    speed 20
  }

  simulate {
    gravity g
    plot test
  }
}
```

---

### 3. Easy Game Building with Planet Gravity

ProjX makes it easy to build simple projectile-based game scenarios using planets, levels, and lives.

```px
game {
  planet moon
  level 2
  lives 3
}
```

Users can explore how gravity changes gameplay across **Earth**, **Moon**, **Mars**, **Jupiter**, and even the **Sun**.

---

### 4. Fork Block for Easy Comparison

The `fork` block compares the same projectile in multiple environments at once.

```px
projectile ball {
  angle 45
  speed 50
}

fork ball {
  branch "Earth"   { gravity 9.8 }
  branch "Moon"    { gravity 1.62 }
  branch "Mars"    { gravity 3.72 }
  branch "Jupiter" { gravity 24.8 }
}
```

Great for side-by-side comparison of gravity, bounce behavior, and environment-specific outcomes.

---

### 5. Why ProjX Is Unique from Regular Physics Engines

Most regular physics engines focus on raw simulation. ProjX goes further by combining a DSL, built-in physics queries, programmable control flow, and browser-ready output in one tool.

```px
projectile probe {
  angle 40
  speed 50
  mass 0.5
  drag_coefficient 0.47
  cross_section 0.01
}

let ideal = max_range.probe(9.8)

simulate {
  gravity 9.8
  air_resistance true
  air_density 1.225
  wind_x -3
  wind_y 0
  plot probe
  range probe
  max_height probe
  check range.probe() < ideal
}
```

What makes ProjX stand out:

- it is not just a simulator, but also a small programming language
- it supports both formula-based queries and drag-aware numerical simulation
- it can compare scenarios, run sweeps, and generate interactive output from one script
- it blends classroom physics, experimentation, and lightweight game ideas in the same environment

---

## In Short

**ProjX is not just a physics engine - it is a smarter, easier, and more interactive way to learn, test, and visualize projectile motion.**

---

## Use Cases

- **Academic visualization:** Great for explaining projectile motion, gravity variation, drag, and optimization in classrooms or demos.
- **Comparative experiments:** Useful for side-by-side studies of Earth vs. Moon vs. Mars trajectories.
- **Programming-based assignments:** Lets students combine physics and control flow in one script.
- **Interactive browser output:** Suitable for presentations, labs, and lightweight simulation showcases.
- **Game-inspired exploration:** Useful for creating simple gravity-based challenge scenarios with planets and levels.

---

## Group 34: Team Contributions

| Team Member | Primary Contributions | Contribution % |
| :--- | :--- | :--- |
| **Pranathi** | **Core:** Implemented the tokenizer, parser, AST, checker, pretty-printing, and error-handling modules.<br>**QA:** Wrote test cases for the compiler pipeline. | **25 %** |
| **Bhuvan** | **Core:** Enhanced the tokenizer, parser, AST, and other supporting modules to integrate newly added language features.<br>**Frontend & Docs:** Upgraded the frontend with 3D visualization using Three.js after the user study and updated the `README.md`. | **25 %** |
| **Sai Chaitanya** | **Core:** Contributed to the checker, pretty-printing, environment, and evaluation modules.<br>**Docs:** Helped document the project and contributed to the `README.md`. | **15 %** |
| **Sanjay** | **Frontend:** Developed the frontend, including the 2D visualization system and game mode.<br>**Docs & Study:** Wrote the DSL user guide used during the survey process. | **20 %** |
| **Ridhi Chopra** | **User Study & Presentation:** Supported the user survey process, created the Google Form for data collection, and prepared the project presentation. | **15 %** |

---

## Notes

- The project layout in this README matches the top-level repository structure shown in your repo.
- The included `ProjX.opam` metadata is still minimal, so the installation steps above use the concrete libraries referenced by the Dune files.
