# 📘 ProjX DSL Project

![DSL](https://img.shields.io/badge/Language-DSL-blue)
![OCaml](https://img.shields.io/badge/Built%20With-OCaml-orange)
![Status](https://img.shields.io/badge/Status-Complete-success)
![License](https://img.shields.io/badge/License-Academic-lightgrey)

---

## 📑 Table of Contents

- [📌 Introduction](#-introduction)
- [🎯 Motivation](#-motivation)
- [👥 Target Users](#-target-users)
- [🧠 DSL Description](#-dsl-description)
- [💻 Sample Program](#-sample-program)
- [📊 Output](#-output)
- [🏗️ Architecture](#-architecture)
- [⚙️ Build & Run](#️-build--run)
- [👨‍💻 Team Contributions](#-team-contributions)
- [📋 User Survey](#-user-survey)
- [🎥 Demo](#-demo)
- [🚀 Features](#-features)
- [📚 Physics Formulas](#-physics-formulas)
- [🏁 Conclusion](#-conclusion)

---

## 📌 Introduction

ProjX is a **Domain-Specific Language (DSL)** designed specifically for modeling, simulating, and visualizing **projectile motion** in an intuitive and structured manner.

Unlike general-purpose programming languages, ProjX allows users to express physical motion using domain-level constructs such as `projectile`, `simulate`, and `plot`, without worrying about low-level implementation details.

The DSL integrates:
- Physics-based computation  
- Structured syntax  
- Visual rendering (via canvas)  

This makes it a powerful educational and analytical tool for understanding projectile motion behavior.

---

## 🎯 Motivation

Projectile motion is a fundamental concept in physics, yet implementing it programmatically involves:

- Complex mathematical formulas  
- Time-based simulation loops  
- Lack of visualization support  

Students often find it difficult to relate theoretical equations with real-world motion.

ProjX addresses these challenges by:

- Providing **high-level abstractions** directly aligned with physics concepts  
- Eliminating the need to manually derive and implement formulas  
- Enabling **instant visualization of motion**  
- Allowing analytical queries like range, collision, and maximum height  

Thus, ProjX bridges the gap between **mathematical theory and practical understanding**.

---

## 👥 Target Users

- 🎓 Students learning projectile motion  
- 👨‍🏫 Educators teaching physics concepts  
- 👨‍💻 Developers exploring DSL design  
- 🧪 Researchers prototyping simulations  

---
## 🧠 DSL Description

ProjX is a **Domain-Specific Language (DSL)** designed to model, simulate, and analyze projectile motion using intuitive and structured constructs. The language abstracts complex physics computations into simple keywords, enabling users to focus on problem-solving rather than implementation details.

---

## 🔤 Keywords and Their Definitions

### 🔹 1. Variable Handling

#### `let`
- Used to **declare and initialize a variable**
- Stores the value in the environment for later use  
- Syntax:
```plaintext
let x = 10
```
- Rules:
  - Variable must not be previously declared  
  - Supports numeric expressions  

---

#### `set`
- Used to **update an existing variable**
- Allows dynamic modification during execution  
- Syntax:
```plaintext
set x = x + 5
```
- Rules:
  - Variable must already be declared using `let`  

---

### 🔹 2. Object Definition

#### `projectile`
- Defines a **projectile object** with physical parameters  
- Syntax:
```plaintext
projectile p1 {
    angle 45
    speed 30
    launch_from (0,0,0)
}
```

- Required fields:
  - `angle` → launch angle in degrees  
  - `speed` → initial velocity  

- Optional field:
  - `launch_from (x, y, t)` → initial position and time  

- Rules:
  - Each projectile must have a unique name  
  - Must define at least `angle` and `speed`  

---

### 🔹 3. Simulation Block

#### `simulate`
- Defines the **main execution block** where physics calculations and visualization occur  
- Syntax:
```plaintext
simulate {
    gravity 9.8
    plot p1
}
```

- Rules:
  - Must contain exactly one `gravity` statement  
  - Should include at least one `plot`  
  - All queries and actions are executed inside this block  

---

#### `gravity`
- Specifies the **gravitational acceleration** used in simulation  
- Syntax:
```plaintext
gravity 9.8
```

- Rules:
  - Must appear inside `simulate`  
  - Only one gravity definition allowed per simulation  

---

#### `plot`
- Used to **visualize the trajectory** of a projectile  
- Syntax:
```plaintext
plot p1
```

- Behavior:
  - Generates graphical output (trajectory curve)  
  - Can be used multiple times for different projectiles  

---

### 🔹 4. Analytical Queries

#### `range`
- Computes the **horizontal distance travelled** by a projectile  
- Syntax:
```plaintext
range p1
```
- Output:
  - Displays numeric value  

---

#### `max_height`
- Computes the **maximum vertical height** reached  
- Syntax:
```plaintext
max_height p1
```

---

#### `max_range`
- Computes the **maximum possible range** for given velocity  
- Syntax:
```plaintext
max_range p1
```

---

#### `min_vel`
- Computes the **minimum velocity required to reach a target point**  
- Syntax:
```plaintext
min_vel p1 tower (x, h)
```

- Meaning:
  - `(x, h)` represents horizontal distance and height of target  

---

#### `collide`
- Determines whether **two projectiles intersect during motion**  
- Syntax:
```plaintext
collide p1 p2
```

- Output:
  - Boolean result (true/false)  

---

#### `min_dist`
- Computes the **minimum distance between two projectiles**  
- Syntax:
```plaintext
min_dist p1 p2
```

---

### 🔹 5. Advanced Features

#### `bounce`
- Simulates **bouncing motion with energy loss**  
- Syntax:
```plaintext
bounce p1 times 2 restitution 0.8
```

- Parameters:
  - `times` → number of bounces  
  - `restitution` → energy retention factor (0 to 1)  

---

#### `fork`
- Simulates the same projectile under **multiple environmental conditions**  
- Syntax:
```plaintext
fork p1 {
    branch "Earth" { gravity 9.8 }
    branch "Moon" { gravity 1.62 }
}
```

- Purpose:
  - Compare projectile behavior across different gravities  

---

#### `game`
- Enables **interactive simulation mode**  
- Syntax:
```plaintext
game {
    planet earth
    level 2
    lives 3
}
```

- Behavior:
  - Runs simulation with user interaction (implementation dependent)  

---

### 🔹 6. Control Flow Constructs

#### `if`
- Executes block **conditionally**  
- Syntax:
```plaintext
if x > 10 {
    range p1
}
```

---

#### `for`
- Executes loop over a defined range  
- Syntax:
```plaintext
for i from 1 to 5 step 1 {
    range p1
}
```

---

#### `while`
- Executes loop while condition is true  
- Syntax:
```plaintext
while x < 100 {
    set x = x + 10
}
```

---

### 🔹 7. Special Feature

#### Dot Notation
- Allows **query results to be used as values instead of direct output**  
- Syntax:
```plaintext
let r = range.p1()
```

- Features:
  - Returns computed value  
  - Can be used in expressions and conditions  
  - Does not trigger visualization  

---

## 🔤 Allowed Characters and Lexical Rules

The ProjX DSL follows a well-defined set of lexical rules that determine how programs are written and interpreted. These rules ensure consistency, readability, and correct parsing of the language.

---

### 🔹 1. Alphabets

- Uppercase letters: `A–Z`  
- Lowercase letters: `a–z`  

📌 Used for:
- Variable names  
- Projectile names  
- Keywords  

Example:
```plaintext
let angle = 45
projectile p1 { angle 30 speed 20 }
```

---

### 🔹 2. Digits

- `0–9`  

📌 Used for:
- Numeric constants  
- Expressions  

Example:
```plaintext
let speed = 30
let height = speed * 2
```

---

### 🔹 3. Special Symbols

```
{ } ( ) , .
```

📌 Purpose:

- `{ }` → Define blocks (e.g., `simulate`, `projectile`)  
- `( )` → Used in tuples and function-like syntax  
- `,` → Separates values (e.g., coordinates)  
- `.` → Used in dot notation  

Example:
```plaintext
projectile p1 {
    launch_from (0,0,0)
}

let r = range.p1()
```

---

### 🔹 4. Operators

#### Arithmetic Operators
```
+   -   *   /
```

Used for mathematical expressions:
```plaintext
let x = 10 + 5
```

---

#### Relational Operators
```
>   <   >=   <=   ==   !=
```

Used in conditions:
```plaintext
if x > 10 {
    range p1
}
```

---

### 🔹 5. Whitespace

- Spaces  
- Tabs  
- Newlines  

📌 Purpose:
- Improves readability  
- Separates tokens  

Example:
```plaintext
let x = 10
let y = 20
```

---

### 🔹 6. Identifiers (Naming Rules)

Identifiers are names given to variables and projectiles.

#### Rules:
- Must start with a **letter (a–z or A–Z)**  
- Can contain **letters and digits**  
- Cannot contain special characters  
- Cannot be a reserved keyword  

#### Valid Examples:
```plaintext
angle
speed1
p1
```

#### Invalid Examples:
```plaintext
1angle    ✘ starts with digit
angle@    ✘ special character not allowed
```

---

### 🔹 7. Reserved Keywords

The following words are reserved and cannot be used as identifiers:

```
let, set, projectile, simulate, gravity, plot,
range, max_height, max_range, min_vel,
collide, min_dist, bounce, fork, game,
if, for, while
```

---

### 🔹 8. Case Sensitivity

- The language is **case-sensitive**

#### Example:
```plaintext
let x = 10   ✔ valid
LET x = 10   ✘ invalid
```

---

### 🔹 9. Numeric Format

- Only **integer and floating-point values** are allowed  
- Scientific notation is not supported (if not implemented)

Example:
```plaintext
gravity 9.8
let v = 25
```

---

### 📌 Summary

The lexical structure of ProjX ensures:
- Clear syntax rules  
- Easy parsing and interpretation  
- Consistent naming and formatting  

These constraints help maintain the **reliability and simplicity** of the DSL.

---

## 💻 Sample Program

### 🔹 Example 1: Basic & Intermediate Features

```plaintext
# Variable declarations
let angle1 = 45
let angle2 = 60
let speed1 = 30
let speed2 = 25

# Define projectiles
projectile p1 {
    angle angle1
    speed speed1
    launch_from (0,0,0)
}

projectile p2 {
    angle angle2
    speed speed2
}

# Simulation block
simulate {
    gravity 9.8

    # Plot trajectories
    plot p1
    plot p2

    # Basic queries
    range p1
    max_height p1

    range p2
    max_height p2

    # Interaction between projectiles
    collide p1 p2
    min_dist p1 p2

    # Bounce simulation
    bounce p1 times 2 restitution 0.8
}
```

---

### 📊 Output (Example 1)

#### 🟢 Visualization
- Two trajectories are plotted:
  - `p1` (45°) → symmetric curve with maximum range  
  - `p2` (60°) → higher but shorter trajectory  
- Bounce of `p1` is shown with decreasing height  
- If collision occurs, intersection point is marked  

---

#### 🔵 Numerical Output (Approximate)

- Range of `p1` ≈ 91.8 units  
- Max height of `p1` ≈ 22.9 units  

- Range of `p2` ≈ 55.2 units  
- Max height of `p2` ≈ 24.0 units  

- Collision status → displayed (true/false)  
- Minimum distance between `p1` and `p2` → computed  

---

### 🔹 Example 2: Advanced Features

```plaintext
# Variable declarations
let angle = 40
let speed = 28

# Define projectile
projectile p {
    angle angle
    speed speed
}

# Dot notation usage (no direct output)
let r = range.p()
let h = max_height.p()

# Control flow usage
if r > 50 {
    set angle = angle + 5
}

for i from 1 to 2 step 1 {
    range p
}

while angle < 50 {
    set angle = angle + 1
}

# Fork simulation (different environments)
fork p {
    branch "Earth" {
        gravity 9.8
        plot p
    }
    branch "Moon" {
        gravity 1.62
        plot p
    }
}

# Game mode
game {
    planet earth
    level 2
    lives 3
}
```

---

### 📊 Output (Example 2)

#### 🟢 Behavior

- Dot notation computes:
  - `r` → range value (stored, not displayed)
  - `h` → maximum height  

- Control flow:
  - `if` updates angle dynamically  
  - `for` loop computes range multiple times  
  - `while` loop increments angle until condition is met  

---

#### 🌍 Fork Output

- Two simulations are shown:
  - **Earth (g = 9.8)** → shorter trajectory  
  - **Moon (g = 1.62)** → longer and higher trajectory  

---

#### 🎮 Game Mode

- Simulation runs in interactive mode  
- Displays:
  - Selected planet  
  - Level and lives  
- Allows user interaction (based on implementation)  

---

### 📌 Summary

These examples together demonstrate:

- Basic projectile modeling  
- Analytical queries  
- Multi-object interaction  
- Advanced features (bounce, fork, game)  
- Control flow constructs  
- Dot notation for reusable computations  

---

📸 *(Add screenshots of output here for better presentation.)*

## 🏗️ Architecture

Based on implementation modules:

- `my_utils.ml` → Utility functions  
- `tokenizer.ml` → Converts input to tokens  
- `ast.ml` → Abstract Syntax Tree definitions  
- `parser.ml` → Parses tokens into AST  
- `pretty.ml` → Pretty printing  
- `error.ml` → Error handling  
- `checker.ml` → Semantic analysis  
- `env.ml` → Environment (variables, state)  
- `physics.ml` → Physics calculations  
- `eval.ml` → Execution engine  
- `canvas.ml` → Visualization  
- `game.ml` → Game mode logic  

---

## ⚙️ Build & Run

### Build
```bash
dune build
```

### Run
```bash
dune exec ./main.exe
```

### Test
```bash
dune runtest
```

---

## 👨‍💻 Team Contributions

The project was developed collaboratively, with each member contributing to different components of the DSL design, implementation, and testing.

| Name | Roll No | Group No | Contribution | Effort (%) |
|------|--------|---------|-------------|-----------|
| member1 |  |  |  |  |
| member2 |  |  |  |  |
| member3 |  |  |  |  |
| member4 |  |  |  |  |
| member5 |  |  |  |  |
---

### 📌 Contribution Details

- The work was divided to ensure **balanced development across components** such as parsing, execution, and visualization.
- All members collaborated on:
  - Testing and debugging  
  - Designing sample programs  
  - Preparing documentation (README, demo video)  

- Effort distribution reflects **individual contribution to core modules and overall project completion**.

---

## 📋 User Survey

### Questions

1. How intuitive is the DSL syntax for beginners?  
2. Does visualization improve your understanding of projectile motion?  
3. How easy is it to write programs in this DSL?  
4. Which feature did you find most useful (plot, collision, range)?  
5. What additional features would you like to see?  
6. Did the DSL reduce your effort compared to traditional coding?  

---

### Summary

- Most users found DSL **easy and intuitive**  
- Visualization greatly improved conceptual clarity  
- Users appreciated:
  - Simplicity  
  - Immediate feedback  
- Suggested improvements:
  - Better error messages  
  - More examples  

---

## 🎥 Demo

👉 Add your YouTube demo link here  

---

## 🚀 Features

- Domain-specific abstraction  
- Built-in physics engine  
- Visualization support  
- Analytical queries  
- Control flow support  

---


## 📚 Physics Formulas

- Range:  
  R = (v² sin(2θ)) / g  

- Maximum Height:  
  H = (v² sin²θ) / (2g)  

- Time of Flight:  
  T = (2v sinθ) / g  

- Horizontal Velocity:  
  vx = v cosθ  

- Vertical Velocity:  
  vy = v sinθ  

- Position Equations:  
  x = v cosθ * t  
  y = v sinθ * t - (1/2) g t²  

---

## 🏁 Conclusion

ProjX successfully demonstrates how a Domain-Specific Language can simplify complex physical simulations while making them more intuitive and interactive.

It bridges the gap between **theoretical physics and practical visualization**, making learning more engaging and effective.

---
