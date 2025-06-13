# C Static Analyzer

The goal of this homework is to implement a static analyzer that detects potentially vulnerable data flows from source points
to sink points (so called taint analysis) in C programs.
Students will write the following functions in [src/analyzer.ml](src/analyzer.ml) based on the definitions in the lecture slides:

1. the function `extract` using `extract_*` (e.g., `extract_cedge`, `extract_def`, ..) that extract initial facts.
2. the functions `derive_*` (e.g., `derive_kill`, `derive_path`, ..) that derive inductive facts.
3. the function `solve` that computes the fixed point.

In short, replace all `failwith "Not implemented"` with your own implementation.
You may assume that input programs are always **syntactically** valid.

All the submissions must satisfy the basic [requirements](https://github.com/prosyslab-classroom/cs348-information-security/blob/main/REQUIREMENT.md).

## Setup

This homework assumes that students set up the OCaml environment following the same instructions in the previous homework.
After running make under the root directory of this repository, you can run the analyzer with the provided C programs:

```console
# run the analyzer
$ ./analyzer test/example1.c
```

The command line argument specifies the input file path.
Once you complete the implementation, the following output will be printed:

```console
$ ./analyzer test/example1.c
Potential Error @ example1.c:4 (example1.c:5)
```

Each line reports a potentially vulnerable source and sink point at a certain source code location
denoted by [file]:[source_line] ([file]:[sink_line]).

## Instructions

### 1. Working with Control-flow Graph

The analyzer converts a C source file into a control-flow graph using function `of_cfile` in [src/CFG.ml](src/CFG.ml).
In the graph representation of a program, each node has a unique ID (label) and is associated with
a command (module `Command` in [src/CFG.ml](src/CFG.ml)).
Note that the IDs do not necessarily correspond to the line numbers in the C source code.

Here are the descriptions of each command (interchangably, node):

- `Assign (l, v, e)`: The command `l` assigns an expression `e` to variable `v`.
- `Source (l, v)`: The command `l` gets user input by calling the `source` function (declared in [test/homework.h](test/homework.h)) and assign the value to variable `v`. Then, the variable `v` has a tainted value.
- `Sanitizer (l, v, e)`: The command `l` sanitizes the expression `e` by calling the `sanitizer` function and assigns the sanitized value to variable `v`.
  Then, the value of `v` is not tainted anymore.
- `Sink (l, e)`: The command `l` calls the `sink` function with an expression `e` as an argument.
  If `e` contains any tainted value, the analyzer reports this function call as a potentially vulnerable point.
- `Branch l`: The command `l` is the beginning of the branching out control-flows. For simplicity, we ignore the branch condition.
- `Skip l`: The command `l` is considered to be a no-op. All the other commands except for the above will be represented as `Skip` such as `return`.

Consider the following [small C program](test/example3.c) with an if-then-else statement:

```c
0:  x = source();
1:  if (cond) {
2:    y = 1;
    } else {
3:    y = sanitize(x);
    }
4:  sink(y);
5:  return 0;
```

The corresponding control-flow graph is as follows:

```
        ┌───────┐
        │   0   │
        └───┬───┘
        ┌───┴───┐
        │   1   │
        └───┬───┘
     ┌──────┴──────┐
 ┌───┴───┐     ┌───┴───┐
 │   2   │     │   3   │
 └───┬───┘     └───┬───┘
     └──────┬──────┘
        ┌───┴───┐
        │   4   │
        └───┬───┘
        ┌───┴───┐
        │   5   │
        └───────┘
```

The program will be translated to the following set of commands:

`Source(0, x)`, `Branch(1)`, `Assign(2, y, 1)`, `Sanitizer(3, y, x)`, `Sink(4, y)`, and `Skip(5)`.

After running the analyzer, the information will be shown in `cfg.txt`.

### 2. Extracting Basic Facts

Given a program, students will extract basic facts using the `extract` function.
Function `extract` will extract the following basic facts:

- Relation `CEdge` denotes control-flow edges in a program. Control-flow edges specify the execution order.
  That is, tuple `CEdge(a, b)` represents the fact that `b` is executed right after command `a`.
  In the example, the extracted control-flow edges will be
  `CEdge(0, 1)`, `CEdge(1, 2)`, `CEdge(1, 3)`, `CEdge(2, 4)`, `CEdge(3, 4)`and `CEdge(4, 5)`.

- Relation `Source` denotes the source commands.
  The fact `Source(l)` is directly derived from the command `Source (l, v)`.
  In the example, `Source(0)` will be extracted.

- Relation `Sanitizer` denotes the sanitizer commands.
  The fact `Sanitizer(l)` is directly derived from the command `Sanitizer (l, v, e)`.
  In the example, `Sanitizer(3)` will be extracted.

- Relation `Sink` denotes the sink commands.
  The fact `Sink(l)` is directly derived from the command `Sink (l, e)`.
  In the example, `Sink(4)` will be extracted.

- Relation `Kill` denotes the definitions potentially killed at each point.
  The fact `Kill(l1, l2)` represents that the definition at `l2` is killed by the definition at `l1`.
  In the example, `Kill(2, 3)` and `Kill(3, 2)` will be extracted.
- Relations `Def` and `Use` denote definitions and uses of values, respectively.
  The facts `Def(l, v)` and `Use(l, v')` are derived from the command like `Assign (l, v, e)` where `v'` is a variable in `e`.
  Given an expression `e`, one can get all variables in `e` using function `get_vars` in the `Command` module.
  Here, `Def(0, x)`, `Def(2, y)`, `Def(3, y)`, `Use(3, x)`, and `Use(4, y)` will be extracted.

You will extract the facts by traversing control-flow graphs. The `CFG` module provides two functions:

- Function `fold_vertex` for traversing vertices of a graph
- Function `fold_edges` for traversing edges of a graph

The functions behave similarly to `List.fold_left` in the OCaml standard library.
For their usage, see the example implementations of `num_of_assignments` and `num_of_obvious_bugs` in [src/CFG.ml](src/CFG.ml).

### 3. Deriving Inductive Facts

Students will write rules to derive inductive facts.
Given a set of constraints, each function `derive_*` derives new facts following the definition in the lecture slides. There are descriptions for some nontrivial inductive facts.

- `In(a, b)` : The value defined in `b` enters to `a`
- `Out(a, b)` : The value defined in `b` goes out from `a`

### 4. Fixpoint computation

The ultimate goal of the analysis is to derive all inductive facts until reaching a fixed point.
The fixed point solving can be done by iteratevly applying the analysis rules (i.e., `derive_*`) to derive analysis facts
until there is no more changes.
Students will implement this solver in the `solve` function.

After running the analyzer, the analyzer will report potentially vulnerable points (i.e., `Alarm`).
Also, the final set of derived facts will be shown in `result.txt`.

### 5. Format of Input Programs

Input programs in this homework are assumed to have only sub-features of the C language as follows:

- Programs only have a single function definition of `main` and do not have global variables.
- You should handle assignments, branches, and loops.
- There is no function call except for the ones to `source`, `sanitizer` and `sink` that are declared in [this file](test/homework.h).
- All the other instructions are considered to be no-op.

## References

- [OCaml Standard Library](https://v2.ocaml.org/api/)
