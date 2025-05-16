# Vector and Matrix Manipulation DSL

## Overview

This repository contains my code for interpreter for a domain-specific language (DSL) designed for vector and matrix manipulation. The interpreter is built using OCaml-Lex and OCaml-Yacc and was done as a part of the course requirements of COL226(Programming Languages).

## Running the program

```bash
# Build the interpreter
make

# Run a test program
./main path_to_testcase_file
```

For example:

```bash
make

./main testcases/sample_testcases/sample_tc1_inverse.dsl 
```
## Test Programs

Several testcases are included in the `testcases` folder that contains various testcases for versatile vector and matrix operations like finding inverse, gaussian elimination, finding eigen values, matrix magnitude calculation etc. The test suite also tests various cases of runtime errors, lexical errors, ill-typed programs etc.