# grzb changelog

## HEAD

- Redesign module structure: now the whole verifier is parametrised by a varification logic with a simple interface (exports from src/logic.rkt, src/logic/proof-obligations.rkt, and src/logic/solver.rkt).

## v0.1.1

- Exit codes (1 for logical error, 2 for syntax error, 3 for file error)

- "check" statement to run Z3 on a goal with no relation to the program

- Induction macros for nats

- Syntax: axioms before the program

## v0.1.0

- Initial version
