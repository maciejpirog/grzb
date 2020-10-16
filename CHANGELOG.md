# grzb changelog

## 0.2.0

- Support for arrays implemented by introducing L-expressions in A-exprs (but not exposing them to the user), a la Mike Gordon's notes

- Redesigned module structure: now the whole verifier is parametrised by a verification logic with a simple interface (exports from src/logic.rkt, src/logic/proof-obligations.rkt, and src/logic/solver.rkt)

- Fix an ugly bug in substitution, which is now truly capture-avoiding

## 0.1.1

- Exit codes (1 for logical error, 2 for syntax error, 3 for file error)

- "check" statement to run Z3 on a goal with no relation to the program

- Induction macros for nats

- Syntax: axioms before the program

## 0.1.0

- Initial version
