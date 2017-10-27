## Proving Statements about Origami Constructions

Tool to prove that certain properties hold about various origami constructions.  Given a sequence of folds, characterized by the Huzita-Justin (Huzita-Hatori) axioms, this binary generates a sequence of constraints compatible with an SMT solver for nonlinear real arithmetic.  These can be used to prove that certain properties hold or do not hold about a given construction.

One can assert that the statements specifying the construction hold, and then assert that statements specifying properties of the construction do not hold.  Then, "unsat" returned from the SMT solver proves that the properties always hold.

This is well suited towards an SMT solver, as opposed to just a CAS, because this system supports having variables in the construction.  I.e. One could prove a construction trisects an angle, no matter what the original angle.

## Usage
```ghc Main.hs
./Main <input_file> <options>
```
This will generate smt constraint file <input_file>.smt

(For generating z3py code, run with --z3)

## Language Specification

```<identifier> ::= [a-zA-Z0-9]+

; Each fold corresponds to one of the HJ Axioms.
; O3 has 2 solutions.  Here, when there are two solutions (arg1 and arg2, lines, intersect),
; the solution given is the line running through the intersection equal to the shortest
; counterclockwise rotation of line1.  For the other fold, simply reverse the argument order.
; (there is also a nondeterministic "nfold3")
;
; O5 has 2 solutions.  When there are two, fold5_sol1 gives the solution that rotates the first argument
; about the second argument to the third argument such that, when rotating clockwise, one arrives at sol1 first.
; sol2 gives the other.

<fold_decl> ::= "fold1" <identifier> <identifier>
              | "fold2" <identifier> <identifier>
              | "fold3" <identifier> <identifier>
              | "fold4" <identifier> <identifier>
              | "fold5_sol1" <identifier> <identifier> <identifier>
              | "fold5_sol2" <identifier> <identifier> <identifier>
              | <etc>

<assignment> ::= <identifier> "=" <fold_decl>

<assert> ::= "ASSERT" <constraint>

<construct> ::= "CONSTRUCT" <constraint>

<constraint> ::= "AND" '(' <constraint> ')' '(' <constraint> ')'
               | "OR" '(' <constraint> ')' '(' <constraint> ')'
               | "NEG" '(' <constraint> ')'
               | "parallel" <identifier> <identifier>
               ; more primitives...
<var_decl> ::= "VAR" <identifier>

<program> ::= <var_decl>* <assignment>* <construct>* <assert>*

```

Use the construct constraints to assert that the free variables are in some configuration.

When run with --negate, assertions are negated.  Then unsat means that the constructions (fold declarations and the CONSTRUCT constraints) imply the assertions.
