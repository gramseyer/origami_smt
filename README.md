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
