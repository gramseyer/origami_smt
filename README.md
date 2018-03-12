## Proving Statements about Origami Constructions

Tool to prove that certain properties hold about various origami constructions.  Given a sequence of folds, characterized by the Huzita-Justin (Huzita-Hatori) axioms, this binary generates a sequence of constraints compatible with an SMT solver for nonlinear real arithmetic.  These can be used to prove that certain properties hold or do not hold about a given construction.

One can assert that the statements specifying the construction hold, and then assert that statements specifying properties of the construction do not hold.  Then, "unsat" returned from the SMT solver proves that the properties always hold.

This is well suited towards an SMT solver, as opposed to just a CAS, because this system supports having variables in the construction.  I.e. One could prove a construction trisects an angle, no matter what the original angle.

## Installation

Requires Haskell >= 8.0

Cabal Dependencies:  sbv, parsec
```
cabal install sbv
cabal install parsec
```

Install in your path:

Z3 >= 4.5.1

Yices2 >= 2.5.4

Run:
```
git clone https://github.com/gramseyer/origami_smt.git
cd origami_smt
ghc Main.hs
```

## Usage
```
ghc Main.hs
./Main <input_file> <options>
```
This will invoke both yices and z3 on the generated constraints and produce a postscript file visualizing the fold sequence.
Note: At this time, the SBV library may contain small bugs in its interactions with the yices solver.  If you encounter an error of the form
```
 *** Data.SBV: Unexpected response from the solver.
 ***    Context : getModel
 ***    Sent    : (get-value (s0))
 ***    Expected: a value binding for kind: SReal
 ***    Received: ((s0 0.355333))
```
you can run yices directly on the constraints by dumping the constraints in SMT2 format.

To just generate the smt constraints, run with --smt.
This will generate smt constraint file <input_file>.smt

For generating z3py code, run with --z3

## Language Specification

Each fold corresponds to one of the HJ Axioms.
O3 has 2 solutions.  Here, when there are two solutions (arg1 and arg2, lines, intersect),
the solution given is the line running through the intersection equal to the shortest
counterclockwise rotation of line1.  For the other fold, simply reverse the argument order.

O5 has 2 solutions.  When there are two, fold5_s1 gives the solution that rotates the first argument
about the second argument to the third argument such that, when rotating clockwise, one arrives at sol1 first.
sol2 gives the other.

O6 has potentially 3 solutions.  Each solution will be one of the tangent lines on the parabola with point 1 as a focus and line 1 as a directrix.  If one were to rotate line 1, the directrix, to the x axis such that the focus, point 1, has a positive y coordinate, then the solutions 1, 2, and 3 are ordered by their x value.  Note that equivalent solutions are counted as the same solution number - that is, if there is only one unique solution to O6, then calling fold6_2 or fold6_3 will cause an error.

```
<identifier> ::= [a-zA-Z0-9]+

<fold_decl> ::= "fold1" <identifier> <identifier> -- point, point
              | "fold2" <identifier> <identifier> -- point, point
              | "fold3" <identifier> <identifier> -- line, line
              | "fold4" <identifier> <identifier> -- point, line
              | "fold5_1" <identifier> <identifier> <identifier> -- point_move, point_rotate_about, line
              | "fold5_2" <identifier> <identifier> <identifier> 
              | "fold6_1" <identifier> <identifier> <identifier> <identifier> -- point, line, point, line, where point 1 moves onto line 1 and point 2 moves onto line 2
              | "fold6_2" ...
              | "fold6_3" ...
              | "fold7" <identifier> <identifier> <identifier> -- point, line, line - point 1 moves onto line 1, forming line perpendicular to line 2
<assignment> ::= <identifier> "=" <fold_decl>

<assert> ::= "ASSERT" <constraint>

<construct> ::= "CONSTRUCT" <constraint>

<constraint> ::= "AND" '(' <constraint> ')' '(' <constraint> ')'
               | "OR" '(' <constraint> ')' '(' <constraint> ')'
               | "NEG" '(' <constraint> ')'
               | "parallel" <identifier> <identifier>
               | ...more primitives...
               | <distance> '<'|'>'|'=' <distance>
               ;
<distance> ::= 'd(' <identifier> <identifier> ')'
             | <distance> [+-/*] <distance>
             | integer
             ;

<var_decl> ::= "VAR" <identifier>

<program> ::= <var_decl>* <assignment>* <construct>* <assert>*

```

Use the construct constraints to assert that the free variables are in some configuration.

When run with --negate, assertions are negated.  Then unsat means that the constructions (fold declarations and the CONSTRUCT constraints) imply the assertions.

## Acknowledgements:
The sources defined in biblio.bib were extremely useful in working out how to develop this system.

In particular, I used many of the equations outlined in Robert Lang's ReferenceFinder and from his discussion of axiom 6 in Origami and Geometric Constructions to calculate the fold line generated by axiom 6.
