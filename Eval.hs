module Eval (computeTransform) where

import Parser
import State
import qualified Data.Map as Map
import Data.List as List

computeTransform :: Parser.Program -> TransformState ()
computeTransform (Parser.PROGRAM vardecls decls constructConstraints assertConstraints) = 
    executeVarDecls vardecls 
        >> executeDecls decls
        >> executeConstructions constructConstraints
        >> executeConstraints assertConstraints

executeVarDecls :: [Parser.VarDeclaration] -> TransformState ()
executeVarDecls = List.foldr ((>>).addVarDecl) State.doNothing

executeDecls :: [Parser.Declaration] -> TransformState ()
executeDecls = List.foldr ((>>) . addDecl) State.doNothing

executeConstructions :: [Parser.Constraint] -> TransformState ()
executeConstructions = List.foldr ((>>) . (addConstraint addExpr)) State.doNothing

executeConstraints :: [Parser.Constraint] -> TransformState ()
executeConstraints = List.foldr ((>>) . (addConstraint addConstraintExpr)) State.doNothing

distance :: (State.Variable, State.Variable)
         -> (State.Variable, State.Variable)
         -> Expr
distance (x1, y1) (x2, y2) = OP "+" (SQR (OP "-" (VAR x1) (VAR x2)))
                                    (SQR (OP "-" (VAR y1) (VAR y2)))

addVarDecl :: Parser.VarDeclaration -> TransformState ()
addVarDecl (Parser.VAR_DECL var) = do
    State.addPoint var
    p <- State.getPointVars var
    addExpr $ pointInBox p
    return ()

addDecl :: Parser.Declaration -> TransformState ()
addDecl (Parser.DEC_FOLD1 var arg1 arg2)      = addFold1Decl var arg1 arg2
addDecl (Parser.DEC_FOLD2 var arg1 arg2)      = addFold2Decl var arg1 arg2
addDecl (Parser.DEC_FOLD3 var arg1 arg2)      = addFold3Decl var arg1 arg2
addDecl (Parser.DEC_FOLD4 var arg1 arg2)      = addFold4Decl var arg1 arg2
addDecl (Parser.DEC_FOLD5 1 var pointMove pointCenter line)
                                              = addFold5DeclSol1 var pointMove pointCenter line
addDecl (Parser.DEC_FOLD5 2 var pointMove pointCenter line)
                                              = addFold5DeclSol2 var pointMove pointCenter line
addDecl (Parser.DEC_FOLD6 solnum var p1 l1 p2 l2)
                                              = addFold6Decl solnum var p1 l1 p2 l2
addDecl (Parser.DEC_FOLD7 var point l1 l2)    = addFold7Decl var point l1 l2
addDecl (Parser.DEC_INTERSECT var arg1 arg2)  = addIntersectDecl var arg1 arg2

addFold1Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold1Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    addExpr $ ASSIGN x1 (VAR a1)
    addExpr $ ASSIGN y1 (VAR b1)
    addExpr $ ASSIGN x2 (VAR a2)
    addExpr $ ASSIGN y2 (VAR b2)
    --sanity check
    addExpr $ NEG (OP "and" (OP "=" (VAR x1) (VAR x2))
                            (OP "=" (VAR y1) (VAR y2)))

addFold2Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold2Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    let p1x = getAvg (VAR a1) (VAR a2)
    let p1y = getAvg (VAR b1) (VAR b2)
    let dx = OP "-" (VAR a2) (VAR a1)
    let dy = OP "-" (VAR b2) (VAR b1)
    let dx' = OP "-" (CONST 0) dy
    let dy' = dx
    addExpr $ ASSIGN x1 p1x
    addExpr $ ASSIGN y1 p1y
    addExpr $ ASSIGN x2 (OP "+" p1x dx')
    addExpr $ ASSIGN y2 (OP "+" p1y dy')
    addExpr $ NEG (OP "and" (OP "=" (VAR a1) (VAR a2))
                            (OP "=" (VAR b1) (VAR b2)))

assignIntersectPtData :: (Expr, Expr, Expr, Expr)
                      -> (Expr, Expr, Expr, Expr)
                      -> (Expr, Expr, Expr)
assignIntersectPtData (x1, y1, x2, y2) (x3, y3, x4, y4) = (xNum, yNum, denom) where
    denom = OP "-" (OP "*" (OP "-" x1 x2) (OP "-" y3 y4))
                   (OP "*" (OP "-" y1 y2) (OP "-" x3 x4))
    partA = OP "-" (OP "*" x1 y2) (OP "*" y1 x2)
    partB = OP "-" (OP "*" x3 y4) (OP "*" y3 x4)
    xNum = OP "-" (OP "*" partA (OP "-" x3 x4)) (OP "*" (OP "-" x1 x2) partB)
    yNum = OP "-" (OP "*" partA (OP "-" y3 y4)) (OP "*" (OP "-" y1 y2) partB)

addFold3Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold3Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1, a2, b2) <- State.getLineVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2

    let (xNum, yNum, denom) = assignIntersectPtData (VAR a1, VAR b1, VAR a2, VAR b2)
                                                    (VAR c1, VAR d1, VAR c2, VAR d2)
    
    (denomV, parCondV) <- State.freshNamedVarPair "denom"
    addExpr $ ASSIGN denomV denom

    let parallelConstr = OP "=" (CONST 0) denom
    let nonParallelConstr = NEG parallelConstr

    dist1 <- getConstructSqrt (distance (a1, b1) (a2, b2))
    dist2 <- getConstructSqrt (distance (c1, d1) (c2, d2))

    let intNorm1x = OP "/" (OP "-" (VAR a2) (VAR a1)) (VAR dist1)
    let intNorm1y = OP "/" (OP "-" (VAR b2) (VAR b1)) (VAR dist1)
    let intNorm2x = OP "/" (OP "-" (VAR c2) (VAR c1)) (VAR dist2)
    let intNorm2y = OP "/" (OP "-" (VAR d2) (VAR d1)) (VAR dist2)
    (vx, vy) <- State.freshNamedVarPair "diagnostic"
    let mpdX = getAvg intNorm1x intNorm2x
    let mpdY = getAvg intNorm1y intNorm2y
    let mpdX' = getAvg (OP "*" (CONST (-1)) intNorm2x) intNorm1x
    let mpdY' = getAvg (OP "*" (CONST (-1)) intNorm2y) intNorm1y

    addExpr $ ASSIGN vx mpdX
    addExpr $ ASSIGN vy mpdY

    let mpX = OP "+" mpdX (VAR x1)
    let mpY = OP "+" mpdY (VAR y1)
    let mpX' = OP "+" mpdX' (VAR x1)
    let mpY' = OP "+" mpdY' (VAR y1)

    let parX1 = getAvg (VAR a1) (VAR c1)
    let parY1 = getAvg (VAR b1) (VAR d1)
    let parX2 = getAvg (VAR a1) (VAR c2)
    let parY2 = getAvg (VAR b1) (VAR d2)
    (crossProdV, _) <- State.freshNamedVarPair "fold3CrossDot"

    let parCond = OP "and" parallelConstr
                           (ASSIGNS [(parCondV, CONST 12), (x1, parX1), (y1, parY1), (x2, parX2), (y2, parY2)])
                      --     (OP "and" (OP "and" (OP "=" (VAR x1) parX1)
                        --                       (OP "=" (VAR y1) parY1))
                          --           (OP "and" (OP "=" (VAR x2) parX2)
                            --                   (OP "=" (VAR y2) parY2)))
    let crossProd = OP "-" (OP "*" (OP "-" (VAR a2) (VAR a1))
                                   (OP "-" (VAR d2) (VAR d1)))
                           (OP "*" (OP "-" (VAR b2) (VAR b1))
                                   (OP "-" (VAR c2) (VAR c1)))
    addExpr $ ASSIGN crossProdV crossProd    
    
    let selectConstr = LIST "or" [OP "and" (OP "<" (CONST 0) (VAR crossProdV))
                                           (ASSIGNS [(x2, mpX), (y2, mpY)]),
                                     --      (OP "and" (OP "=" (VAR x2) mpX)
                                       --            (OP "=" (VAR y2) mpY)),
                                  OP "and" (OP ">" (CONST 0) (VAR crossProdV))
                                           (ASSIGNS [(x2, mpX'), (y2, mpY')]),
                                         --  (OP "and" (OP "=" (VAR x2) mpX')
                                           --        (OP "=" (VAR y2) mpY')),
                                  OP "=" (CONST 0) (VAR crossProdV)]
    let nParCond = OP "and" nonParallelConstr
                            (ASSIGNS [(parCondV, CONST 1), (x1, OP "/" xNum denom), (y1, OP "/" yNum denom)])
    
    let totalCond = OP "or" parCond nParCond
    addExpr totalCond
    addExpr selectConstr

addFold4Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold4Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a, b) <- State.getPointVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2
    let preRotateX = OP "-" (VAR c2) (VAR c1)
    let preRotateY = OP "-" (VAR d2) (VAR d1)
    let postRotateX = preRotateY
    let postRotateY = OP "-" (CONST 0) preRotateX
 --   let eqAssert = OP "and" (OP "=" (VAR x2) (OP "+" postRotateX (VAR a)))
 --                           (OP "=" (VAR y2) (OP "+" postRotateY (VAR b)))
  --  let eqAssert' = OP "and" (OP "=" (VAR x1) (VAR a))
 --                            (OP "=" (VAR y1) (VAR b))
   -- addExpr eqAssert
    --addExpr eqAssert'
    addExpr $ ASSIGN x1 (VAR a)
    addExpr $ ASSIGN y1 (VAR b)
    addExpr $ ASSIGN x2 (OP "+" postRotateX (VAR a))
    addExpr $ ASSIGN y2 (OP "+" postRotateY (VAR b))    

addFold5DeclSol1 :: Parser.Identifier
                 -> Parser.Identifier
                 -> Parser.Identifier
                 -> Parser.Identifier
                 -> TransformState()
addFold5DeclSol1 = addFold5DeclGenerator True

addFold5DeclSol2 :: Parser.Identifier
                 -> Parser.Identifier
                 -> Parser.Identifier
                 -> Parser.Identifier
                 -> TransformState ()
addFold5DeclSol2 = addFold5DeclGenerator False

addFold5DeclGenerator :: Bool
                      -> Parser.Identifier
                      -> Parser.Identifier
                      -> Parser.Identifier
                      -> Parser.Identifier
                      -> TransformState ()
addFold5DeclGenerator flag var pointMove pointOnLine line = do
    (xc, yc) <- State.getPointVars pointOnLine
    (a, b) <- State.getPointVars pointMove
    (c1, d1, c2, d2) <- State.getLineVars line
    (x1, y1, x2, y2) <- State.addLine var
    let r2 = distance (xc, yc) (a, b)
    --let quadA = OP "+" (SQR (OP "-" (VAR c2) (VAR c1))) (SQR (OP "-" (VAR d2) (VAR d1)))
   -- let quadB = OP "+" (OP "*" (CONST 2) (OP "*" (OP "-" (VAR c2) (VAR c1))
   --                                              (OP "-" (VAR c1) (VAR c2))))
   --                    (OP "*" (CONST 2) (OP "*" (OP "-" (VAR d2) (VAR d1))
   --                                              (OP "-" (VAR d1) (VAR d2))))
   -- let quadC = OP "-" (OP "-" (OP "+" (OP "+" (OP "+" (SQR (VAR d1)) (SQR (VAR c1)))
   --                                            (SQR (VAR xc)))
    --                                   (SQR (VAR yc)))
     --                          (OP "*" (CONST 2) (OP "+" (OP "*" (VAR xc) (VAR c1))
       --                                                  (OP "*" (VAR yc) (VAR d1)))))
         --              r2
    

    (c1v, d1v) <- State.freshNamedVarPair "c1d1"
    (c2v, d2v) <- State.freshNamedVarPair "c2d2"
    (xcv, ycv) <- State.freshNamedVarPair "xcyc"
    addExpr $ ASSIGN c1v $VAR c1
    addExpr $ ASSIGN d1v $VAR d1
    addExpr $ ASSIGN c2v $ VAR c2 
    addExpr $ ASSIGN d2v $VAR d2
    addExpr $ ASSIGN xcv $VAR  xc
    addExpr $ ASSIGN ycv $VAR yc


    let quadA = OP "+" (SQR (OP "-" (VAR c2) (VAR c1))) (SQR (OP "-" (VAR d2) (VAR d1)))
    let quadB = OP "*" (CONST 2) (OP "+" (OP "*" (OP "-" (VAR c2) (VAR c1)) (OP "-" (VAR c1) (VAR xc)))
                                         (OP "*" (OP "-" (VAR d2) (VAR d1)) (OP "-" (VAR d1) (VAR yc))))
    let quadC = OP "-" (OP "+" (SQR (OP "-" (VAR c1) (VAR xc))) (SQR (OP "-" (VAR d1) (VAR yc)))) r2
    (_, crossProdV) <- State.freshNamedVarPair "fold5Diagnostics"
    let discExpr = OP "-" (SQR quadB) (OP "*" (OP "*" (CONST 4) quadA) quadC)
    --addExpr $ OP "=" (SQR (VAR desc)) descExpr
    --addExpr $ OP ">" (VAR desc) (CONST 0)
    disc <- getConstructSqrt discExpr
    let sol1 = OP "/" (OP "+" (OP "-" (CONST 0) quadB) (VAR disc))
                      (OP "*" (CONST 2) quadA)
    let sol2 = OP "/" (OP "-" (OP "-" (CONST 0) quadB) (VAR disc))
                      (OP "*" (CONST 2) quadA)
    (s1, s2) <- State.freshNamedVarPair "fold5Sols"
    addExpr $ ASSIGN s1 sol1
    addExpr $ ASSIGN s2 sol2
    let sol1x = (OP "+" (VAR c1) (OP "*" (VAR s1) (OP "-" (VAR c2) (VAR c1))))
    let sol1y = (OP "+" (VAR d1) (OP "*" (VAR s1) (OP "-" (VAR d2) (VAR d1))))
    let sol2x = (OP "+" (VAR c1) (OP "*" (VAR s2) (OP "-" (VAR c2) (VAR c1))))
    let sol2y = (OP "+" (VAR d1) (OP "*" (VAR s2) (OP "-" (VAR d2) (VAR d1))))

    (discV, _) <- State.freshNamedVarPair "disc_diag"
    (qa, qb) <- State.freshNamedVarPair "quadratic_AB"
    (qc, _) <- State.freshNamedVarPair "quadratic_c"
    
    addExpr $ ASSIGN qa quadA
    addExpr $ ASSIGN qb quadB
    addExpr $ ASSIGN qc quadC

    (sol1xV, sol1yV) <- State.freshNamedVarPair "sol1_diag"
    (sol2xV, sol2yV) <- State.freshNamedVarPair "sol2_diag"
 
    addExpr $ ASSIGN discV discExpr
    addExpr $ ASSIGN sol1xV sol1x
    addExpr $ ASSIGN sol1yV sol1y

    addExpr $ ASSIGN sol2xV sol2x
    addExpr $ ASSIGN sol2yV sol2y

    let wx = OP "-" sol1x (VAR a)
    let wy = OP "-" sol1y (VAR b)
    let vx = OP "-" sol2x (VAR a)
    let vy = OP "-" sol2y (VAR b)

    let sol1x' = OP "+" (VAR xc) (OP "*" (CONST (-1)) wy)
    let sol1y' = OP "+" (VAR yc) (wx)
    let sol2x' = OP "+" (VAR xc) (OP "*" (CONST (-1)) vy)
    let sol2y' = OP "+" (VAR yc) (vx)

    let crossProd = crossProdExpr (vx, vy) (wx, wy)

    addExpr $ ASSIGN crossProdV crossProd

    let sol1Expr = ASSIGNS [(x2, sol1x'), (y2, sol1y')]
    let sol2Expr = ASSIGNS [(x2, sol2x'), (y2, sol2y')]

    let firstStr = if flag then ">=" else "<="
    let secondStr = if flag then "<=" else ">="

    let sol1Constrained = OP "and" (OP firstStr (VAR crossProdV) (CONST 0)) sol1Expr
    let sol2Constrained = OP "and" (OP secondStr (VAR crossProdV) (CONST 0)) sol2Expr



    addExpr $ ASSIGNS  [(x1, (VAR xc)), (y1, (VAR yc))]
    let totalExpr = OP "or" sol1Constrained sol2Constrained
    addExpr totalExpr

{-addNFold5Decl :: Parser.Identifier
              -> Parser.Identifier
              -> Parser.Identifier
              -> Parser.Identifier
              -> TransformState ()
addNFold5Decl var pointMove pointOnLine line = do
    (x1, y1, x2, y2) <- State.addLine var
    (xc, yc) <- State.getPointVars pointOnLine
    (a, b) <- State.getPointVars pointMove
    (c1, d1, c2, d2) <- State.getLineVars line
    let r2 = distance (xc, yc) (a, b)
    let quadA = OP "+" (SQR (OP "-" (VAR c2) (VAR c1))) (SQR (OP "-" (VAR d2) (VAR d1)))
    let quadB = OP "+" (OP "*" (CONST 2) (OP "*" (OP "-" (VAR c2) (VAR c1))
                                                 (OP "-" (VAR c1) (VAR c2))))
                       (OP "*" (CONST 2) (OP "*" (OP "-" (VAR d2) (VAR d1))
                                                 (OP "-" (VAR d1) (VAR d2))))
    let quadC = OP "-" (OP "-" (OP "+" (OP "+" (OP "+" (SQR (VAR d1)) (SQR (VAR c1)))
                                               (SQR (VAR xc)))
                                       (SQR (VAR yc)))
                               (OP "*" (CONST 2) (OP "+" (OP "*" (VAR xc) (VAR c1))
                                                         (OP "*" (VAR yc) (VAR d1)))))
                       r2
    (desc, f2) <- State.freshVarPair
    let descExpr = OP "-" (SQR quadB) (OP "*" (OP "*" (CONST 2) quadA) quadC)
    addExpr $ OP "=" (SQR (VAR desc)) descExpr
    let sol1 = OP "/" (OP "+" (OP "-" (CONST 0) quadB) (VAR desc))
                      (OP "*" (CONST 2) quadA)
    let sol2 = OP "/" (OP "-" (OP "-" (CONST 0) quadB) (VAR desc))
                      (OP "*" (CONST 2) quadA)
    let sol1x = midPoint (VAR a) (OP "+" (VAR c1) (OP "*" sol1 (OP "-" (VAR c2) (VAR c1))))
    let sol1y = midPoint (VAR b) (OP "+" (VAR d1) (OP "*" sol1 (OP "-" (VAR d2) (VAR d1))))
    let sol2x = midPoint (VAR a) (OP "+" (VAR c1) (OP "*" sol2 (OP "-" (VAR c2) (VAR c1))))
    let sol2y = midPoint (VAR b) (OP "+" (VAR d1) (OP "*" sol2 (OP "-" (VAR d2) (VAR d1))))

    let sol1Expr = OP "and" (OP "=" (VAR x2) sol1x) (OP "=" (VAR y2) sol1y)
    let sol2Expr = OP "and" (OP "=" (VAR x2) sol2x) (OP "=" (VAR y2) sol2y)
    let centerExpr = OP "and" (OP "=" (VAR x1) (VAR xc)) (OP "=" (VAR y1) (VAR yc))
    let totalExpr = OP "and" centerExpr (OP "or" sol1Expr sol2Expr)
    addExpr totalExpr
-}
addFold6Decl :: Int
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> TransformState ()
addFold6Decl solNum var p1 l1 p2 l2 = do
    (sol1, sol2, sol3) <- fold6Find3Solutions solNum p1 l1 p2 l2
    case solNum of
        1 -> assignSolution sol1 var
        2 -> assignSolution sol2 var
        3 -> assignSolution sol3 var
        _ -> error "invalid number of solutions in 3solution fold6 parse"

assignSolution :: (Expr, Expr, Expr, Expr) -> Parser.Identifier -> TransformState ()
assignSolution (a1, b1, a2, b2) var = do
    (x1, y1, x2, y2) <- State.addLine var
    addExpr $ ASSIGN x1 a1
    addExpr $ ASSIGN y1 b1
    addExpr $ ASSIGN x2 a2
    addExpr $ ASSIGN y2 b2

conditionalAddExpr :: Bool -> State.Variable -> State.Variable  -> TransformState ()
conditionalAddExpr True v1 v2 = addExpr $ OP "<" (VAR v2) (VAR v1)
conditionalAddExpr False _ _ = return ()

{- Credit for the equations here goes to Robert Lang's ReferenceFinder -}

fold6Find3Solutions :: Int
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> TransformState ((Expr, Expr, Expr, Expr), (Expr, Expr, Expr, Expr), (Expr, Expr, Expr, Expr))
fold6Find3Solutions solNum p1 l1 p2 l2 = do
    compareVector <- fold6GetCompareVector p1 l1
    (t1, t2, t3) <- fold6FindRoots p1 l1 p2 l2 compareVector
    s1 <- findSolnForRoot t1 p1 l1 p2 l2 compareVector
    s2 <- findSolnForRoot t2 p1 l1 p2 l2 compareVector
    s3 <- findSolnForRoot t3 p1 l1 p2 l2 compareVector

    conditionalAddExpr (solNum >= 2) t1 t2
    conditionalAddExpr (solNum == 3) t2 t3
    return (s1, s2, s3)

{-
fold6Find2Solutions :: Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> TransformState ((Expr, Expr, Expr, Expr), (Expr, Expr, Expr, Expr))
fold6Find2Solutions p1 l1 p2 l2 = do
    (t1, t2, t3) <- fold6FindRoots p1 l1 p2 l2
    s1 <- findSolnForRoot t1 p1 l1 p2 l2
    s2 <- findSolnForRoot t2 p1 l1 p2 l2
    addExpr $ OP "<" (VAR t2) (VAR t1)
    addExpr $ OP "=" (VAR t3) (VAR t2)
    return (s1, s2)

fold6Find1Solution :: Parser.Identifier
                   -> Parser.Identifier
                   -> Parser.Identifier
                   -> Parser.Identifier
                   -> TransformState ((Expr, Expr, Expr, Expr))
fold6Find1Solution p1 l1 p2 l2 = do
    (t1, t2, t3) <- fold6FindRoots p1 l1 p2 l2
    s <- findSolnForRoot t1 p1 l1 p2 l2
    addExpr $ OP "=" (VAR t1) (VAR t2)
    addExpr $ OP "=" (VAR t2) (VAR t3)
    return s
-}
fold6GetCompareVector :: Parser.Identifier
                      -> Parser.Identifier
                      -> TransformState (Expr, Expr)
fold6GetCompareVector p1 l1 = do
    (xc, yc) <- State.getPointVars p1
    (a1, b1, a2, b2) <- State.getLineVars l1

    (x0, y0) <- State.freshNamedVarPair "basept_parabola"
    let (xNum, yNum, denom) = assignIntersectPtData
                                 (VAR a1, VAR b1, VAR a2, VAR b2)
                                 (VAR xc, VAR yc, OP "+" (VAR xc) (OP "-" (VAR b1) (VAR b2)),
                                                  OP "+" (VAR yc) (OP "-" (VAR a2) (VAR a1)))
    addExpr $ ASSIGN x0 (OP "/" xNum denom)
    addExpr $ ASSIGN y0 (OP "/" yNum denom)
    let crossProd = crossProdExpr (OP "-" (VAR a2) (VAR a1), OP "-" (VAR b2) (VAR b1))
                                  (OP "-" (VAR xc) (VAR x0), OP "-" (VAR yc) (VAR y0))
    (vx, vy) <- State.freshVarPair
    addExpr $ OP "or" (OP "and" (OP ">" crossProd (CONST 0)) (ASSIGN vx (OP "-" (VAR a2) (VAR a1))))
                      (OP "and" (OP "<" crossProd (CONST 0)) (ASSIGN vx (OP "-" (VAR a1) (VAR a2))))

    addExpr $ OP "or" (OP "and" (OP ">" crossProd (CONST 0)) (ASSIGN vy (OP "-" (VAR b2) (VAR b1))))
                      (OP "and" (OP "<" crossProd (CONST 0)) (ASSIGN vy (OP "-" (VAR b1) (VAR b2))))
    norm <- getConstructSqrt (OP "+" (SQR (VAR vx)) (SQR (VAR vy)))

    return (OP "/" (VAR vx) (VAR norm), OP "/" (VAR vy) (VAR norm))


findSolnForRoot :: State.Variable
                -> Parser.Identifier
                -> Parser.Identifier
                -> Parser.Identifier
                -> Parser.Identifier
                -> (Expr, Expr) -- result of fold6GetCompareVector
                -> TransformState (Expr, Expr, Expr, Expr)
findSolnForRoot t p1 l1 p2 l2 (u1px, u1py)= do
    (x1, y1) <- State.getPointVars p1
    (a1, b1, a2, b2) <- State.getLineVars l1
    --(u1px, u1py) <- fold6GetCompareVector p1 l1
    let u1y = OP "-" (CONST 0) u1px
    let u1x = u1py
    let d1 = dot (u1x, u1y) (VAR a1, VAR b1)
    (x2, y2) <- State.getPointVars p2
    (a3, b3, a4, b4) <- State.getLineVars l2
  {-  let u2px = OP "-" (VAR a4) (VAR a3)
    let u2py = OP "-" (VAR b4) (VAR b3)
    let u2x = OP "-" (CONST 0) u2py
    let u2y = u2px -}
    (u2x, u2y, u2px, u2py) <- getNormedPerpVector l2
    let d2 = dot (u2x, u2y) (VAR a3, VAR b3)
    let p1px = OP "+" (OP "*" d1 u1x) (OP "*" (VAR t) u1px)
    let p1py = OP "+" (OP "*" d1 u1y) (OP "*" (VAR t) u1py)
    mpx <- midPoint p1px (VAR x1)
    mpy <- midPoint p1py (VAR y1)
    let vx = OP "-" p1px (VAR x1)
    let vy = OP "-" p1py (VAR y1)
    let vx' = OP "*" (CONST (-1)) vy
    let vy' = vx
    let mpx' = OP "+" mpx vx'
    let mpy' = OP "+" mpy vy'
    return (mpx, mpy, mpx', mpy')
    
fold6FindRoots :: Parser.Identifier
               -> Parser.Identifier
               -> Parser.Identifier
               -> Parser.Identifier
               -> (Expr, Expr)
               -> TransformState (State.Variable, State.Variable, State.Variable)
fold6FindRoots p1 l1 p2 l2 u1p = do
    coeffs <- fold6DeclGetCoeffs p1 l1 p2 l2 u1p
    roots <- getDistinctRootCount coeffs
    (t1, t2) <- State.freshNamedVarPair "root"
    (t3, _)  <- State.freshNamedVarPair "root"
    (t1', t2') <- State.freshNamedVarPair "root_rationalized"
    (t3', _) <- State.freshNamedVarPair "root_rationalized"
    -- roots has already been assigned, these are equality checks
    --addExpr $ OP "and" (OP ">=" (VAR roots) (CONST 3)) (OP "<" (VAR t3) (VAR t2))
    --addExpr $ OP "and" (OP ">=" (VAR roots) (CONST 2)) (OP "<" (VAR t2) (VAR t1))
    let distinctCond = LIST "or" [OP "and" (OP "=" (VAR roots) (CONST 3))
                                           (OP "and" ( (OP "<" (VAR t2) (VAR t1)))
                                                     ( (OP "<" (VAR t3) (VAR t2)))),
                                  OP "and" (OP "=" (VAR roots) (CONST 2)) ( (OP "<" (VAR t2) (VAR t1))),
                                  OP "=" (VAR roots) (CONST 1)]
    addExpr $ distinctCond
    addExpr $ findRootExpr coeffs t1
    addExpr $ findRootExpr coeffs t2
    addExpr $ findRootExpr coeffs t3
    --addExpr $ OP "<=" (SQR (OP "-" (VAR t1) (VAR t1'))) errorTerm
    --addExpr $ OP "<=" (SQR (OP "-" (VAR t2) (VAR t2'))) errorTerm
    --addExpr $ OP "<=" (SQR (OP "-" (VAR t3) (VAR t3'))) errorTerm
    return (t1, t2, t3)
    
findRootExpr :: (Expr, Expr, Expr, Expr) -> State.Variable -> Expr
findRootExpr (a, b, c, d) t = 
    OP ">=" errorTerm $ SQR $
        LIST "+" [LIST "*" [a, VAR t, VAR t, VAR t],
                  LIST "*" [b, VAR t, VAR t], 
                  LIST "*" [c, VAR t],
                  d]

getDistinctRootCount :: (Expr, Expr, Expr, Expr) -> TransformState (State.Variable)
getDistinctRootCount (a,b,c,d) = do
    (roots, _) <- State.freshNamedVarPair "rootCnt"
    let t3desc = OP "-" (OP "-" (OP "-" (LIST "*" [CONST 18, a, b, c, d]) (LIST "*" [CONST 4, b, b, b, d]))
                                (LIST "*" [CONST 4, a, c, c, c]))
                        (LIST "*" [CONST 27, a, a, d, d])
    let t2desc = OP "-" (SQR c) (LIST "*" [CONST 4, b, d])
    
    (t3descV, t2descV) <- State.freshNamedVarPair "discriminants"
    addExpr $ ASSIGN t3descV t3desc
    addExpr $ ASSIGN t2descV t2desc
    (diag, _) <-State.freshNamedVarPair "foobar"

    let t3cond' = LIST "and" [OP ">" (VAR t3descV) (CONST 0), NEG (OP "=" (CONST 0) a), ASSIGN roots (CONST 3)]
    let t3cond'' = LIST "and" [OP "=" (VAR t3descV) (CONST 0), NEG (OP "=" (CONST 0) a), ASSIGN roots (CONST 2)]
    let t3cond''' = LIST "and" [OP "<" (VAR t3descV) (CONST 0), NEG (OP "=" (CONST 0) a), ASSIGN roots (CONST 1)] 
    let t3cond = LIST "or" [t3cond', t3cond'', t3cond''']

    let t2cond' = LIST "and" [OP ">" (VAR t2descV) (CONST 0), NEG (OP "=" (CONST 0) b), ASSIGN roots (CONST 2)]
    let t2cond'' = LIST "and" [OP "=" (VAR t2descV) (CONST 0), NEG (OP "=" (CONST 0) b), ASSIGN roots (CONST 1)]
    let t2cond''' = LIST "and" [OP "<" (VAR t2descV) (CONST 0), NEG (OP "=" (CONST 0) b), ASSIGN roots (CONST 0)]
    let t2cond = LIST "or" [t2cond', t2cond'', t2cond''']

    let t1cond = LIST "and" [NEG (OP "=" (CONST 0) c), ASSIGN roots (CONST 1)]
 
    let rootsCond = LIST "or" [t3cond, OP "and" (OP "=" (CONST 0) a) t2cond, LIST "and" [OP "=" (CONST 0) a, OP "=" (CONST 0) b, t1cond]]

    addExpr rootsCond
    return roots

getNormedPerpVector :: Parser.Identifier
                    -> TransformState (Expr, Expr, Expr, Expr)
getNormedPerpVector l = do
    (a1, b1, a2, b2) <- State.getLineVars l
    let u1px' = OP "-" (VAR a2) (VAR a1)
    let u1py' = OP "-" (VAR b2) (VAR b1)
    let u1x' = OP "-" (CONST 0) u1py'
    let u1y' = u1px'
    --(norm1, _) <- State.freshVarPair
    norm1 <- getConstructSqrt (OP "+" (SQR u1px') (SQR u1py'))
    --addExpr $ OP "=" (SQR (VAR norm1)) (OP "+" (SQR u1px') (SQR u1py'))
    --addExpr $ OP ">" (VAR norm1) (CONST 0)
    let u1x = OP "/" u1x' (VAR norm1)
    let u1y = OP "/" u1y' (VAR norm1)
    let u1px = OP "/" u1px' (VAR norm1)
    let u1py = OP "/" u1py' (VAR norm1)
    return (u1x, u1y, u1px, u1py)

fold6DeclGetCoeffs :: Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> Parser.Identifier
                    -> (Expr, Expr)
                    -> TransformState (Expr, Expr, Expr, Expr)
fold6DeclGetCoeffs p1 l1 p2 l2 (u1px, u1py) = do
    (x1, y1) <- State.getPointVars p1
    (a1, b1, a2, b2) <- State.getLineVars l1
    --(u1x, u1y, _, _) <- getNormedPerpVector l1
    let u1x = u1py
    let u1y = OP "*" (CONST (-1)) u1px
    let d1 = dot (u1x, u1y) (VAR a1, VAR b1)
    (x2, y2) <- State.getPointVars p2
    (u2x, u2y, u2px, u2py) <- getNormedPerpVector l2


    (a3, b3, a4, b4) <- State.getLineVars l2
  {-  let u2px' = OP "-" (VAR a4) (VAR a3)
    let u2py' = OP "-" (VAR b4) (VAR b3)
    let u2x' = OP "-" (CONST 0) u2py'
    let u2y' = u2px'
    addExpr $ OP "=" (SQR (VAR norm2)) (OP "+" (SQR u2px') (SQR u2py'))
    addExpr $ OP ">" (VAR norm2) (CONST 0)
    let u2x = OP "/" u2x' (VAR norm2)
    let u2y = OP "/" u2y' (VAR norm2)
    let u2px = OP "/" u2px' (VAR norm2)
    let u2py = OP "/" u2py' (VAR norm2)
-}
    let d2 = dot (u2x, u2y) (VAR a3, VAR b3)
    let v1x' = OP "+" (VAR x1) (OP "-" (OP "*" d1 u1x) (OP "*" (CONST 2) (VAR x2)))
    let v1y' = OP "+" (VAR y1) (OP "-" (OP "*" d1 u1y) (OP "*" (CONST 2) (VAR y2)))
    let v2x' = OP "-" (OP "*" d1 u1x) (VAR x1)
    let v2y' = OP "-" (OP "*" d1 u1y) (VAR y1)
    (v1xV, v1yV) <- State.freshNamedVarPair "v1"
    (v2xV, v2yV) <- State.freshNamedVarPair "v2"
    let v1x = VAR v1xV
    let v1y = VAR v1yV
    let v2x = VAR v2xV
    let v2y = VAR v2yV
    addExpr $ OP "=" v1x v1x'
    addExpr $ OP "=" v1y v1y'
    addExpr $ OP "=" v2x v2x'
    addExpr $ OP "=" v2y v2y'
    (u2xV, u2yV) <- State.freshNamedVarPair "getcoeff_u2"
    (u1xV, u1yV) <- State.freshNamedVarPair "getcoeff_u1"
    (u1pxV, u1pyV) <- State.freshNamedVarPair "getcoeff_u1p"
    addExpr $ ASSIGN u1xV u1x
    addExpr $ ASSIGN u1yV u1y
    addExpr $ ASSIGN u2xV u2x
    addExpr $ ASSIGN u2yV u2y
    addExpr $ ASSIGN u1pxV u1px
    addExpr $ ASSIGN u1pyV u1py

    let c1 = OP "-" (dot (VAR x2, VAR y2) (u2x, u2y)) d2
    let c2 = OP "*" (CONST 2) (dot (v2x, v2y) (u1px, u1py))
    let c3 = dot (v2x, v2y) (v2x, v2y)
    let c4 = dot (OP "+" v1x v2x, OP "+" v1y v2y) (u1px, u1py)
    let c5 = dot (v1x, v1y) (v2x, v2y)
    let c6 = dot (u1px, u1py) (u2x, u2y)
    let c7 = dot (v2x, v2y) (u2x, u2y)

    (c1V, c2V) <- State.freshNamedVarPair "ci_12"
    (c3V, c4V) <- State.freshNamedVarPair "ci_34"
    (c5V, c6V) <- State.freshNamedVarPair "ci_56"
    (c7V, _) <- State.freshNamedVarPair "ci_7"
    addExpr $ ASSIGN c1V c1
    addExpr $ ASSIGN c2V c2
    addExpr $ ASSIGN c3V c3
    addExpr $ ASSIGN c4V c4
    addExpr $ ASSIGN c5V c5
    addExpr $ ASSIGN c6V c6
    addExpr $ ASSIGN c7V c7
    
    let a = (VAR c6V)
    let b = OP "+" (VAR c1V) $ OP "+" (OP "*" (VAR c4V) (VAR c6V)) (VAR c7V)
    let c = OP "+" (OP "*" (VAR c1V) (VAR c2V)) (OP "+" (OP "*" (VAR c5V) (VAR c6V)) (OP "*" (VAR c4V) (VAR c7V)))
    let d = OP "+" (OP "*" (VAR c1V) (VAR c3V)) (OP "*" (VAR c5V) (VAR c7V))
    (aV, bV) <- State.freshNamedVarPair "coeff_ab"
    (cV, dV) <- State.freshNamedVarPair "coeff_cd"
    addExpr $ ASSIGN aV a
    addExpr $ ASSIGN bV b
    addExpr $ ASSIGN cV c
    addExpr $ ASSIGN dV d
    return (VAR aV, VAR bV, VAR cV, VAR dV)

dot :: (Expr, Expr) -> (Expr, Expr) -> Expr
dot (x1, y1) (x2, y2) = OP "+" (OP "*" x1 x2) (OP "*" y1 y2)

addFold7Decl :: Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> TransformState ()
addFold7Decl var p l1 l2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (x,y) <- State.getPointVars p
    (a1, b1, a2, b2) <- State.getLineVars l1
    (c1, d1, c2, d2) <- State.getLineVars l2
    (tempx, tempy) <- State.freshVarPair
    (vx, vy) <- State.freshVarPair
    addExpr $ ASSIGN vx (OP "-" (VAR a2) (VAR a1))
    addExpr $ ASSIGN vy (OP "-" (VAR b2) (VAR b1))
    let newX = OP "+" (VAR x) (VAR vx)
    let newY = OP "+" (VAR y) (VAR vy)
    let (pl2x, pl2y, denom) = assignIntersectPtData (VAR x, VAR y, newX, newY) (VAR c1, VAR d1, VAR c2, VAR d2)
    addExpr $ ASSIGN tempx pl2x
    addExpr $ ASSIGN tempy pl2y
    let mpx = getAvg (VAR tempx) (VAR x)
    let mpy = getAvg (VAR tempy) (VAR y)
    addExpr $ ASSIGN x1 mpx
    addExpr $ ASSIGN y1 mpy
    let dxl1 = VAR vx
    let dyl1 = VAR vy
    let dx' = dyl1
    let dy' = OP "-" (CONST 0) dxl1
    addExpr $ ASSIGN x2 (OP "+" mpx dx')
    addExpr $ ASSIGN y2 (OP "+" mpy dy')

errorTerm :: Expr
errorTerm = OP "/" (CONST 0) (CONST 1)--4000000000485760000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)--922337203685477580700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

getConstructSqrt :: Expr -> TransformState (Variable)
getConstructSqrt e = do
    var <- State.freshNamedVar "sqrt"
    addExpr $ OP "<=" (SQR (OP "-" e (SQR (VAR var)))) errorTerm
    addExpr $ OP ">=" (VAR var) (CONST 0)
    return var

getAvg :: Expr -> Expr -> Expr
getAvg a b = OP "/" (OP "+" a b) (CONST 2)

midPoint :: Expr -> Expr -> TransformState (Expr)
midPoint a b = do
    return $ OP "/" (OP "+" a b) (CONST 2)

addIntersectDecl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addIntersectDecl var arg1 arg2 = do
    (newx, newy) <- State.addPoint var
    (x1, y1, x2, y2) <- State.getLineVars arg1
    (x3, y3, x4, y4) <- State.getLineVars arg2
    let (xNum, yNum, denom) = assignIntersectPtData (VAR x1, VAR y1, VAR x2, VAR y2) (VAR x3, VAR y3, VAR x4, VAR y4)
    addExpr $ ASSIGN newx (OP "/" xNum denom)
    addExpr $ ASSIGN newy (OP "/" yNum denom)
    addExpr $ pointInBox (newx, newy)
    
--taken from wikipedia line-line intersection page
getIntersectPtExprs :: (State.Variable, State.Variable)
                    -> (Expr, Expr, Expr, Expr)
                    -> (Expr, Expr, Expr, Expr)
                    -> (Expr, Expr)
getIntersectPtExprs (newx, newy) (x1, y1, x2, y2) (x3, y3, x4, y4) = (xconstr, yconstr) where
    denom = OP "-" (OP "*" (OP "-" x1 x2) (OP "-" y3 y4))
                   (OP "*" (OP "-" y1 y2) (OP "-" x3 x4))
    part1 = OP "-" (OP "*" x1 y2) (OP "*" y1 x2)
    part2 = OP "-" (OP "*" x3 y4) (OP "*" x4 y3)
    xconstr = OP "=" (OP "*" (VAR newx) denom)
                     (OP "-" (OP "*" part1 (OP "-" x3 x4))
                             (OP "*" (OP "-" x1 x2) part2))
    yconstr = OP "=" (OP "*" (VAR newy) denom)
                     (OP "-" (OP "*" part1 (OP "-" y3 y4))
                             (OP "*" (OP "-" y1 y2) part2))

getIntersectPt :: (State.Variable, State.Variable)
               -> (State.Variable, State.Variable, State.Variable, State.Variable)
               -> (State.Variable, State.Variable, State.Variable, State.Variable)
               -> (Expr, Expr)
getIntersectPt v (x1, y1, x2, y2) (x3, y3, x4, y4) =
    getIntersectPtExprs v (VAR x1, VAR y1, VAR x2, VAR y2) (VAR x3, VAR y3, VAR x4, VAR y4)

pointInBox :: (State.Variable, State.Variable) -> Expr
pointInBox (x, y) = OP "and" (OP "and" (OP ">=" (VAR x) (CONST 0))
                                       (OP "<=" (VAR x) (CONST State.paperSize)))
                             (OP "and" (OP ">=" (VAR y) (CONST 0))
                                       (OP "<=" (VAR y) (CONST State.paperSize)))

addConstraint :: (Expr -> TransformState ()) -> Parser.Constraint -> TransformState ()
addConstraint logExpr constraint = getConstraint constraint>>= logExpr

getConstraint :: Parser.Constraint -> TransformState Expr
getConstraint (Parser.CN_PARALLEL var1 var2) = do
    (x1, y1, x2, y2) <- State.getLineVars var1
    (a1, b1, a2, b2) <- State.getLineVars var2
    return $ getParallelConstr(x1, y1, x2, y2) (a1, b1, a2, b2)
getConstraint (Parser.CN_PERPENDICULAR var1 var2) = do
    l1 <- State.getLineVars var1
    l2 <- State.getLineVars var2
    return $ getPerpConstr l1 l2
getConstraint (Parser.CN_COLINEAR varp varl) = do
    p <- State.getPointVars varp
    l <- State.getLineVars varl
    return $ getColinearExpr p l
getConstraint (Parser.CN_AND c1 c2) = do
    e1 <- getConstraint c1
    e2 <- getConstraint c2
    return $ OP "and" e1 e2
getConstraint (Parser.CN_OR c1 c2) = do
    e1 <- getConstraint c1
    e2 <- getConstraint c2
    return $ OP "or" e1 e2
getConstraint (Parser.CN_NEG c) = do
    e <- getConstraint c
    return $ NEG e

getConstraint _ = error "TODO constraint unimplemented"

getParallelConstr :: (State.Variable, State.Variable, State.Variable, State.Variable)
                  -> (State.Variable, State.Variable, State.Variable, State.Variable)
                  -> Expr
getParallelConstr (x1, y1, x2, y2) (a1, b1, a2, b2) = 
    OP "=" (CONST 0)
           (OP "-" (OP "*" (OP "-" (VAR x2) (VAR x1))
                           (OP "-" (VAR b2) (VAR b1)))
                   (OP "*" (OP "-" (VAR y2) (VAR y1))
                           (OP "-" (VAR a2) (VAR a1))))

getPerpConstr (x1, y1, x2, y2) (a1, b1, a2, b2) =  --yb=-xa
    OP "=" (OP "*" (OP "-" (VAR y2) (VAR y1))
                   (OP "-" (VAR b2) (VAR b1)))
           (OP "*" (OP "*" (OP "-" (CONST 0) (CONST 1))
                           (OP "-" (VAR x2) (VAR x1)))
                   (OP "-" (VAR a2) (VAR a1)))

getColinearExpr :: (State.Variable, State.Variable)
                -> (State.Variable, State.Variable, State.Variable, State.Variable)
                -> Expr
getColinearExpr (a, b) (x1, y1, x2, y2) =
    OP "="(OP "*" (OP "-" (VAR x2) (VAR x1)) (OP "-" (VAR b) (VAR y1)))
          (OP "*" (OP "-" (VAR y2) (VAR y1)) (OP "-" (VAR a) (VAR x1)))

crossProdExpr :: (Expr, Expr) -> (Expr, Expr) -> Expr
crossProdExpr (vx, vy) (wx, wy) = OP "-" (OP "*" vx wy) (OP "*" vy wx)

addExpr :: Expr -> TransformState ()
addExpr e = do
    str <- State.resolveExpr e
    State.addClause str

addConstraintExpr :: Expr -> TransformState ()
addConstraintExpr e = do
    str <- State.resolveExpr e
    State.addConstraintClause str
