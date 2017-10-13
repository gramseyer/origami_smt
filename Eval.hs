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

data Expr = OP String Expr Expr
          | VAR String
          | SQR Expr
          | CONST Int
          | NEG Expr

translateExpr :: Expr -> String
translateExpr (VAR v)        = v
translateExpr (OP str e1 e2) = "(" ++ str ++ " " ++ translateExpr e1 ++ " " ++ translateExpr e2 ++ ")"
translateExpr (SQR expr)     = "(* " ++ translateExpr expr ++ " " ++  translateExpr expr ++ " )"
translateExpr (CONST x)      = if x >= 0 then show x else "(- " ++ show (abs x) ++ ")"
translateExpr (NEG expr)     = "(not " ++ translateExpr expr ++ ")"

distance :: (State.Variable, State.Variable)
         -> (State.Variable, State.Variable)
         -> Expr
distance (x1, y1) (x2, y2) = OP "+" (SQR (OP "-" (VAR x1) (VAR x2))) (SQR (OP "-" (VAR y1) (VAR y2)))

        -- v1x, v1y, v2x, v2y
dotprod :: Expr -> Expr -> Expr -> Expr -> Expr
dotprod v1x v1y v2x v2y =
    OP "+" (OP "*" v1x v2x) (OP "*" v1y v2y)

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
addDecl (Parser.DEC_NFOLD3 var arg1 arg2)     = addNFold3Decl var arg1 arg2
addDecl (Parser.DEC_FOLD4 var arg1 arg2)      = addFold4Decl var arg1 arg2
addDecl (Parser.DEC_FOLD5_SOL1 var pointMove pointCenter line)
                                              = addFold5DeclSol1 var pointMove pointCenter line
addDecl (Parser.DEC_FOLD5_SOL2 var pointMove pointCenter line)
                                              = addFold5DeclSol2 var pointMove pointCenter line
addDecl (Parser.DEC_NFOLD5 var pointMove pointCenter line)
                                              = addNFold5Decl var pointMove pointCenter line
addDecl (Parser.DEC_FOLD6 sols solnum var p1 l1 p2 l2)
                                              = addFold6Decl sols solnum var p1 l1 p2 l2
addDecl (Parser.DEC_FOLD7 var point l1 l2)    = addFold7Decl var point l1 l2
addDecl (Parser.DEC_INTERSECT var arg1 arg2)  = addIntersectDecl var arg1 arg2

addFold1Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold1Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    addExpr $ OP "=" (VAR a1) (VAR x1)
    addExpr $ OP "=" (VAR b1) (VAR y1)
    addExpr $ OP "=" (VAR a2) (VAR x2)
    addExpr $ OP "=" (VAR b2) (VAR y2)
    --sanity check
    addExpr $ NEG (OP "and" (OP "=" (VAR x1) (VAR x2))
                            (OP "=" (VAR y1) (VAR y2)))

addFold2Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold2Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    addExpr $ OP "=" (distance (x1, y1) (a1, b1)) (distance (x1, y1) (a2, b2))
    addExpr $ OP "=" (distance (x2, y2) (a1, b1)) (distance (x2, y2) (a2, b2))
    addExpr $ OP ">" (distance (x1, y1) (x2, y2)) (CONST 0)

addFold3Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold3Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1, a2, b2) <- State.getLineVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2
    let parallelConstr = getParallelConstr (a1, b2, a2, b2) (c1, d1, c2, d2)
    let nonParallelConstr = NEG parallelConstr
    let (intX, intY) = getIntersectPt (x1, y1) (a1, b1, a2, b2) (c1, d1, c2, d2)
    let intNorm1x = OP "/" (OP "-" (VAR a2) (VAR a1)) (distance (a1, b1) (a2, b2))
    let intNorm1y = OP "/" (OP "-" (VAR b2) (VAR b1)) (distance (a1, b1) (a2, b2))
    let intNorm2x = OP "/" (OP "-" (VAR c2) (VAR c1)) (distance (c1, d1) (c2, d2))
    let intNorm2y = OP "/" (OP "-" (VAR d2) (VAR d1)) (distance (c1, d1) (c2, d2))
    let mpdX = midPoint intNorm1x intNorm2x
    let mpdY = midPoint intNorm1y intNorm2y
    let mpdX' = midPoint (OP "*" (CONST (-1)) intNorm1x) intNorm1y
    let mpdY' = midPoint (OP "*" (CONST (-1)) intNorm2x) intNorm2y

    let mpX = OP "+" mpdX (VAR x1)
    let mpY = OP "+" mpdY (VAR y1)
    let mpX' = OP "+" mpdX' (VAR x1)
    let mpY' = OP "+" mpdY' (VAR y1)

    let parX1 = midPoint (VAR a1) (VAR c1)
    let parY1 = midPoint (VAR b1) (VAR d1)
    let parX2 = midPoint (VAR a1) (VAR c2)
    let parY2 = midPoint (VAR b1) (VAR d2)
    let parCond = OP "and" parallelConstr
                           (OP "and" (OP "and" (OP "=" (VAR x1) parX1)
                                               (OP "=" (VAR y1) parY1))
                                     (OP "and" (OP "=" (VAR x2) parX2)
                                               (OP "=" (VAR y2) parY2)))
    let crossProd = OP "-" (OP "*" (OP "-" (VAR a2) (VAR a1))
                                   (OP "-" (VAR d2) (VAR d1)))
                           (OP "*" (OP "-" (VAR b2) (VAR b1))
                                   (OP "-" (VAR c2) (VAR c1)))
    let selectConstr = OP "or" (OP "and" (OP "<" (CONST 0) crossProd)
                                         (OP "and" (OP "=" (VAR x2) mpX)
                                                   (OP "=" (VAR y2) mpY)))
                               (OP "and" (OP ">" (CONST 0) crossProd)
                                         (OP "and" (OP "=" (VAR x2) mpX')
                                                   (OP "=" (VAR y2) mpY')))
    let nParCond = OP "and" nonParallelConstr
                            (OP "and" (OP "and" intX intY)
                                      selectConstr)
    let totalCond = OP "or" parCond nParCond
    addExpr totalCond

addNFold3Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addNFold3Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1, a2, b2) <- State.getLineVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2

    let parallelConstr = getParallelConstr (a1, b1, a2, b2) (c1, d1, c2, d2)
    let nonParallelConstr = NEG parallelConstr
    let (intX, intY) = getIntersectPt (x1, y1) (a1, b1, a2, b2) (c1, d1, c2, d2)
    let intNorm1x = OP "/" (OP "-" (VAR a2) (VAR a1)) (distance (a1, b1) (a2, b2))
    let intNorm1y = OP "/" (OP "-" (VAR b2) (VAR b1)) (distance (a1, b1) (a2, b2))
    let intNorm2x = OP "/" (OP "-" (VAR c2) (VAR c1)) (distance (c1, d1) (c2, d2))
    let intNorm2y = OP "/" (OP "-" (VAR d2) (VAR d1)) (distance (c1, d1) (c2, d2))
    let mpdX = midPoint intNorm1x intNorm2x
    let mpdY = midPoint intNorm1y intNorm2y
    let mpdX' = midPoint (OP "*" (CONST (-1)) intNorm1x) intNorm1y
    let mpdY' = midPoint (OP "*" (CONST (-1)) intNorm2x) intNorm2y
    
    let mpX = OP "+" mpdX (VAR x1)
    let mpY = OP "+" mpdY (VAR y1)
    let mpX' = OP "+" mpdX' (VAR x1)
    let mpY' = OP "+" mpdY' (VAR y1)

    let parX1 = midPoint (VAR a1) (VAR c1)
    let parY1 = midPoint (VAR b1) (VAR d1)
    let parX2 = midPoint (VAR a1) (VAR c2)
    let parY2 = midPoint (VAR b1) (VAR d2)

    let parCond = OP "and" parallelConstr
                           (OP "and" (OP "and" (OP "=" (VAR x1) parX1)
                                               (OP "=" (VAR y1) parY1))
                                     (OP "and" (OP "=" (VAR x2) parX2)
                                               (OP "=" (VAR y2) parY2)))
    let nParCond = OP "and" nonParallelConstr
                            (OP "and" (OP "and" intX intY)
                                      (OP "or" (OP "and" (OP "=" (VAR x2) mpX)
                                                         (OP "=" (VAR y2) mpY))
                                               (OP "and" (OP "=" (VAR x2) mpX')
                                                         (OP "=" (VAR y2) mpY'))))
    let totalCond = OP "or" parCond nParCond
    addExpr totalCond

addFold4Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold4Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a, b) <- State.getPointVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2
    let preRotateX = OP "-" (VAR c2) (VAR c1)
    let preRotateY = OP "-" (VAR d2) (VAR d1)
    let postRotateX = preRotateY
    let postRotateY = OP "-" (CONST 0) preRotateX
    let eqAssert = OP "and" (OP "=" (VAR x2) (OP "+" postRotateX (VAR a)))
                            (OP "=" (VAR y2) (OP "+" postRotateY (VAR b)))
    let eqAssert' = OP "and" (OP "=" (VAR x1) (VAR a))
                             (OP "=" (VAR y1) (VAR b))
    addExpr eqAssert
    addExpr eqAssert'    

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

    let wx = OP "-" sol1x (VAR a)
    let wy = OP "-" sol1y (VAR b)
    let vx = OP "-" sol2x (VAR a)
    let vy = OP "-" sol2y (VAR b)

    let crossProd = crossProdExpr (vx, vy) (wx, wy)
    
    let sol1Expr = OP "and" (OP "=" (VAR x2) sol1x) (OP "=" (VAR y2) sol1y)
    let sol2Expr = OP "and" (OP "=" (VAR x2) sol2x) (OP "=" (VAR y2) sol2y)

    let firstStr = if flag then ">=" else "<="
    let secondStr = if flag then "<=" else ">="

    let sol1Constrained = OP "and" (OP firstStr crossProd (CONST 0)) sol1Expr
    let sol2Constrained = OP "and" (OP secondStr crossProd (CONST 0)) sol2Expr

    let centerExpr = OP "and" (OP "=" (VAR x1) (VAR xc)) (OP "=" (VAR y1) (VAR yc))
    let totalExpr = OP "and" centerExpr (OP "or" sol1Constrained sol2Constrained)
    addExpr totalExpr

addNFold5Decl :: Parser.Identifier
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

addFold6Decl :: Int
             -> Int
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> Parser.Identifier
             -> TransformState ()
addFold6Decl 3 solNum var p1 l1 p2 l2 = do
    (sol1, sol2, sol3) <- fold6Decl_get3sol var p1 l1 p2 l2
    case solNum of
        1 -> assignSolution sol1 var
        2 -> assignSolution sol2 var
        3 -> assignSolution sol3 var
        _ -> error "invalid number of solutions in 3solution fold6 parse"
addFold6Decl 2 solNum var p1 l1 p2 l2 = do
    (sol1, sol2) <- fold6Decl_get2sol var p1 l1 p2 l2
    case solNum of
        1 -> assignSolution sol1 var
        2 -> assignSolution sol2 var
        _ -> error "invalid number of solutions in 2solution fold6 parse"
addFold6Decl 1 solNum var p1 l1 p2 l2 = do
    sol1 <- fold6Decl_get1sol var p1 l1 p2 l2
    case solNum of
        1 -> assignSolution sol1 var
        _ -> error "invalid number of solutions in 1solution fold6 parse"

addFold6Decl _ _ _ _ _ _ _ = error "invalid number of solutions to fold6"

assignSolution :: (Expr, Expr, Expr, Expr) -> Parser.Identifier -> TransformState ()
assignSolution (a1, b1, a2, b2) var = do
    (x1, y1, x2, y2) <- State.addLine var
    addExpr $ OP "=" (VAR x1) a1
    addExpr $ OP "=" (VAR y1) b1
    addExpr $ OP "=" (VAR x2) a2
    addExpr $ OP "=" (VAR y2) b2

fold6Decl_get3sol :: Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> TransformState ((Expr, Expr, Expr, Expr), (Expr,Expr, Expr, Expr), (Expr, Expr, Expr, Expr))
fold6Decl_get3sol var p1 l1 p2 l2 = do
    p1exprs <- getParametrizationsForParabola p1 l1
    p2exprs <- getParametrizationsForParabola p2 l2
    (t1, t1') <- State.freshVarPair
    (t2, t2') <- State.freshVarPair
    (t3, t3') <- State.freshVarPair
    let t1exprs = constructParametrizationFunction t1 p1exprs
    let t1'exprs = constructParametrizationFunction t1' p2exprs
    let t2exprs = constructParametrizationFunction t2 p1exprs
    let t2'exprs = constructParametrizationFunction t2' p2exprs
    let t3exprs = constructParametrizationFunction t3 p1exprs
    let t3'exprs = constructParametrizationFunction t3' p2exprs
    requireMatchup t1exprs t1'exprs
    requireMatchup t2exprs t2'exprs
    requireMatchup t3exprs t3'exprs
    addExpr $ OP "<" (VAR t3) (VAR t2)
    addExpr $ OP "<" (VAR t2) (VAR t1)
    return (getSoln t1exprs t1'exprs, getSoln t2exprs t2'exprs, getSoln t3exprs t3'exprs)

fold6Decl_get2sol :: Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> TransformState ((Expr, Expr, Expr, Expr), (Expr,Expr, Expr, Expr))
fold6Decl_get2sol var p1 l1 p2 l2 = do
    p1exprs <- getParametrizationsForParabola p1 l1
    p2exprs <- getParametrizationsForParabola p2 l2
    (t1, t1') <- State.freshVarPair
    (t2, t2') <- State.freshVarPair
    let t1exprs = constructParametrizationFunction t1 p1exprs
    let t1'exprs = constructParametrizationFunction t1' p2exprs
    let t2exprs = constructParametrizationFunction t2 p1exprs
    let t2'exprs = constructParametrizationFunction t2' p2exprs
    requireMatchup t1exprs t1'exprs
    requireMatchup t2exprs t2'exprs
    addExpr $ OP "<" (VAR t2) (VAR t1)
    return (getSoln t1exprs t1'exprs, getSoln t2exprs t2'exprs)

fold6Decl_get1sol :: Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> Parser.Identifier
                  -> TransformState (Expr,Expr, Expr, Expr)
fold6Decl_get1sol var p1 l1 p2 l2 = do
    p1exprs <- getParametrizationsForParabola p1 l1
    p2exprs <- getParametrizationsForParabola p2 l2
    (t1, t1') <- State.freshNamedVarPair "t1"
    let t1exprs = constructParametrizationFunction t1 p1exprs
    let t1'exprs = constructParametrizationFunction t1' p2exprs
    requireMatchup t1exprs t1'exprs
    return (getSoln t1exprs t1'exprs)

getSoln :: (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
getSoln (x1, y1, _, _) (x2, y2, _, _) = (x1, y1, x2, y2)
    

getParametrizationsForParabola :: Parser.Identifier
                               -> Parser.Identifier
                               -> TransformState (Expr, Expr, Expr, Expr, State.Variable)
getParametrizationsForParabola p l = do
    (xc, yc) <- State.getPointVars p
    (a1, b1, a2, b2) <- State.getLineVars l
    (x0, y0) <- State.freshNamedVarPair "basept_parabola"
    let (xConstr, yConstr) = getIntersectPtExprs (x0, y0)
                                 (VAR a1, VAR b1, VAR a2, VAR b2)
                                 (VAR xc, VAR yc, OP "+" (VAR xc) (OP "-" (VAR b1) (VAR b2)),
                                                  OP "+" (VAR yc) (OP "-" (VAR a2) (VAR a1)))
    addExpr xConstr
    addExpr yConstr

    let crossProd = crossProdExpr (OP "-" (VAR a2) (VAR a1), OP "-" (VAR b2) (VAR b1)) (OP "-" (VAR xc) (VAR x0), OP "-" (VAR yc) (VAR y0))
    (vx, vy) <- State.freshVarPair
    addExpr $ OP "or" (OP "and" (OP ">" crossProd (CONST 0)) (OP "=" (VAR vx) (OP "-" (VAR a2) (VAR a1))))
                      (OP "and" (OP "<" crossProd (CONST 0)) (OP "=" (VAR vx) (OP "-" (VAR a1) (VAR a2))))

    addExpr $ OP "or" (OP "and" (OP ">" crossProd (CONST 0)) (OP "=" (VAR vy) (OP "-" (VAR b2) (VAR b1))))
                      (OP "and" (OP "<" crossProd (CONST 0)) (OP "=" (VAR vy) (OP "-" (VAR b1) (VAR b2))))
 
    (norm, z) <- State.freshNamedVarPair "normz"
    addExpr $ OP "=" (SQR (VAR norm)) (OP "+" (SQR (VAR vx)) (SQR (VAR vy)))
    addExpr $ OP "=" (SQR (VAR z)) (OP "+" (SQR (OP "-" (VAR yc) (VAR y0))) (SQR (OP "-" (VAR xc) (VAR x0))))
    addExpr $ OP ">=" (VAR norm) (CONST 0)
    addExpr $ OP ">=" (VAR z) (CONST 0)
    let normalizedVx = OP "/" (VAR vx) (VAR norm)
    let normalizedVy = OP "/" (VAR vy) (VAR norm)

    return $ (VAR x0, VAR y0, normalizedVx, normalizedVy, z)

constructParametrizationFunction :: State.Variable
                                 -> (Expr, Expr, Expr, Expr, State.Variable)
                                 -> (Expr, Expr, Expr, Expr)
constructParametrizationFunction t (x0, y0, normalizedVx, normalizedVy, z) = (px, py, dPx, dPy) where
    sqrTerm = OP "/" (OP "+" (SQR (VAR t)) (SQR (VAR z))) $ OP "*" (CONST 2) (VAR z)
    px = OP "+" x0
               $ OP "-" (OP "*" normalizedVx (VAR t))
                       $ OP "*" normalizedVy sqrTerm
    py = OP "+" y0
               $ OP "+" (OP "*" normalizedVy (VAR t))
                       $ OP "*" normalizedVx sqrTerm

    dPx = OP "-" normalizedVx $ OP "*" normalizedVy $ OP "/" (VAR t) (VAR z)
    dPy = OP "+" normalizedVy $ OP "*" normalizedVx $ OP "/" (VAR t) (VAR z)

requireMatchup :: (Expr, Expr, Expr, Expr) -> (Expr, Expr, Expr, Expr) -> TransformState ()
requireMatchup (p1x, p1y, dP1x, dP1y) (p2x, p2y, dP2x, dP2y) = do
    addExpr $ OP "=" (OP "*" dP1x dP2y) (OP "*" dP1y dP2x)
    addExpr $ OP "=" (OP "*" (OP "-" p1y p2y) dP1x) (OP "*" (OP "-" p1x p2x) dP1y)

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
    let newX = OP "+" (VAR x) (OP "-" (VAR a2) (VAR a1))
    let newY = OP "+" (VAR y) (OP "-" (VAR b2) (VAR b1))
    let (intX, intY) = getIntersectPtExprs (tempx, tempy) (VAR x, VAR y, newX, newY) (VAR c1, VAR d1, VAR c2, VAR d2)
    let mpx = midPoint (VAR tempx) (VAR x)
    let mpy = midPoint (VAR tempy) (VAR y)
    addExpr $ OP "=" mpx (VAR x1)
    addExpr $ OP "=" mpy (VAR y1)
    let dxl1 = OP "-" (VAR a2) (VAR a1)
    let dyl1 = OP "-" (VAR b2) (VAR b1)
    let dx' = dyl1
    let dy' = OP "-" (CONST 0) dxl1
    addExpr $ OP "=" (VAR x2) (OP "+" mpx dx')
    addExpr $ OP "=" (VAR y2) (OP "+" mpy dy')

midPoint :: Expr -> Expr -> Expr
midPoint a b = OP "/" (OP "+" a b) (CONST 2)

addIntersectDecl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addIntersectDecl var arg1 arg2 = do
    (newx, newy) <- State.addPoint var
    (x1, y1, x2, y2) <- State.getLineVars arg1
    (x3, y3, x4, y4) <- State.getLineVars arg2
    let (xconstr, yconstr) = getIntersectPt (newx, newy) (x1, y1, x2, y2) (x3, y3, x4, y4)
    addExpr xconstr
    addExpr yconstr
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
                                       (OP "<=" (VAR x) (CONST 1)))
                             (OP "and" (OP ">=" (VAR y) (CONST 0))
                                       (OP "<=" (VAR y) (CONST 1)))

addConstraint :: (Expr -> TransformState ()) -> Parser.Constraint -> TransformState ()
addConstraint logExpr (Parser.CN_PARALLEL var1 var2) = do
    (x1, y1, x2, y2) <- State.getLineVars var1
    (a1, b1, a2, b2) <- State.getLineVars var2
    logExpr $ getParallelConstr(x1, y1, x2, y2) (a1, b1, a2, b2)
addConstraint logExpr (Parser.CN_PERPENDICULAR var1 var2) = do
    l1 <- State.getLineVars var1
    l2 <- State.getLineVars var2
    logExpr $ getPerpConstr l1 l2
addConstraint logExpr (Parser.CN_COLINEAR varp varl) = do
    p <- State.getPointVars varp
    l <- State.getLineVars varl
    logExpr $ getColinearExpr p l
addConstraint _ _ = error "TODO constraint unimplemented"

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
addExpr = State.addClause.translateExpr

addConstraintExpr :: Expr -> TransformState ()
addConstraintExpr = State.addConstraintClause.translateExpr
