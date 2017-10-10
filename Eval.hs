module Eval (computeTransform) where

import Parser
import State
import qualified Data.Map as Map
import Data.List as List

computeTransform :: Parser.Program -> TransformState ()
computeTransform (Parser.PROGRAM decls constrs) = (executeDecls decls) >> (executeConstraints constrs)

executeDecls :: [Parser.Declaration] -> TransformState ()
executeDecls decls = List.foldr (>>) State.doNothing (List.map addDecl decls)

executeConstraints :: [Parser.Constraint] -> TransformState ()
executeConstraints constrs = List.foldr (>>) State.doNothing (List.map addConstraint constrs)

data Expr = OP String Expr Expr
          | VAR String
          | SQR Expr
          | CONST Int
          | NEG Expr

translateExpr :: Expr -> String
translateExpr (VAR v)        = v
translateExpr (OP str e1 e2) = "(" ++ str ++ " " ++ (translateExpr e1) ++ " " ++ (translateExpr e2) ++ ")"
translateExpr (SQR expr)     = "(*" ++ (translateExpr expr) ++ (translateExpr expr) ++ ")"
translateExpr (CONST x)      = if x>=0 then (show x) else ("(- " ++ (show (abs x)) ++ ")")
translateExpr (NEG expr)     = "(not " ++ (translateExpr expr) ++ ")"

distance :: (State.Variable, State.Variable) -> (State.Variable, State.Variable) -> Expr
distance (x1, y1) (x2, y2) = OP "+" (SQR (OP "-" (VAR x1) (VAR x2))) (SQR (OP "-" (VAR y1) (VAR y2)))

        -- v1x, v1y, v2x, v2y
dotprod :: Expr -> Expr -> Expr -> Expr -> Expr
dotprod v1x v1y v2x v2y =
    OP "+" (OP "*" v1x v2x) (OP "*" v1y v2y)

addDecl :: Parser.Declaration -> TransformState ()
addDecl (Parser.DEC_FOLD1 var arg1 arg2)  = addFold1Decl var arg1 arg2
addDecl (Parser.DEC_FOLD2 var arg1 arg2)  = addFold2Decl var arg1 arg2
addDecl (Parser.DEC_FOLD3 var arg1 arg2)  = addFold3Decl var arg1 arg2
addDecl (Parser.DEC_NFOLD3 var arg1 arg2) = addNFold3Decl var arg1 arg2
addDecl (Parser.DEC_FOLD4 var arg1 arg2)  = addFold4Decl var arg1 arg2
addDecl (Parser.DEC_FOLD5_SOL1 var pointMove pointCenter line)
                                          = addFold5DeclSol1 var pointMove pointCenter line
addDecl (Parser.DEC_FOLD5_SOL2 var pointMove pointCenter line)
                                          = addFold5DeclSol2 var pointMove pointCenter line
addDecl (Parser.DEC_NFOLD5 var pointMove pointCenter line)
                                          = addNFold5Decl var pointMove pointCenter line
addDecl (Parser.DEC_INTERSECT var arg1 arg2) = addIntersectDecl var arg1 arg2
addDecl _ = error "TODO"

addFold1Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold1Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    addExpr $ OP "=" (VAR a1) (VAR x1)
    addExpr $ OP "=" (VAR b1) (VAR y1)
    addExpr $ OP "=" (VAR a2) (VAR x2)
    addExpr $ OP "=" (VAR b2) (VAR y2)

addFold2Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold2Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1) <- State.getPointVars arg1
    (a2, b2) <- State.getPointVars arg2
    addExpr $ (OP "=" (distance (x1, y1) (a1, b1)) (distance (x1, y1) (a2, b2)))
    addExpr $ (OP "=" (distance (x2, y2) (a1, b1)) (distance (x2, y2) (a2, b2)))
    addExpr $ (OP ">" (distance (x1, y1) (x2, y2)) (CONST 0))

addFold3Decl :: Parser.Identifier -> Parser.Identifier -> Parser.Identifier -> TransformState ()
addFold3Decl var arg1 arg2 = do
    (x1, y1, x2, y2) <- State.addLine var
    (a1, b1, a2, b2) <- State.getLineVars arg1
    (c1, d1, c2, d2) <- State.getLineVars arg2
    let parallelConstr = getParallelConstr (a1, b2, a2, b2) (c1, d1, c2, d2)
    let nonParallelConstr = NEG parallelConstr
    let (intX, intY) = getIntersectPt (x1, y1) (a1, b1, a2, b2) (c1, d1, c2, d2)
    let intNorm1x = (OP "/" (OP "-" (VAR a2) (VAR a1)) (distance (a1, b1) (a2, b2)))
    let intNorm1y = (OP "/" (OP "-" (VAR b2) (VAR b1)) (distance (a1, b1) (a2, b2)))
    let intNorm2x = (OP "/" (OP "-" (VAR c2) (VAR c1)) (distance (c1, d1) (c2, d2)))
    let intNorm2y = (OP "/" (OP "-" (VAR d2) (VAR d1)) (distance (c1, d1) (c2, d2)))
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
    let parX2 = midPoint (VAR a2) (VAR c2)
    let parY2 = midPoint (VAR b2) (VAR d2)
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
    let intNorm1x = (OP "/" (OP "-" (VAR a2) (VAR a1)) (distance (a1, b1) (a2, b2)))
    let intNorm1y = (OP "/" (OP "-" (VAR b2) (VAR b1)) (distance (a1, b1) (a2, b2)))
    let intNorm2x = (OP "/" (OP "-" (VAR c2) (VAR c1)) (distance (c1, d1) (c2, d2)))
    let intNorm2y = (OP "/" (OP "-" (VAR d2) (VAR d1)) (distance (c1, d1) (c2, d2)))
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
    let parX2 = midPoint (VAR a2) (VAR c2)
    let parY2 = midPoint (VAR b2) (VAR d2)

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
    let eqAssert = OP "and" (OP "=" (VAR x2) postRotateX)
                            (OP "=" (VAR y2) postRotateY)
    let eqAssert' = OP "and" (OP "=" (VAR x1) (VAR c1))
                             (OP "=" (VAR y1) (VAR d1))
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

{-
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
  -}  



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
getIntersectPt :: (State.Variable, State.Variable)
               -> (State.Variable, State.Variable, State.Variable, State.Variable)
               -> (State.Variable, State.Variable, State.Variable, State.Variable)
               -> (Expr, Expr)
getIntersectPt (newx, newy) (x1, y1, x2, y2) (x3, y3, x4, y4) = (xconstr, yconstr) where
    denom = OP "-" (OP "*" (OP "-" (VAR x1) (VAR x2)) (OP "-" (VAR y3) (VAR y4)))
                   (OP "*" (OP "-" (VAR y1) (VAR y2)) (OP "-" (VAR x3) (VAR x4)))
    part1 = OP "-" (OP "*" (VAR x1) (VAR y2)) (OP "*" (VAR y1) (VAR x2))
    part2 = OP "-" (OP "*" (VAR x3) (VAR y4)) (OP "*" (VAR x4) (VAR y3))
    xconstr = OP "=" (OP "*" (VAR newx) denom)
                     (OP "-" (OP "*" part1 (OP "-" (VAR x3) (VAR x4)))
                             (OP "*" (OP "-" (VAR x1) (VAR x2)) part2))
    yconstr = OP "=" (OP "*" (VAR newy) denom)
                     (OP "-" (OP "*" part1 (OP "-" (VAR y3) (VAR y4)))
                             (OP "*" (OP "-" (VAR y1) (VAR y2)) part2))

pointInBox :: (State.Variable, State.Variable) -> Expr
pointInBox (x, y) = OP "and" (OP "and" (OP ">=" (VAR x) (CONST 0))
                                       (OP "<=" (VAR x) (CONST 1)))
                             (OP "and" (OP ">=" (VAR y) (CONST 0))
                                       (OP "<=" (VAR y) (CONST 1)))

addConstraint :: Parser.Constraint -> TransformState ()
addConstraint (Parser.CN_PARALLEL var1 var2) = do
    (x1, y1, x2, y2) <- State.getLineVars var1
    (a1, b1, a2, b2) <- State.getLineVars var2
    addExpr $ getParallelConstr(x1, y1, x2, y2) (a1, b1, a2, b2)

getParallelConstr :: (State.Variable, State.Variable, State.Variable, State.Variable)
                  -> (State.Variable, State.Variable, State.Variable, State.Variable)
                  -> Expr
getParallelConstr (x1, y1, x2, y2) (a1, b1, a2, b2) = 
    OP "=" (CONST 0)
           (OP "-" (OP "*" (OP "-" (VAR x2) (VAR x1))
                           (OP "-" (VAR b2) (VAR b1)))
                   (OP "*" (OP "-" (VAR y2) (VAR y1))
                           (OP "-" (VAR a2) (VAR a1))))

crossProdExpr :: (Expr, Expr) -> (Expr, Expr) -> Expr
crossProdExpr (vx, vy) (wx, wy) = OP "-" (OP "*" vx wy) (OP "*" vy wx)


addExpr :: Expr -> TransformState ()
addExpr = (State.addClause).translateExpr
