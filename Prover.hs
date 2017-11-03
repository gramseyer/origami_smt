module Prover (runSolvers) where

import Control.Monad
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import qualified Data.List as List
import State

runSolvers :: Bool -> State.Transform -> IO [(Variable, AlgReal)]
runSolvers b t = runSMT $ makeSymbolicCalculation b t

makeSymbolicCalculation :: Bool -> State.Transform -> Symbolic [(Variable, AlgReal)]
makeSymbolicCalculation b t = do
    varDeclMap <- makeVarDecls (State.varNameList t)
  --  consts <- (liftM2 List.map) processConstants (State.constructionClauses t ++ State.assertionClauses t)
  --  let totalMap = Map.union varDeclMap (Map.fromList consts)
    assertConstraints varDeclMap (State.constructionClauses t)
    if b then assertNegatedConstraints varDeclMap (State.assertionClauses t)
         else assertConstraints varDeclMap (State.assertionClauses t)

    result <- query $ do
        cs <- checkSat
        case cs of
            Unk -> do
                 io $ putStrLn $ "smt returned unknown"
                 return []
            Unsat -> do
                io $ putStrLn $ "smt returned unsat"
                return []
            Sat -> do
                io $ putStrLn $ "smt returned sat"
                model <- getModel
                io $ putStrLn $ show model
                return []      
    return result
makeVarDecls :: [Variable] -> Symbolic (Map.Map Variable SReal)
makeVarDecls varnames = liftM Map.fromList $ List.foldr (liftM2 (:)) (return []) (List.map makeVarDecl varnames)
    
makeVarDecl :: Variable -> Symbolic (Variable, SReal)
makeVarDecl varname = do
    sVar <- sReal varname
    return (varname, sVar)
{-
processConstants :: Expr -> Symbolic [(Variable, SReal)]
processConstants (OP _ e1 e2) = (processConstants e1) (liftM2 (++)) (processConstants e2)
processConstants (NEG e) = processConstants e
processConstants (ASSIGNS xs) = (liftM2 List.map) (processConstants.snd)
processConstants (CONST' (x, y)) = do
    let varname = makeConstVar (x, y)
    var <- sReal varname
    constrain $ 0.5 * var .== (fromInteger x)
    return [(varname, var)]
processConstants _ = return [] 
-}
assertConstraints :: Map.Map Variable SReal -> [Clause] -> Symbolic ()
assertConstraints map exprs = List.foldr (>>) (return ()) $
                                  List.map (constrain . makeConstraintBool map) exprs

assertNegatedConstraints :: Map.Map Variable SReal -> [Clause] -> Symbolic ()
assertNegatedConstraints map exprs = constrain.bnot.bAnd $ List.map (makeConstraintBool map) exprs

makeConstVar :: (Integer, Integer) -> String
makeConstVar (x, y) = show x ++ "_div_" ++ show y

makeConstraintReal :: Map.Map Variable SReal -> Expr -> SReal
makeConstraintReal map (VAR v) = map Map.! v
makeConstraintReal map (OP "+" e1 e2) = (makeConstraintReal map e1) + (makeConstraintReal map e2)
makeConstraintReal map (OP "-" e1 e2) = (makeConstraintReal map e1) - (makeConstraintReal map e2)
makeConstraintReal map (OP "*" e1 e2) = (makeConstraintReal map e1) * (makeConstraintReal map e2)
makeConstraintReal map (OP "/" e1 e2) = (makeConstraintReal map e1) / (makeConstraintReal map e2)
makeConstraintReal map (CONST' (x, y)) = fromRational (fromInteger x / fromInteger y)
makeConstraintReal _ e = error $ "invalid real-valued expression" ++ show e

makeConstraintBool :: Map.Map Variable SReal -> Expr -> SBool
makeConstraintBool map (OP "and" e1 e2) = (makeConstraintBool map e1) &&& (makeConstraintBool map e2)
makeConstraintBool map (OP "or" e1 e2) = (makeConstraintBool map e1) ||| (makeConstraintBool map e2)
makeConstraintBool map (OP "<" e1 e2) = (makeConstraintReal map e1) .< (makeConstraintReal map e2)
makeConstraintBool map (OP "<=" e1 e2) = (makeConstraintReal map e1) .<= (makeConstraintReal map e2)
makeConstraintBool map (OP ">" e1 e2) = (makeConstraintReal map e1) .> (makeConstraintReal map e2)
makeConstraintBool map (OP ">=" e1 e2) = (makeConstraintReal map e1) .>= (makeConstraintReal map e2)
makeConstraintBool map (OP "=" e1 e2) = (makeConstraintReal map e1) .== (makeConstraintReal map e2)
makeConstraintBool map (NEG e) = bnot $ makeConstraintBool map e
makeConstraintBool map (ASSIGNS xs) = bAnd $ List.map (makeAssign map) xs
makeConstraintBool _ (BOOL b) = literal b
makeConstraintBool _ e = error $ "invalid bool-valued expression" ++ show e

makeAssign :: Map.Map Variable SReal -> (Variable, Expr) -> SBool
makeAssign map (v, e) = (map Map.! v) .== (makeConstraintReal map e)

