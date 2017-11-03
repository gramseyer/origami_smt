module Prover (runSolver, runSolvers) where

import Control.Monad
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import qualified Data.List as List
import State
import Control.Concurrent
import Data.SBV.Internals

runSolvers :: Bool -> State.Transform -> IO (Map.Map String CW)
runSolvers b t = do
    solvers <- sbvAvailableSolvers
    blocker <- newEmptyMVar
    mapM_ (forkIO.(parallelHelper blocker (runSolver b t))) [disableIncrementalConfig yices] -- $ List.map disableIncrementalConfig $ [z3]
    model <- takeMVar blocker
    putStrLn $ show model
    return model

disableIncremental :: SMTSolver -> SMTSolver
disableIncremental solver = solver { options = const ["--mcsat"]}

disableIncrementalConfig :: SMTConfig -> SMTConfig
disableIncrementalConfig config = config { solver = disableIncremental (solver config) }

parallelHelper :: MVar (Map.Map String CW) -> (SMTConfig -> IO (Map.Map String CW)) -> SMTConfig -> IO ()
parallelHelper blocker f config = do
    result <- f config
    putMVar blocker result

runSolver :: Bool -> State.Transform -> SMTConfig -> IO (Map.Map String CW)
runSolver b t config = runSMTWith config $ makeSymbolicCalculation (show . name . solver $ config) b t

makeSymbolicCalculation :: String -> Bool -> State.Transform -> Symbolic (Map.Map String CW)
makeSymbolicCalculation solverName b t = do
    varDeclMap <- makeVarDecls (State.varNameList t)
    assertConstraints varDeclMap (State.constructionClauses t)
    if b then assertNegatedConstraints varDeclMap (State.assertionClauses t)
         else assertConstraints varDeclMap (State.assertionClauses t)
    setLogic QF_NRA
    result <- query $ do
        cs <- getSMTResult
        case cs of
            Unknown _ _ -> do
                 -- TODO wrap this all in a maybe and then have this return Nothing and then take mvars until we get a just
                 io $ putStrLn $ solverName ++ "smt returned unknown"
                 return Map.empty
            Unsatisfiable _ -> do
                io $ putStrLn $ solverName ++ "returned unsat"
                return Map.empty
            Satisfiable _ _ -> do
                io $ putStrLn $ solverName ++ " returned sat"
                --io $ putStrLn $ show model
                return $ getModelDictionary cs
            SatExtField _ _ -> do
                io $ putStrLn $ solverName ++ " is wtf"
                return Map.empty
            ProofError _ _ -> do
                io $ putStrLn $ solverName ++ " had an error"
                return Map.empty
    return result

--formatModel :: SMTModel -> [(Variable, AlgReal)]
--formatModel model = List.map (\(str, cw) -> (str, fromCW cw)) (modelAssocs model)

makeVarDecls :: [Variable] -> Symbolic (Map.Map Variable SReal)
makeVarDecls varnames = liftM Map.fromList $ List.foldr (liftM2 (:)) (return []) (List.map makeVarDecl varnames)
    
makeVarDecl :: Variable -> Symbolic (Variable, SReal)
makeVarDecl varname = do
    sVar <- sReal varname
    return (varname, sVar)

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

