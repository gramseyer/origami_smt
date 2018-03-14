module Prover (runSolver, runSolvers) where

import Control.Monad
import qualified Data.Map as Map
import Data.SBV
import Data.SBV.Control
import Data.Maybe
import qualified Data.List as List
import State
import Control.Concurrent
import Data.SBV.Internals
import Debug.Trace

-- Pass a set of constraints to an SMT solver.  Has the option of 
-- running multiple solvers in parallel (and returning the first
-- to finish).
runSolvers :: Bool -> State.Transform -> IO (Map.Map String CW)
runSolvers b t = do
    solvers <- sbvAvailableSolvers
    blocker <- newEmptyMVar
    -- Yices is currently disabled by default because of a bug in which one spawned SMT process does not always quit after the other finishes.
    mapM_ (forkIO.(parallelHelper blocker (runSolver b t))) $ [z3]--, disableIncrementalConfig yices]
    model <- takeMVar blocker
    putStrLn.prettyprint $ Map.toList model
    return model

prettyprint :: [(String, CW)] -> String
prettyprint (x:xs) = (show x) ++ "\n" ++ prettyprint xs
prettyprint [] = ""

disableIncremental :: SMTSolver -> SMTSolver
disableIncremental solver = solver { options = const ["--timeout=5"]}

disableIncrementalConfig :: SMTConfig -> SMTConfig
disableIncrementalConfig config = config { solver = disableIncremental (solver config) }

parallelHelper :: MVar (Map.Map String CW) -> (SMTConfig -> IO (Maybe (Map.Map String CW))) -> SMTConfig -> IO ()
parallelHelper blocker f config = do
    result <- f config
    case result of 
        Nothing -> putStrLn $ "no result from smt solver"
        Just x  -> putMVar blocker x

runSolver :: Bool -> State.Transform -> SMTConfig -> IO (Maybe (Map.Map String CW))
runSolver b t config = runSMTWith config $ makeSymbolicCalculation (show . name . solver $ config) b t

makeSymbolicCalculation :: String -> Bool -> State.Transform -> Symbolic (Maybe (Map.Map String CW))
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
                 io $ putStrLn $ solverName ++ " returned unknown"
                 return Nothing
            Unsatisfiable _ -> do
                io $ putStrLn $ solverName ++ " returned unsat"
                return (Just (Map.empty))
            Satisfiable _ _ -> do
                io $ putStrLn $ solverName ++ " returned sat"
                return $ Just $ getModelDictionary cs
            SatExtField _ _ -> do
                io $ putStrLn $ solverName ++ " is not returning sensible results"
                return Nothing
            ProofError _ _ -> do
                io $ putStrLn $ solverName ++ " had an error"
                return Nothing
    return result

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

-- Transform a real-valued term into input for the SMT solver.
makeConstraintReal :: Map.Map Variable SReal -> Expr -> SReal
makeConstraintReal map (VAR v) = map Map.! (v)
makeConstraintReal map (OP "+" e1 e2) = (makeConstraintReal map e1) + (makeConstraintReal map e2)
makeConstraintReal map (OP "-" e1 e2) = (makeConstraintReal map e1) - (makeConstraintReal map e2)
makeConstraintReal map (OP "*" e1 e2) = (makeConstraintReal map e1) * (makeConstraintReal map e2)
makeConstraintReal map (OP "/" e1 e2) = (makeConstraintReal map e1) / (makeConstraintReal map e2)
makeConstraintReal map (CONST' (x, y)) = fromRational (fromInteger x / fromInteger y)
makeConstraintReal _ e = error $ "invalid real-valued expression " ++ show e

-- Transform a boolean-valued term into input for the SMT solver.
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
makeConstraintBool _ e = error $ "invalid bool-valued expression " ++ show e

makeAssign :: Map.Map Variable SReal -> (Variable, Expr) -> SBool
makeAssign map (v, e) = (map Map.! (v)) .== (makeConstraintReal map e)

