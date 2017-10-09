module State (
    TransformState,
    Transform,
    Variable,
    Clause,
    execTransform,
    getPointVars,
    getLineVars,
    addPoint,
    addLine,
    freshVarPair,
    addClause,
    doNothing) where


import Parser
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.List as List

type TransformState a = State Transform a

type Transform = (Map.Map Parser.Identifier (Variable, Variable), --(x,y)
                  Map.Map Parser.Identifier (Variable, Variable, Variable, Variable), --(x1, y1, x2, y2)
                  [Clause],
                  Int)

type Clause = String
type Variable = String

cornerVars :: [(Parser.Identifier, (Variable, Variable))]
cornerVars = [("LB", ("0", "0")), ("RB", ("1", "0")), ("RT", ("1", "1")), ("LT", ("0", "1"))]

initialState :: Transform
initialState = (Map.fromList cornerVars, Map.empty, [], 0)

execTransform :: TransformState a -> Transform
execTransform st = execState st initialState 

getPointMap :: TransformState (Map.Map Parser.Identifier (Variable, Variable))
getPointMap = state $ \(ptMap, lineMap, clauses, freshCnt) -> (ptMap, (ptMap, lineMap, clauses, freshCnt))

putPointMap :: Map.Map Parser.Identifier (Variable, Variable) -> TransformState ()
putPointMap ptMap = state $ \(_, lineMap, clauses, freshCnt) -> ((), (ptMap, lineMap, clauses, freshCnt))

getLineMap :: TransformState (Map.Map Parser.Identifier (Variable, Variable, Variable, Variable))
getLineMap = state $ \(ptMap, lineMap, clauses, freshCnt) -> (lineMap, (ptMap, lineMap, clauses, freshCnt))

putLineMap :: Map.Map Parser.Identifier (Variable, Variable, Variable, Variable) -> TransformState ()
putLineMap lineMap = state $ \(ptMap, _, clauses, freshCnt) -> ((),(ptMap, lineMap, clauses, freshCnt))

getPointVars :: Parser.Identifier -> TransformState (Variable, Variable)
getPointVars iden = do
    ptMap <- getPointMap
    case Map.lookup iden ptMap of
        Just v -> return v
        Nothing -> error $ "undefined pt " ++ iden

getLineVars :: Parser.Identifier -> TransformState (Variable, Variable, Variable, Variable)
getLineVars iden = do
    lineMap <- getLineMap
    case Map.lookup iden lineMap of
        Just v -> return v
        Nothing -> error $"undefined line " ++ iden

freshVarPair :: TransformState (Variable, Variable)
freshVarPair = state $
    \(ptMap, lineMap, clauses, freshCnt)
        -> (("x" ++ (show freshCnt), "y" ++ (show freshCnt)), (ptMap, lineMap, clauses, freshCnt + 1))


addPoint :: Parser.Identifier -> TransformState (Variable, Variable)
addPoint iden = do
    ptMap <- getPointMap
    case Map.lookup iden ptMap of
        Just v -> error ("Redefining point " ++ iden)
        Nothing -> do 
            newVars <- freshVarPair
            putPointMap (Map.insert iden newVars ptMap)
            return newVars

addLine :: Parser.Identifier -> TransformState (Variable, Variable, Variable, Variable)
addLine iden = do
    lineMap <- getLineMap
    case Map.lookup iden lineMap of
        Just v -> error $ "Redefining line " ++ iden
        Nothing -> do
            (x1, y1) <- freshVarPair
            (x2, y2) <- freshVarPair
            putLineMap (Map.insert iden (x1, y1, x2, y2) lineMap)
            return (x1, y1, x2, y2)

addClause :: Clause -> TransformState ()
addClause c = state $ \(ptMap, lineMap, clauses, freshCnt) -> ((), (ptMap, lineMap, c:clauses, freshCnt))

doNothing :: TransformState ()
doNothing = state $ \s -> ((), s)
