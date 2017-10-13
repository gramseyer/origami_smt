module State (
    TransformState,
    Transform (constructionClauses, assertionClauses, freshVarCnt, varNameMap),
    Variable,
    Clause,
    execTransform,
    getPointVars,
    getLineVars,
    addPoint,
    addLine,
    freshVarPair,
    freshNamedVarPair,
    addClause,
    addConstraintClause,
    doNothing) where

import Parser
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.List as List

type TransformState a = State Transform a

data Transform = T { pointMap :: Map.Map Parser.Identifier (Variable, Variable), --(x,y)
                     lineMap :: Map.Map Parser.Identifier (Variable, Variable, Variable, Variable), --(x1, y1, x2, y2)
                     constructionClauses :: [Clause],
                     assertionClauses :: [Clause],
                     freshVarCnt :: Int,
                     varNameMap :: Map.Map Int String}

type Clause = String
type Variable = String

cornerVars :: [(Parser.Identifier, (Variable, Variable))]
cornerVars = [("LB", ("_left", "_bottom")),
              ("RB", ("_right", "_bottom")),
              ("RT", ("_right", "_top")),
              ("LT", ("_left", "_top"))]

initialState :: Transform
initialState = T { pointMap = Map.fromList cornerVars,
                   lineMap = Map.empty, 
                   constructionClauses = ["(= _right 1)", "(= _left 0)", "(= _top 1)", "(= _bottom 0)" ],
                   assertionClauses = [],
                   freshVarCnt =  0,
                   varNameMap = Map.empty }

execTransform :: TransformState a -> Transform
execTransform st = execState st initialState 

getPointMap :: TransformState (Map.Map Parser.Identifier (Variable, Variable))
getPointMap = state $ \t -> (pointMap t, t)

putPointMap :: Map.Map Parser.Identifier (Variable, Variable) -> TransformState ()
putPointMap ptMap = state $ \t -> ((), t {pointMap = ptMap})

getLineMap :: TransformState (Map.Map Parser.Identifier (Variable, Variable, Variable, Variable))
getLineMap = state $ \t -> (lineMap t, t)

putLineMap :: Map.Map Parser.Identifier (Variable, Variable, Variable, Variable) -> TransformState ()
putLineMap lineMap = state $ \t -> ((), t { lineMap = lineMap })

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
        Nothing -> error $ "undefined line " ++ iden

freshVarPair :: TransformState (Variable, Variable)
freshVarPair = freshNamedVarPair ""

freshNamedVarPair :: String -> TransformState (Variable, Variable)
freshNamedVarPair s = state $ \t
    -> ((str ++ "_x" ++ show (freshVarCnt t), str ++ "_y" ++ show (freshVarCnt t)),
        t { varNameMap = Map.insert (freshVarCnt t) str (varNameMap t), freshVarCnt = (freshVarCnt t) + 1}) where
    str = disableNaming s

--disable variable naming
disableNaming :: String -> String
disableNaming str = str

addPoint :: Parser.Identifier -> TransformState (Variable, Variable)
addPoint iden = do
    ptMap <- getPointMap
    case Map.lookup iden ptMap of
        Just v -> error ("Redefining point " ++ iden)
        Nothing -> do 
            newVars <- freshNamedVarPair $ iden ++ "_point"
            putPointMap (Map.insert iden newVars ptMap)
            return newVars

addLine :: Parser.Identifier -> TransformState (Variable, Variable, Variable, Variable)
addLine iden = do
    lineMap <- getLineMap
    case Map.lookup iden lineMap of
        Just v -> error $ "Redefining line " ++ iden
        Nothing -> do
            (x1, y1) <- freshNamedVarPair $ iden ++ "_line_1"
            (x2, y2) <- freshNamedVarPair $ iden ++ "_line_2"
            putLineMap (Map.insert iden (x1, y1, x2, y2) lineMap)
            return (x1, y1, x2, y2)

addClause :: Clause -> TransformState ()
addClause c = if c == "" then error "clause cannot be empty"
                         else state $ \t -> ((), t { constructionClauses = c:(constructionClauses t) })

addConstraintClause :: Clause -> TransformState ()
addConstraintClause c = if c == "" then error "constraint clause cannot be empty" 
                                   else state $ \t -> ((), t { assertionClauses = c:(assertionClauses t) })

doNothing :: TransformState ()
doNothing = state $ \s -> ((), s)
