module State (
    TransformState,
    Transform (constructionClauses, assertionClauses, freshVarCnt),
    Variable,
    Clause,
    execTransform,
    getPointVars,
    getLineVars,
    addPoint,
    addLine,
    freshVarPair,
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
                     freshVarCnt :: Int }

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
                   freshVarCnt =  0 }

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
        Nothing -> error $"undefined line " ++ iden

freshVarPair :: TransformState (Variable, Variable)
freshVarPair = state $ \t 
    -> (("x" ++ show (freshVarCnt t), "y" ++ show (freshVarCnt t)), t { freshVarCnt = (freshVarCnt t) + 1 })

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
addClause c = state $ \t -> ((), t { constructionClauses = c:(constructionClauses t) })

addConstraintClause :: Clause -> TransformState ()
addConstraintClause c = state $ \t -> ((), t { assertionClauses = c:(assertionClauses t) })

doNothing :: TransformState ()
doNothing = state $ \s -> ((), s)
