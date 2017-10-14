module State (
    TransformState,
    Transform (constructionClauses, assertionClauses, freshVarCnt, varNameMap),
    Variable,
    Clause,
    Expr (OP, VAR, CONST, BOOL, NEG, ASSIGN, ASSIGNS, SQR, LIST),
    resolveExpr,
    reduceExpr,
    normalizeExpr,
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

type Constant = (Int, Int)

data Expr = OP String Expr Expr
          | VAR String
          | CONST' Constant
          | BOOL Bool
          | NEG Expr
          | ASSIGNS [(Variable, Expr)]
          -- Non normalized forms
          | SQR Expr
          | CONST Int
          | LIST String [Expr]
          | ASSIGN Variable Expr
    deriving Show

data Transform = T { pointMap :: Map.Map Parser.Identifier (Variable, Variable), --(x,y)
                     lineMap :: Map.Map Parser.Identifier (Variable, Variable, Variable, Variable), --(x1, y1, x2, y2)
                     constructionClauses :: [Clause],
                     assertionClauses :: [Clause],
                     freshVarCnt :: Int,
                     varNameMap :: Map.Map Int String,
                     valueMap :: Map.Map String Expr
                     }

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
                   varNameMap = Map.empty,
                   valueMap = Map.fromList [("_left", CONST' (0,1)),
                                            ("_right", CONST' (1,1)),
                                            ("_top", CONST' (1,1)),
                                            ("_bottom", CONST' (0,1))] }

normalizeExpr :: Expr -> Expr
-- remove weird forms
normalizeExpr (SQR x) = OP "*" (normalizeExpr x) (normalizeExpr x)
normalizeExpr (LIST str (x:(y:xs))) = OP str (normalizeExpr x) (normalizeExpr (LIST str (y:xs)))
normalizeExpr (LIST str [x]) = (normalizeExpr x)
-- recurse down expression tree
normalizeExpr (CONST x) = (CONST' (x, 1))
normalizeExpr (OP s e1 e2) = OP s (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (NEG e) = NEG (normalizeExpr e)
normalizeExpr (ASSIGN s e) = ASSIGNS [(s, normalizeExpr e)]
normalizeExpr (ASSIGNS xs) = ASSIGNS $  List.map (\(s, e)->(s, normalizeExpr e)) xs
normalizeExpr e = e


processAssign :: (Variable, Expr) -> TransformState ()
processAssign (v, CONST' x) = bindVariable v (CONST' x)
processAssign (v, BOOL b) = bindVariable v (BOOL b)
processAssign _ = doNothing

-- Does not add to clause list.  Must be done elsewhere.
resolveExpr :: Expr -> TransformState (String)
resolveExpr e = do
    vMap <- getValueMap
    let e' = reduceExpr vMap (normalizeExpr e)
    case e' of
        ASSIGNS xs -> List.foldr ((>>).processAssign) doNothing xs
        _ -> return ()
    return $ translateExpr e'

showInt :: Int -> String
showInt x = if x>= 0 then show x else "(- " ++ show (abs x) ++ ")"

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

translateAssign :: (Variable, Expr) -> String
translateAssign (v, e) = "(= " ++ v ++ " " ++ translateExpr e ++ ")"

translateExpr :: Expr -> String
translateExpr (VAR v)         = v
translateExpr (OP str e1 e2)  = "(" ++ str ++ " " ++ translateExpr e1 ++ " " ++ translateExpr e2 ++ ")"
translateExpr (CONST' (x, y)) = "(/ " ++ showInt x ++ " " ++ showInt y ++ " )"
translateExpr (NEG expr)      = "(not " ++ translateExpr expr ++ ")"
translateExpr (BOOL b)        = showBool b
translateExpr (ASSIGNS xs)    = "(and " ++ List.concatMap translateAssign xs ++ ")"
translateExpr k = error $ "unnormalized input to translateExpr" ++ show k

reduceExpr :: Map.Map String Expr -> Expr -> Expr
reduceExpr map (OP str e1 e2) = combine str (reduceExpr map e1) (reduceExpr map e2)
reduceExpr map (VAR str) = case Map.lookup str map of
    Just static -> static
    Nothing -> VAR str
reduceExpr map (CONST' x) = CONST' x
reduceExpr map (BOOL b) = BOOL b
reduceExpr map (NEG e) = negateExpr (reduceExpr map e)
reduceExpr map (ASSIGNS xs) = ASSIGNS (List.map (mapAssigns map) xs)
reduceExpr _ e = error $ show e

mapAssigns :: Map.Map String Expr -> (String, Expr) -> (String, Expr)
mapAssigns m (v, e) = (v, reduceExpr m e)

negateExpr :: Expr -> Expr
negateExpr (BOOL b) = BOOL (not b)
negateExpr x = NEG x

plus :: Constant -> Constant -> Constant
plus (x1, y1) (x2, y2) = (x1*y2 + x2*y1, y1*y2)

minus :: Constant -> Constant -> Constant
minus (x1, y1) (x2, y2) = (x1*y2 - x2 * y1, y1*y2)

times :: Constant -> Constant -> Constant
times (x1, y1) (x2, y2) = (x1*x2, y1*y2)

divide :: Constant -> Constant -> Constant
divide (x1, y1) (x2, y2) = (x1*y2, y1*x2)

comparison :: (Int -> Int -> Bool) -> Constant -> Constant -> Bool
comparison f (x1, y1) (x2, y2) = ((x1*y2) `f` (x2*y1))

equals :: Constant -> Constant -> Bool
equals = comparison (==)

lt :: Constant -> Constant -> Bool
lt = comparison (<)

gt :: Constant -> Constant -> Bool
gt = comparison (>)

lte :: Constant -> Constant -> Bool
lte = comparison (<=)

gte :: Constant -> Constant -> Bool
gte = comparison (>=)

combine :: String -> Expr -> Expr -> Expr
combine "and" (BOOL False) _ = BOOL False
combine "and" _ (BOOL False) = BOOL False
combine "and" (BOOL true) e = e
combine "and" e (BOOL true) = e
combine "or" (BOOL True) _ = BOOL True
combine "or" _ (BOOL True) = BOOL True
combine "or" (BOOL False) e = e
combine "or" e (BOOL False) = e
combine "+" (CONST' x) (CONST' y) = CONST' (x `plus` y)
combine "-" (CONST' x) (CONST' y) = CONST' (x `minus` y)
combine "*" (CONST' x) (CONST' y) = CONST' (x `times` y)
combine "/" (CONST' x) (CONST' y) = CONST' (x `divide` y) -- If this throws an error, then the situation wasn't satisfiable anyways
combine "=" (CONST' x) (CONST' y) = BOOL (x `equals` y)
combine "<" (CONST' x) (CONST' y) = BOOL (x `lt` y)
combine ">" (CONST' x) (CONST' y) = BOOL (x `gt` y)
combine ">=" (CONST' x) (CONST' y) = BOOL (x `gte` y)
combine "<=" (CONST' x) (CONST' y) = BOOL (x `lte` y)
combine str e1 e2 = OP str e1 e2

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

getValueMap :: TransformState (Map.Map String Expr)
getValueMap = state $ \t -> (valueMap t, t)

bindVariable :: Variable -> Expr -> TransformState ()
bindVariable v e = state $ \t -> ((), t { valueMap = (Map.insert v e (valueMap t)) })

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
                         else state $ \t -> ((), t { constructionClauses = c:(constructionClauses t) }) where

addConstraintClause :: Clause -> TransformState ()
addConstraintClause c = if c == "" then error "constraint clause cannot be empty" 
                                   else state $ \t -> ((), t { assertionClauses = c:(assertionClauses t) }) where

doNothing :: TransformState ()
doNothing = state $ \s -> ((), s)
