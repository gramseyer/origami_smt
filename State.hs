module State (
    TransformState,
    Transform (lineMap, constructionClauses, assertionClauses, varNameList, lineList),
    Variable,
    Clause,
    Expr (OP, VAR, CONST, CONST', BOOL, NEG, ASSIGN, ASSIGNS, SQR, LIST),
    execTransform,
    resolveExpr,
    getPointVars,
    getLineVars,
    addPoint,
    addLine,
    freshVarPair,
    freshNamedVarPair,
    freshNamedVar,
    getDistVarName,
    newDistance,
    hasDistance,
    addClause,
    addConstraintClause,
    doNothing,
    paperSize) where

import Parser
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.List as List
import Data.SBV
import qualified Data.Set as Set
import Debug.Trace

type TransformState a = State Transform a

type Constant = (Integer, Integer)

data Expr = OP String Expr Expr
          | VAR Variable
          | CONST' Constant
          | BOOL Bool
          | NEG Expr
          | ASSIGNS [(Variable, Expr)]
          -- Non normalized forms
          | SQR Expr
          | CONST Integer
          | LIST String [Expr]
          | ASSIGN Variable Expr
    deriving Show

data Transform = T {
    -- map from textual point names to SMT variable names.  Output is (x_coordinate, y_coordinate)
    pointMap            :: Map.Map Parser.Identifier (Variable, Variable),
    -- map from textual line name to SMT variable names.  Ouptut is (x_1, y_1, x_2, y_2)
    lineMap             :: Map.Map Parser.Identifier (Variable, Variable, Variable, Variable),
    -- list of clauses involved in an origami construction.
    constructionClauses :: [Clause],
    -- list of clauses to be asserted.  Negated, depending on options chosen.
    assertionClauses    :: [Clause],
    -- increments on each new variable declaration.
    freshVarCnt         :: Int,
    -- list of declared SMT variables
    varNameList         :: [Variable],
    -- map from variables to assigned expressions.  Used in constraint optimization. 
    valueMap            :: Map.Map Variable Expr,
    -- List of line names.  Used in visualization system.
    lineList            :: [String],
    -- Set of point-point distance declarations.  Used in computing distance constraints.
    distanceSet         :: Set.Set (Parser.Identifier, Parser.Identifier)
    }
    deriving Show

type Clause = Expr
type Variable = String --V (Symbolic SReal, String)

{-
instance Eq Variable where
    (V (_, x)) == (V (_, y)) = x==y

instance Show Variable where
    show (V (_, x)) = x

instance Ord Variable where
    V (_, x) <= V (_, y) = x<=y
    compare (V (_, x)) (V (_, y)) = compare x y
-}
mkVariable :: String -> Variable
mkVariable = id --str = V (sReal str, str)

-- The dimension of the paper.
paperSize :: Integer
paperSize = 1

left :: Variable
left = mkVariable "_left"

right :: Variable
right = mkVariable "_right"

top :: Variable
top = mkVariable "_top"

bottom :: Variable
bottom = mkVariable "_bottom"

cornerVars :: [(Parser.Identifier, (Variable, Variable))]
cornerVars = [("LB", (left, bottom)),
              ("RB", (right, bottom)),
              ("RT", (right, top)),
              ("LT", (left, top))]

initialState :: Transform
initialState = T { pointMap = Map.fromList cornerVars,
                   lineMap = Map.empty, 
                   constructionClauses = [ASSIGNS [(left, CONST' (0,1)),
                                                   (right, CONST' (paperSize, 1)),
                                                   (top, CONST' (paperSize, 1)),
                                                   (bottom, CONST' (0,1))]],
                   assertionClauses = [],
                   freshVarCnt =  0,
                   varNameList = [left, right, bottom, top],
                   valueMap = Map.fromList [(left, CONST' (0,1)),
                                            (right, CONST' (paperSize,1)),
                                            (top, CONST' (paperSize,1)),
                                            (bottom, CONST' (0,1))],
                   lineList = [],
                   distanceSet = Set.empty
                 }


--Transform logged expressions into normalized forms.
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

--Maps 
processAssign :: (Variable, Expr) -> TransformState ()
processAssign (v, CONST' x) = bindVariable v (CONST' x)
processAssign (v, BOOL b) = bindVariable v (BOOL b)
processAssign (v, VAR newVar) = bindVariable v (VAR newVar)
processAssign _ = doNothing

-- Does not add to clause list.  Must be done elsewhere.
-- In our constraint generation, we have labelled substitutable variable
-- assignments with "ASSIGN x c" instead of "OP = x c", guaranteeing that
-- each variable is only "assigned" once and it's first reference is an
-- assignment, if it is ever assigned.
resolveExpr :: Expr -> TransformState (Expr)
resolveExpr e = do
    vMap <- getValueMap
    let e' = reduceExpr vMap (normalizeExpr e)
    case e' of
        ASSIGNS xs -> List.foldr ((>>).processAssign) doNothing xs
        _ -> return ()
    return e'

--showInt :: Integer -> String
--showInt x = if x>= 0 then show x else "(- " ++ show (abs x) ++ ")"

--showBool :: Bool -> String
--showBool True = "true"
--showBool False = "false"

--translateAssign :: (Variable, Expr) -> String
--translateAssign (v, e) = "(= " ++ v ++ " " ++ translateExpr e ++ ")"

--translateExpr :: Expr -> String
--translateExpr (VAR v)         = v
--translateExpr (OP str e1 e2)  = "(" ++ str ++ " " ++ translateExpr e1 ++ " " ++ translateExpr e2 ++ ")"
--translateExpr (CONST' (x, y)) = "(/ " ++ showInt x ++ " " ++ showInt y ++ " )"
--translateExpr (NEG expr)      = "(not " ++ translateExpr expr ++ ")"
--translateExpr (BOOL b)        = showBool b
--translateExpr (ASSIGNS xs)    = "(and " ++ List.concatMap translateAssign xs ++ ")"
--translateExpr k = error $ "unnormalized input to translateExpr" ++ show k

reduceExpr :: Map.Map Variable Expr -> Expr -> Expr
reduceExpr map (OP str e1 e2) = combine str (reduceExpr map e1) (reduceExpr map e2)
reduceExpr map (VAR str) = case Map.lookup str map of
    Just static -> static
    Nothing -> VAR str
reduceExpr map (CONST' x) = CONST' x
reduceExpr map (BOOL b) = BOOL b
reduceExpr map (NEG e) = negateExpr (reduceExpr map e)
reduceExpr map (ASSIGNS xs) = ASSIGNS (List.map (mapAssigns map) xs)
reduceExpr _ e = error $ show e

mapAssigns :: Map.Map Variable Expr -> (Variable, Expr) -> (Variable, Expr)
mapAssigns m (v, e) = (v, reduceExpr m e)

negateExpr :: Expr -> Expr
negateExpr (BOOL b) = BOOL (not b)
negateExpr x = NEG x

plus :: Constant -> Constant -> Constant
plus (x1, y1) (x2, y2) = removeGCD (x1*y2 + x2*y1, y1*y2)

minus :: Constant -> Constant -> Constant
minus (x1, y1) (x2, y2) = removeGCD (x1*y2 - x2 * y1, y1*y2)

times :: Constant -> Constant -> Constant
times (x1, y1) (x2, y2) = removeGCD (x1*x2, y1*y2)

divide :: Constant -> Constant -> Constant
divide (x1, y1) (x2, y2) = removeGCD (x1*y2, y1*x2)

removeGCD :: Constant -> Constant
removeGCD (a, b) = normalizeNegatives (a `div` c, b `div` c) where
    c = gcd a b

normalizeNegatives :: Constant -> Constant
normalizeNegatives (a, b) = if (b < 0) then ((-a), (-b)) else (a, b)

comparison :: (Integer -> Integer -> Bool) -> Constant -> Constant -> Bool
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

hasDistance :: (Parser.Identifier, Parser.Identifier) -> TransformState (Bool)
hasDistance (p1, p2) = do
    distSet <- getDistanceSet
    if Set.member (p1, p2) distSet then
        return True
      else 
        return False

newDistance :: (Parser.Identifier, Parser.Identifier) -> TransformState (Variable)
newDistance (p1, p2) = do
   b <- hasDistance (p1, p2)
   if b then
       return (getDistVarName p1 p2)
     else do
       addDistVar (getDistVarName p1 p2)
       distSet <- getDistanceSet
       addDistanceToSet (p1, p2)
       addDistanceToSet (p2, p1)
       return $ getDistVarName p1 p2

--Doesn't guarantee the variable exists
getDistVarName :: Parser.Identifier -> Parser.Identifier -> String
getDistVarName p1 p2 = if p1 < p2 then
    "d_" ++ p1 ++ "_" ++ p2
  else
    "d_" ++ p2 ++ "_" ++ p1 

freshVarPair :: TransformState (Variable, Variable)
freshVarPair = freshNamedVarPair ""

freshNamedVarPair :: String -> TransformState (Variable, Variable)
freshNamedVarPair str = do
    xv <- freshNamedVar $ str ++ "_x"
    yv <- freshNamedVar $ str ++ "_y"
    return (xv, yv)

freshNamedVar :: String -> TransformState(Variable)
freshNamedVar str = do
    varCnt <- getFreshVarCnt
    let varName = makeVarName str varCnt
    let var = mkVariable varName
    addNamedVar var

getFreshVarCnt :: TransformState (Int)
getFreshVarCnt = state $ \t -> (freshVarCnt t, t)

addNamedVar :: Variable -> TransformState (Variable)
addNamedVar str = state $ \t
    -> (str,
        t { varNameList = str : (varNameList t), freshVarCnt = (freshVarCnt t) + 1})

addDistVar :: Variable -> TransformState (Variable)
addDistVar str = state $ \t
    -> (str,
        t { varNameList = str : (varNameList t) })

makeVarName :: String -> Int -> String
makeVarName str varCnt = str ++ "_" ++ show varCnt

getDistanceSet :: TransformState (Set.Set (Parser.Identifier, Parser.Identifier))
getDistanceSet = state $ \t -> (distanceSet t, t)

addDistanceToSet :: (Parser.Identifier, Parser.Identifier) -> TransformState ()
addDistanceToSet (p1, p2) = state $ \t -> 
    ((), t { distanceSet = Set.insert (p1, p2) (distanceSet t) } )

--disable variable naming
--disableNaming :: String -> String
--disableNaming str = str

getValueMap :: TransformState (Map.Map Variable Expr)
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
    pushLineName iden
    case Map.lookup iden lineMap of
        Just v -> error $ "Redefining line " ++ iden
        Nothing -> do
            (x1, y1) <- freshNamedVarPair $ iden ++ "_line_1"
            (x2, y2) <- freshNamedVarPair $ iden ++ "_line_2"
            putLineMap (Map.insert iden (x1, y1, x2, y2) lineMap)
            return (x1, y1, x2, y2)

pushLineName :: String -> TransformState ()
pushLineName s = state $ \t -> ((), t { lineList = s : lineList t })

addClause :: Clause -> TransformState ()
addClause c = state $ \t -> ((), t { constructionClauses = c:(constructionClauses t) })

addConstraintClause :: Clause -> TransformState ()
addConstraintClause c = state $ \t -> ((), t { assertionClauses = c:(assertionClauses t) })

doNothing :: TransformState ()
doNothing = state $ \s -> ((), s)

