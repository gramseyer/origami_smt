module SMTLib (makeOutputStr) where

import State
import qualified Data.List as List
import qualified Data.Map as Map
{-
makeZ3Str :: Bool -> State.Transform -> String
makeZ3Str b t = 
    z3startStr
    ++ z3MakeVarDecls (State.varNameMap t) (State.freshVarCnt t)
    ++ z3CornerVarDecls
    ++ List.concatMap (z3makeClause False) (List.reverse (State.constructionClauses t))
    ++ z3makeClause b (unifyClauses (State.assertionClauses t))
    ++ z3endStr

z3MakeVarDecl :: (Map.Map Int String) -> Int -> String
z3MakeVarDecl varnames x = varname ++ "_x" ++ show x ++ " = Real('" ++ varname ++ "_x" ++ show x ++ "')\n"
                        ++ varname ++ "_y" ++ show x ++ " = Real('" ++ varname ++ "_y" ++ show x ++ "')\n" where
    varname = case Map.lookup x varnames of
        Nothing -> error $ "couldn't find var number " ++ show x
        Just name -> name

z3makeVarDecls :: (Map.M
-}
z3CornerVarDecls :: String
z3CornerVarDecls = "_left = Real('_left')\n"
                ++ "_right = Real('_right')\n"
                ++ "_top = Real('_top')\n"
                ++ "_bottom = Real('_bottom')\n"

makeOutputStr :: Bool -> State.Transform -> String
makeOutputStr b t =
    startupStr
    ++ makeVarDecls (State.varNameMap t) (State.freshVarCnt t)
    ++ cornerVarDecls
    ++ List.concatMap (makeClause False) (List.map Just (List.reverse (State.constructionClauses t)))
    ++ makeClause b (unifyClauses (State.assertionClauses t))
    ++ endStr

startupStr :: String
startupStr = "(set-logic QF_NRA)\n"
          ++ "(set-info :status sat)\n" --copied from meti-tarski of QF_NRA benchmarks

vf :: String
vf = "declare-fun"

vf2 :: String
vf2 = "()"

makeVarDecls :: (Map.Map Int String) -> Int -> String
makeVarDecls varnames x = List.concatMap (makeVarDecl varnames) [0..(x-1)]

makeVarDecl :: (Map.Map Int String) -> Int -> String
makeVarDecl varnames x = "(" ++ vf ++ " " ++ varname ++ "_x" ++ show x ++ " " ++ vf2 ++ " Real)\n"
                      ++ "(" ++ vf ++ " " ++ varname ++ "_y" ++ show x ++ " " ++ vf2 ++ " Real)\n" where
    varname = case Map.lookup x varnames of
        Nothing -> error "couldn't find variable number" ++ (show x)
        Just name -> name

cornerVarDecls :: String
cornerVarDecls = "(" ++ vf ++ " _left " ++ vf2 ++ " Real)\n"
              ++ "(" ++ vf ++ " _right " ++ vf2 ++ " Real)\n"
              ++ "(" ++ vf ++ " _top " ++ vf2 ++ " Real)\n"
              ++ "(" ++ vf ++ " _bottom " ++ vf2 ++ " Real)\n"

foldClauses :: Expr -> Expr -> Expr
foldClauses c1 c2 = OP "and" c1 c2

unifyClauses :: [Expr] -> Maybe Expr
unifyClauses [] = Nothing
unifyClauses xs = Just $ List.foldr1 foldClauses xs

makeClause :: Bool -> Maybe Clause -> String
makeClause False (Just clause) = "(assert " ++ (smtlibTranslateExpr clause) ++ ")\n"
makeClause True (Just clause) = "(assert (not " ++ (smtlibTranslateExpr clause) ++ "))\n"
makeClause _ Nothing = ""

endStr :: String
endStr = "(get-info :all-statistics)\n"
      ++ "(check-sat)\n"
      ++ "(get-model)\n"
      ++ "(get-info :all-statistics)\n"
      ++ "(exit)\n"

smtlibTranslateExpr :: Expr -> String
smtlibTranslateExpr (VAR v)         = v
smtlibTranslateExpr (OP str e1 e2)  = "(" ++ str ++ " " ++ smtlibTranslateExpr e1 ++ " " ++ smtlibTranslateExpr e2 ++ ")"
smtlibTranslateExpr (CONST' (x, y)) = "(/ " ++ showInt x ++ " " ++ showInt y ++ ")"
smtlibTranslateExpr (NEG expr)      = "(not " ++ smtlibTranslateExpr expr ++ ")"
smtlibTranslateExpr (BOOL b)        = showBool b
smtlibTranslateExpr (ASSIGNS xs)    = "(and " ++ List.concatMap translateAssign xs ++ ")"
translateExpr k = error $ "unnormalized input to smtlibTranslateExpr " ++ show k

translateAssign :: (Variable, Expr) -> String
translateAssign (v, e) = "(= " ++ v ++ " " ++ smtlibTranslateExpr e ++ ")"

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

showInt :: Integer -> String
showInt x = if x >= 0 then show x else "(- " ++ show (abs x) ++ ")"
