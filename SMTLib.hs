module SMTLib (makeOutputStr) where

import State
import qualified Data.List as List
import qualified Data.Map as Map

makeOutputStr :: Bool -> State.Transform -> IO String
makeOutputStr b t = return $
    startupStr
    ++ makeVarDecls (State.varNameList t)
  --  ++ cornerVarDecls
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

makeVarDecls :: [Variable] -> String
makeVarDecls varnames = List.concatMap makeVarDecl varnames

makeVarDecl :: Variable -> String
makeVarDecl varname = "(" ++ vf ++ " " ++ showVar varname ++ " " ++ vf2 ++ " Real)\n"

--cornerVarDecls :: String
--cornerVarDecls = "(" ++ vf ++ " _left " ++ vf2 ++ " Real)\n"
 --             ++ "(" ++ vf ++ " _right " ++ vf2 ++ " Real)\n"
   --           ++ "(" ++ vf ++ " _top " ++ vf2 ++ " Real)\n"
     --         ++ "(" ++ vf ++ " _bottom " ++ vf2 ++ " Real)\n"

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
smtlibTranslateExpr (VAR v)         = showVar v
smtlibTranslateExpr (OP str e1 e2)  = "(" ++ str ++ " " ++ smtlibTranslateExpr e1 ++ " " ++ smtlibTranslateExpr e2 ++ ")"
smtlibTranslateExpr (CONST' (x, y)) = "(/ " ++ showInt x ++ " " ++ showInt y ++ ")"
smtlibTranslateExpr (NEG expr)      = "(not " ++ smtlibTranslateExpr expr ++ ")"
smtlibTranslateExpr (BOOL b)        = showBool b
smtlibTranslateExpr (ASSIGNS xs)    = "(and " ++ List.concatMap translateAssign xs ++ ")"
translateExpr k = error $ "unnormalized input to smtlibTranslateExpr " ++ show k

translateAssign :: (Variable, Expr) -> String
translateAssign (v, e) = "(= " ++ showVar v ++ " " ++ smtlibTranslateExpr e ++ ")"

showBool :: Bool -> String
showBool True = "true"
showBool False = "false"

showInt :: Integer -> String
showInt x = if x >= 0 then show x else "(- " ++ show (abs x) ++ ")"

showVar :: Variable -> String
showVar str = str
