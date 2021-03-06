module Z3Lib (makeOutputStr) where

import State
import qualified Data.List as List
import qualified Data.Map as Map

makeOutputStr :: Bool -> State.Transform -> IO String
makeOutputStr b t = return $
    startupStr 
    ++ makeVarDecls (State.varNameList t)
--    ++ cornerVarDecls
    ++ List.concatMap (makeClause False) (List.map Just (List.reverse (State.constructionClauses t)))
    ++ makeClause b (unifyClauses (State.assertionClauses t))
    ++ endStr

startupStr :: String
startupStr = "from z3 import *\n"
          ++ "s = Solver()\n"

endStr :: String
endStr = "print s.check()\nprint s.model()"

makeVarDecls :: [Variable] -> String
makeVarDecls varnames = List.concatMap makeVarDecl varnames

makeVarDecl :: Variable -> String
makeVarDecl varname = show varname ++ " = Real('" ++ show varname ++ "')\n"

--cornerVarDecls :: String 
--cornerVarDecls = "_left = Real('_left')\n"
  --            ++ "_right = Real('_right')\n"
    --          ++ "_top = Real('_top')\n"
      --        ++ "_bottom = Real('_bottom')\n"

makeClause :: Bool -> Maybe Clause -> String
makeClause False (Just clause) = "s.add(" ++ translateExpr clause ++ ")\n"
makeClause True (Just clause) = "s.add(" ++ translateExpr (NEG clause) ++ ")\n"
makeClause _ Nothing = ""

foldClauses :: Expr -> Expr -> Expr
foldClauses e1 e2 = (OP "and" e1 e2)

unifyClauses :: [Expr] -> Maybe Expr
unifyClauses [] = Nothing
unifyClauses xs = Just $ List.foldr1 foldClauses xs

translateExpr :: Expr -> String
translateExpr (VAR v) = show v
translateExpr (OP "and" e1 e2) = "(And (" ++ translateExpr e1 ++ ", " ++ translateExpr e2 ++ "))"
translateExpr (OP "or" e1 e2)  = "(Or (" ++ translateExpr e1 ++ ", " ++ translateExpr e2 ++ "))"
translateExpr (OP "=" e1 e2)   = "(" ++ translateExpr e1 ++ " == " ++ translateExpr e2 ++ ")"
translateExpr (OP binop e1 e2) = "(" ++ translateExpr e1 ++ binop  ++ translateExpr e2 ++ ")"
translateExpr (CONST' (x,y)) = "(" ++ show x ++ ".0 / " ++ show y ++ ".0)"
translateExpr (NEG expr) = "(Not (" ++ translateExpr expr ++ "))"
translateExpr (BOOL b) = show b
translateExpr (ASSIGNS xs) = "(And (" ++ List.foldr1 (\s1 -> (\s2 -> s1 ++ ", " ++ s2)) (List.map translateAssign xs) ++ "))"

translateAssign :: (Variable, Expr) -> String
translateAssign (v, e) = "(" ++ show v ++ " == " ++ translateExpr e ++ ")"




