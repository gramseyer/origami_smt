module SMTLib (makeSMTLibStr) where

import State
import qualified Data.List as List
import qualified Data.Map as Map

makeSMTLibStr :: Bool -> State.Transform -> String
makeSMTLibStr b t =
    startupStr
    ++ makeVarDecls (State.varNameMap t) (State.freshVarCnt t)
    ++ cornerVarDecls
    ++ List.concatMap (makeClause False) (List.reverse (State.constructionClauses t))
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

foldClauses :: String -> String -> String
foldClauses c1 c2 = "(and " ++ c1 ++ " " ++ c2 ++ ")"

unifyClauses :: [String] -> String
unifyClauses = List.foldr foldClauses ""

makeClause :: Bool -> String -> String
makeClause _ "" = ""
makeClause False clause = "(assert " ++ clause ++ ")\n"
makeClause True clause = "(assert (not " ++ clause ++ "))\n"

endStr :: String
endStr = "(get-info :all-statistics)\n"
      ++ "(check-sat)\n"
      ++ "(get-model)\n"
      ++ "(get-info :all-statistics)\n"
      ++ "(exit)\n"
