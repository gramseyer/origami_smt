module SMTLib (makeSMTLibStr) where

import State
import qualified Data.List as List

makeSMTLibStr :: Bool -> State.Transform -> String
makeSMTLibStr b (_, _, clauses, constraintClauses, freshVars) =
    startupStr
    ++ (makeVarDecls freshVars)
    ++ cornerVarDecls
    ++ (List.concat (List.map (makeClause False) clauses))
    ++ (makeClause b (unifyClauses constraintClauses))
    ++ endStr

startupStr :: String
startupStr = "(set-logic QF_NRA)\n"
          ++ "(set-info :status sat)\n" --copied from meti-tarski of QF_NRA benchmarks

makeVarDecls :: Int -> String
makeVarDecls x = List.concat $ List.map makeVarDecl [0..(x-1)]

makeVarDecl :: Int -> String
makeVarDecl x = "(declare-fun x" ++ (show x) ++ " () Real)\n(declare-fun y" ++ (show x) ++ " () Real)\n"

cornerVarDecls :: String
cornerVarDecls = "(declare-fun _left () Real)\n(declare-fun _right () Real)\n(declare-fun _top () Real)\n(declare-fun _bottom () Real)\n"

foldClauses :: String -> String -> String
foldClauses c1 c2 = "(and " ++ c1 ++ " " ++ c2 ++ ")"

unifyClauses :: [String] -> String
unifyClauses clauses = List.foldr foldClauses "" clauses

makeClause :: Bool -> String -> String
makeClause False clause = "(assert " ++ clause ++ ")\n"
makeClause True clause = "(assert (not " ++ clause ++ "))\n"

endStr :: String
endStr = "(check-sat)\n(get-model)\n(exit)\n"
