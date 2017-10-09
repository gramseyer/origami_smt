module SMTLib (makeSMTLibStr) where

import State
import qualified Data.List as List

makeSMTLibStr :: State.Transform -> String
makeSMTLibStr (_, _, clauses, freshVars) = startupStr
                                        ++ (makeVarDecls freshVars)
                                        ++ (List.concat (List.map makeClause clauses))
                                        ++ endStr

startupStr :: String
startupStr = "(set-logic QF_NRA)\n"
          ++ "(set-info :status sat)\n" --copied from meti-tarski of QF_NRA benchmarks

makeVarDecls :: Int -> String
makeVarDecls x = List.concat $ List.map makeVarDecl [0..(x-1)]

makeVarDecl :: Int -> String
makeVarDecl x = "(declare-fun x" ++ (show x) ++ " () Real)\n(declare-fun y" ++ (show x) ++ " () Real)\n"

makeClause :: State.Clause -> String
makeClause clause = "(assert " ++ clause ++ ")\n"

endStr :: String
endStr = "(check-sat)\n(get-model)\n(exit)\n"
