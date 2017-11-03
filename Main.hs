import Parser
import State
import Eval
import SMTLib
import Z3Lib
import System.Environment
import System.IO
import qualified Data.List as List
import Prover

import Data.SBV

run :: (State.Transform -> String) -> String -> String
run f = f.State.execTransform.Eval.computeTransform.Parser.parseProgram

loadFile :: (String, (State.Transform -> String), String) -> IO (String, String, (State.Transform -> String))
loadFile (str, func, suffix) = do
    contents <- readFile str
    let filename = str ++ suffix
    putStrLn $ "Writing to " ++ filename
    return (filename, contents, func)

processArgs :: [String] -> IO (String, (State.Transform -> String), String)
{-processArgs [name] = do
    putStrLn $ "Compiling file \"" ++ name ++ "\""
    return (name, False)
processArgs [name,"--negate"] = do
    putStrLn $ "Compiling file \"" ++ name ++ "\" to negated form"
    return (name, True)
processArgs _ = do
    putStrLn "Usage: ./Main <filename> <--negate>"
    error "invalid usage"
-}
--processArgs :: [String] -> IO ()
processArgs (name:options) = do
   --let negateB = False
    --let outputFunc = SMTLib.makeOutputStr
    --let outputType = ".smt"
    let negateB = elem "--negate" options
    let (outputFunc, outputType) = if elem "--z3" options
        then
            (Z3Lib.makeOutputStr, ".py")
        else
            (SMTLib.makeOutputStr, ".smt")
    return (name, outputFunc negateB, outputType)
processArgs _ = do
    putStrLn "Usage: ./Main <input_file> <optiosn>"
    error "invalid usage"

validateOptions :: [String] -> IO ([String])
validateOptions (name:opts) = if not $ List.null (opts List.\\ ["--negate", "--z3"])
                                  then error "invalid option"
                                  else return (name:opts)

outputFile :: (String, String, (State.Transform -> String)) -> IO ()
outputFile (filename, str, f) = writeFile filename $ run f str

runProver :: (String, String, (State.Transform -> String)) -> IO ()
runProver (filename, str, f) = do
   vars <- (Prover.runSolvers False) . State.execTransform . Eval.computeTransform .  Parser.parseProgram $ str
   putStrLn $ show vars

main :: IO ()
main = getArgs >>= validateOptions >>= processArgs >>= loadFile >>= runProver -- >>= outputFile
