import Parser
import State
import Eval
import SMTLib
import Z3Lib
import System.Environment
import System.IO
import qualified Data.List as List
import qualified Data.Map as Map
import Prover
import Postscript

import Data.SBV

run :: (State.Transform -> IO String) -> String -> IO String
run f = f.State.execTransform.Eval.computeTransform.Parser.parseProgram

loadFile :: (String, (State.Transform -> IO String), String) -> IO (String, String, (State.Transform -> IO String))
loadFile (str, func, suffix) = do
    contents <- readFile str
    let filename = str ++ suffix
    putStrLn $ "Writing to " ++ filename
    return (filename, contents, func)

processArgs :: [String] -> IO (String, (State.Transform -> IO String), String)
processArgs (name:options) = do
    let negateB = elem "--negate" options
    let (outputFunc, outputType) = if elem "--z3" options
        then
            (Z3Lib.makeOutputStr, ".py")
        else if elem "--smt" options
            then 
                (SMTLib.makeOutputStr, ".smt")
            else
                (genPostScript, ".ps")
    return (name, outputFunc negateB, outputType)
processArgs _ = do
    putStrLn "Usage: ./Main <input_file> <optiosn>"
    error "invalid usage"

genPostScript :: Bool -> State.Transform -> IO String
genPostScript b t = do
    map <- Prover.runSolvers b t
    let lines = List.map snd $ Map.toList (State.lineMap t)
    return $ Postscript.makeDocument map (List.reverse $ State.lineList t) (State.lineMap t)

validateOptions :: [String] -> IO ([String])
validateOptions (name:opts) = if not $ List.null (opts List.\\ ["--negate", "--z3", "--smt"])
                                  then error "invalid option"
                                  else return (name:opts)

outputFile :: (String, String, (State.Transform -> IO String)) -> IO ()
outputFile (filename, str, f) = run f str >>= writeFile filename

main :: IO ()
main = getArgs >>= validateOptions >>= processArgs >>= loadFile >>= outputFile

