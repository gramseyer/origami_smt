import Parser
import State
import Eval
import SMTLib
import Z3Lib
import System.Environment
import System.IO

run :: (State.Transform -> String) -> String -> String
run f = f.State.execTransform.Eval.computeTransform.Parser.parseProgram

loadFile :: (String, (State.Transform -> String), String) -> IO (String, String, (State.Transform -> String))
loadFile (str, func, suffix) = do
    contents <- readFile str
    return (str ++ suffix, contents, func)

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

outputFile :: (String, String, (State.Transform -> String)) -> IO ()
outputFile (filename, str, f) = writeFile filename $ run f str

main :: IO ()
main = getArgs >>= processArgs >>= loadFile >>= outputFile
