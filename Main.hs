import Parser
import State
import Eval
import SMTLib
import System.Environment
import System.IO

run :: String -> Bool -> String
run str b = SMTLib.makeSMTLibStr b $
                State.execTransform $
                    Eval.computeTransform $
                        Parser.parseProgram str

loadFile :: (String, Bool) -> IO (String, String, Bool)
loadFile (str, b) = do
    contents <- readFile str
    return (str ++ ".smt", contents, b)

processArgs :: [String] -> IO (String, Bool)
processArgs (name:[]) = do
    putStrLn $ "Compiling file \"" ++ name ++ "\""
    return (name, False)
processArgs (name:"--negate":[]) = do
    putStrLn $ "Compiling file \"" ++ name ++ "\" to negated form"
    return (name, True)
processArgs _ = do
    putStrLn "Usage: ./Main <filename> <--negate>"
    error "invalid usage"

outputFile :: (String, String, Bool) -> IO ()
outputFile (filename, str, b) = do
    writeFile filename $ run str b

main :: IO ()
main = getArgs >>= processArgs >>= loadFile >>= outputFile
