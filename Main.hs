import Parser
import State
import Eval
import SMTLib
import System.Environment
import System.IO

run :: String -> String
run str = SMTLib.makeSMTLibStr $
              State.execTransform $
                  Eval.computeTransform $
                      Parser.parseProgram str

loadFile :: String -> IO (String, String)
loadFile str = do
    contents <- readFile str
    return (str ++ ".smt", contents)

processArgs :: [String] -> IO (String)
processArgs (name:[]) = do
    putStrLn $ "Compiling file \"" ++ name ++ "\""
    return name
processArgs _ = do
    putStrLn "Usage: ./Main <filename"
    error "invalid usage"

outputFile :: (String, String) -> IO ()
outputFile (filename, str) = do
    writeFile filename $ run str

main :: IO ()
main = getArgs >>= processArgs >>= loadFile >>= outputFile
