module Parser(
    Program (PROGRAM),
    Declaration (DEC_FOLD1,
                 DEC_FOLD2,
                 DEC_FOLD3,
                 DEC_FOLD4,
                 DEC_FOLD5,
                 DEC_FOLD6,
                 DEC_INTERSECT),
    Constraint (CN_PARALLEL),
    Identifier,
    parseProgram) where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Map as Map
import Data.List as List

data Program = PROGRAM [Declaration] [Constraint]
    deriving Show

-- first arg is the name of the variable
data Declaration = DEC_FOLD1 Identifier Identifier Identifier
                 | DEC_FOLD2 Identifier Identifier Identifier
                 | DEC_FOLD3 Identifier Identifier Identifier
                 | DEC_FOLD4 Identifier Identifier Identifier
                 | DEC_FOLD5 Identifier Identifier Identifier Identifier
                 | DEC_FOLD6 Identifier Identifier Identifier Identifier Identifier
                 | DEC_INTERSECT Identifier Identifier Identifier
    deriving Show

data Constraint = CN_PARALLEL Identifier Identifier
    deriving Show

type Identifier = String

parseProgram :: String -> Program
parseProgram str = case parse program "Invalid Parse" str of
    Left x -> error (show x)
    Right x -> x

program :: Parser Program
program = do
    many ignore
    declarations <- many declaration
    constraints <- many constraint
    eof
    return $ PROGRAM declarations constraints

declaration :: Parser Declaration
declaration = try fold1dec
          <|> try fold2dec
          <|> try fold3dec
          <|> try fold4dec
          <|> try fold5dec
          <|> try intersectdec
    
fold1dec :: Parser Declaration
fold1dec = decmaker "fold1" DEC_FOLD1

fold2dec :: Parser Declaration
fold2dec = decmaker "fold2" DEC_FOLD2

fold3dec :: Parser Declaration
fold3dec = decmaker "fold3" DEC_FOLD3

fold4dec :: Parser Declaration
fold4dec = decmaker "fold4" DEC_FOLD4

-- Move first arg point over fold crossing arg2 onto line arg3
fold5dec :: Parser Declaration
fold5dec = do 
    varname <- identifier
    whitespace
    char '='
    whitespace
    string "fold5"
    whitespace
    pointMove <- identifier
    whitespace
    pointCenter <- identifier
    whitespace
    line <- identifier
    whitespace
    endl
    many ignore
    return $ DEC_FOLD5 varname pointMove pointCenter line
    
intersectdec :: Parser Declaration
intersectdec = decmaker "intersect" DEC_INTERSECT

constraint :: Parser Constraint
constraint = constraintParallel

constraintParallel :: Parser Constraint
constraintParallel = do
    string "assert"
    whitespace
    string "parallel"
    whitespace
    var1 <- identifier
    whitespace
    var2 <- identifier
    whitespace
    endl
    many ignore
    return $ CN_PARALLEL var1 var2

decmaker :: String -> (Identifier -> Identifier -> Identifier -> Declaration) -> Parser Declaration
decmaker str cons = do
    varname <- identifier
    whitespace
    char '='
    whitespace
    string str
    whitespace
    arg1 <- identifier
    whitespace
    arg2 <- identifier
    whitespace
    endl
    many ignore
    return $ cons varname arg1 arg2

identifier :: Parser Identifier
identifier = many alphaNum

ignore :: Parser ()
ignore = comment <|> whiteline

whiteline :: Parser ()
whiteline = do
    whitespace
    endl

comment :: Parser ()
comment = do
    char ';'
    manyTill anyChar endl
    return ()

endl :: Parser ()
endl = do
    char '\n'
    return ()

whitespace :: Parser ()
whitespace = do
    many $ satisfy isWhiteSpace
    return ()

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False
