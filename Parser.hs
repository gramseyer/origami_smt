module Parser(
    Program (PROGRAM),
    VarDeclaration (VAR_DECL),
    Declaration (DEC_FOLD1,
                 DEC_FOLD2,
                 DEC_FOLD3,
                 DEC_NFOLD3,
                 DEC_FOLD4,
                 DEC_FOLD5_SOL1,
                 DEC_FOLD5_SOL2,
                 DEC_NFOLD5,
                 DEC_FOLD6,
                 DEC_FOLD7,
                 DEC_INTERSECT),
    Constraint (CN_AND,
                CN_OR,
                CN_NEG,
                CN_PARALLEL,
                CN_PERPENDICULAR,
                CN_COLINEAR,
                CN_ANG_EQ,
                CN_ANG_GT,
                CN_ANG_LT),
    Identifier,
    parseProgram) where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.Map as Map
import Data.List as List

data Program = PROGRAM [VarDeclaration] [Declaration] [Constraint] [Constraint]
    deriving Show

newtype VarDeclaration = VAR_DECL Identifier
    deriving Show

-- first arg is the name of the variable
data Declaration = DEC_FOLD1 Identifier Identifier Identifier
                 | DEC_FOLD2 Identifier Identifier Identifier
                 | DEC_FOLD3 Identifier Identifier Identifier
                 | DEC_NFOLD3 Identifier Identifier Identifier
                 | DEC_FOLD4 Identifier Identifier Identifier
                 | DEC_FOLD5_SOL1 Identifier Identifier Identifier Identifier
                 | DEC_FOLD5_SOL2 Identifier Identifier Identifier Identifier
                 | DEC_NFOLD5 Identifier Identifier Identifier Identifier
                 | DEC_FOLD6 Int Int Identifier Identifier Identifier Identifier Identifier
                 | DEC_FOLD7 Identifier Identifier Identifier Identifier
                 | DEC_INTERSECT Identifier Identifier Identifier
    deriving Show

data Constraint = CN_AND Constraint Constraint
                | CN_OR Constraint Constraint
                | CN_NEG Constraint
                | CN_PARALLEL Identifier Identifier
                | CN_PERPENDICULAR Identifier Identifier
                | CN_COLINEAR Identifier Identifier
                | CN_ANG_EQ Angle Angle
                | CN_ANG_GT Angle Angle
                | CN_ANG_LT Angle Angle
    deriving Show

data Angle = ANGLE Identifier Identifier Identifier
    deriving Show

type Identifier = String

parseProgram :: String -> Program
parseProgram str = case parse program "Invalid Parse" str of
    Left x -> error (show x)
    Right x -> x

program :: Parser Program
program = do
    many ignore
    vardeclarations <- many vardeclaration
    declarations <- many declaration
    constructions <- many construct
    assertions <- many assert
    eof
    return $ PROGRAM vardeclarations declarations constructions assertions

vardeclaration :: Parser VarDeclaration
vardeclaration = do
    string "VAR"
    whitespace
    name <- identifier
    whitespace
    endl
    many ignore
    return $ VAR_DECL name

declaration :: Parser Declaration
declaration = try fold1dec
          <|> try fold2dec
          <|> try fold3dec
          <|> try nfold3dec
          <|> try fold4dec
          <|> try fold5decSol1
          <|> try fold5decSol2
          <|> try nfold5dec
          <|> try fold6dec
          <|> try fold7dec
          <|> try intersectdec
    
fold1dec :: Parser Declaration
fold1dec = decmaker "fold1" DEC_FOLD1

fold2dec :: Parser Declaration
fold2dec = decmaker "fold2" DEC_FOLD2

fold3dec :: Parser Declaration
fold3dec = decmaker "fold3" DEC_FOLD3

nfold3dec :: Parser Declaration
nfold3dec = decmaker "nfold3" DEC_NFOLD3

fold4dec :: Parser Declaration
fold4dec = decmaker "fold4" DEC_FOLD4

-- Move first arg point over fold crossing arg2 onto line arg3
fold5decSol1 :: Parser Declaration
fold5decSol1 = ternarydecmaker "fold5_sol1" DEC_FOLD5_SOL1

fold5decSol2 :: Parser Declaration
fold5decSol2 = ternarydecmaker "fold5_sol2" DEC_FOLD5_SOL2
--like the above but nondeterministic choice of which fold to use
nfold5dec :: Parser Declaration
nfold5dec = ternarydecmaker "nfold5" DEC_NFOLD5

fold6dec :: Parser Declaration
fold6dec = try fold6dec3sol1
       <|> try fold6dec3sol2
       <|> try fold6dec3sol3
       <|> try fold6dec2sol1
       <|> try fold6dec2sol2
       <|> try fold6dec1sol1

fold6dec3sol1 :: Parser Declaration
fold6dec3sol1 = quaternarydecmaker "fold6_1/3" $ DEC_FOLD6 3 1

fold6dec3sol2 :: Parser Declaration
fold6dec3sol2 = quaternarydecmaker "fold6_2/3" $ DEC_FOLD6 3 2

fold6dec3sol3 :: Parser Declaration
fold6dec3sol3 = quaternarydecmaker "fold6_3/3" $ DEC_FOLD6 3 3

fold6dec2sol1 :: Parser Declaration
fold6dec2sol1 = quaternarydecmaker "fold6_1/2" $ DEC_FOLD6 2 1

fold6dec2sol2 :: Parser Declaration
fold6dec2sol2 = quaternarydecmaker "fold6_2/2" $ DEC_FOLD6 2 2

fold6dec1sol1 :: Parser Declaration
fold6dec1sol1 = quaternarydecmaker "fold6_1/1" $ DEC_FOLD6 1 1


fold7dec :: Parser Declaration
fold7dec = ternarydecmaker "fold7" DEC_FOLD7

ternarydecmaker :: String
                -> (Identifier
                 -> Identifier
                 -> Identifier
                 -> Identifier
                 -> Declaration)
                -> Parser Declaration
ternarydecmaker str cons = do
    varname <- identifier
    whitespace
    char '='
    whitespace
    string str
    whitespace
    pointMove <- identifier
    whitespace
    pointCenter <- identifier
    whitespace
    line <- identifier
    whitespace
    endl
    many ignore
    return $ cons varname pointMove pointCenter line

quaternarydecmaker :: String
                    -> (Identifier
                     -> Identifier
                     -> Identifier
                     -> Identifier
                     -> Identifier
                     -> Declaration)
                    -> Parser Declaration
quaternarydecmaker str cons = do
    varname <- identifier
    whitespace
    char '='
    whitespace
    string str
    whitespace
    p1 <- identifier
    whitespace
    l1 <- identifier
    whitespace
    p2 <- identifier
    whitespace
    l2 <- identifier
    whitespace
    endl
    many ignore
    return $ cons varname p1 l1 p2 l2

intersectdec :: Parser Declaration
intersectdec = decmaker "intersect" DEC_INTERSECT

construct :: Parser Constraint
construct = do
    string "CONSTRUCT"
    whitespace
    cn <- constraintInt
    endl
    many ignore
    return cn

assert :: Parser Constraint
assert= do
    string "ASSERT"
    whitespace
    cn <- constraintInt
    endl
    many ignore
    return cn

constraintInt :: Parser Constraint
constraintInt = try constraintAnd
            <|> try constraintOr
            <|> try constraintNeg
            <|> try constraintParallel
            <|> try constraintPerpendicular
            <|> try constraintColinear
            <|> constraintAngle

constraintAnd :: Parser Constraint
constraintAnd = constraintGen "AND" CN_AND

constraintOr :: Parser Constraint
constraintOr = constraintGen "OR" CN_OR

constraintGen :: String -> (Constraint -> Constraint -> Constraint) ->  Parser Constraint
constraintGen str constructor = do
    string str
    whitespace
    char '('
    whitespace
    cn1 <- constraintInt
    char ')'
    whitespace
    char '('
    whitespace
    cn2 <- constraintInt
    char ')'
    whitespace
    return $ constructor cn1 cn2

constraintNeg :: Parser Constraint
constraintNeg = do
    string "NOT"
    whitespace
    char '('
    whitespace
    cn <- constraintInt
    char ')'
    whitespace
    return $ CN_NEG cn
    
constraintParallel :: Parser Constraint
constraintParallel = do
    string "isParallel"
    whitespace
    var1 <- identifier
    whitespace
    var2 <- identifier
    whitespace
    return $ CN_PARALLEL var1 var2

constraintPerpendicular :: Parser Constraint
constraintPerpendicular = do
    string "isPerpendicular"
    whitespace
    var1 <- identifier
    whitespace
    var2 <- identifier
    whitespace
    return $ CN_PERPENDICULAR var1 var2

constraintColinear :: Parser Constraint
constraintColinear = do
    string "isColinear"
    whitespace
    point <- identifier
    whitespace
    line <- identifier
    whitespace
    return $ CN_COLINEAR point line

constraintAngle :: Parser Constraint
constraintAngle = try angleEq
              <|> try angleLt
              <|> try angleGt

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

angleEq :: Parser Constraint
angleEq = angleDecGenerator "==" CN_ANG_EQ

angleLt :: Parser Constraint
angleLt = angleDecGenerator "<" CN_ANG_LT

angleGt :: Parser Constraint
angleGt = angleDecGenerator ">" CN_ANG_GT

angleDecGenerator :: String -> (Angle -> Angle -> Constraint) ->  Parser Constraint
angleDecGenerator str cons = do
    ang1 <- angle
    string str
    whitespace
    ang2 <- angle
    whitespace
    return $ cons ang1 ang2

angle :: Parser Angle
angle = do
    string "/_"
    whitespace
    arg1 <- identifier
    whitespace
    arg2 <- identifier
    whitespace
    arg3 <- identifier
    whitespace
    return $ ANGLE arg1 arg2 arg3

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
