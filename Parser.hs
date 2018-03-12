module Parser(
    Program (PROGRAM),
    VarDeclaration (VAR_DECL),
    Declaration (..),
    Constraint (..),
    Distance (..),
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
                 | DEC_FOLD4 Identifier Identifier Identifier
                 | DEC_FOLD5 Int Identifier Identifier Identifier Identifier
                 | DEC_FOLD6 Int Identifier Identifier Identifier Identifier Identifier
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
                | CN_DIST_EQ Distance Distance
                | CN_DIST_LT Distance Distance
                | CN_DIST_GT Distance Distance
    deriving Show

data Distance = DIST Identifier Identifier
              | DIST_BINOP Distance Char Distance
              | DIST_CONST Integer
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
    endCommand
    return $ VAR_DECL name

declaration :: Parser Declaration
declaration = try fold1dec
          <|> try fold2dec
          <|> try fold3dec
          <|> try fold4dec
          <|> try fold5decSol1
          <|> try fold5decSol2
          <|> try fold6dec
          <|> try fold7dec
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
fold5decSol1 :: Parser Declaration
fold5decSol1 = ternarydecmaker "fold5_1" $ DEC_FOLD5 1

fold5decSol2 :: Parser Declaration
fold5decSol2 = ternarydecmaker "fold5_2" $ DEC_FOLD5 2

fold6dec :: Parser Declaration
fold6dec = try fold6decsol1
       <|> try fold6decsol2
       <|> try fold6decsol3

fold6decsol1 :: Parser Declaration
fold6decsol1 = quaternarydecmaker "fold6_1" $ DEC_FOLD6 1

fold6decsol2 :: Parser Declaration
fold6decsol2 = quaternarydecmaker "fold6_2" $ DEC_FOLD6 2

fold6decsol3 :: Parser Declaration
fold6decsol3 = quaternarydecmaker "fold6_3" $ DEC_FOLD6 3

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
    endCommand
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
    endCommand
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
            <|> try constraintAngle
            <|> constraintDist

constraintAnd :: Parser Constraint
constraintAnd = constraintGen "AND" CN_AND

constraintOr :: Parser Constraint
constraintOr = constraintGen "OR" CN_OR

constraintGen :: String
             -> (Constraint -> Constraint -> Constraint)
              -> Parser Constraint
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

constraintDist :: Parser Constraint
constraintDist = try distEq
             <|> try distLt
             <|> try distGt

decmaker :: String
         -> (Identifier
          -> Identifier
          -> Identifier
          -> Declaration)
         -> Parser Declaration
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
    endCommand
    return $ cons varname arg1 arg2

dist :: Parser Distance
dist = try distBinop
   <|> try distGen
   <|> distConst

distGen :: Parser Distance
distGen = do
    string "d("
    whitespace
    p1 <- identifier
    whitespace
    p2 <- identifier
    whitespace
    string ")"
    whitespace
    return $ DIST p1 p2

distBinop :: Parser Distance
distBinop = do
    char '('
    whitespace
    d1 <- dist
    whitespace
    op <- binop
    whitespace
    d2 <- dist
    whitespace
    char ')'
    whitespace
    return $ DIST_BINOP d1 op d2

distConst :: Parser Distance
distConst = do
    top <- many1 digit
    --TODO support floats
    whitespace
    return $ DIST_CONST (read top)

distEq :: Parser Constraint
distEq = distDecGenerator "==" CN_DIST_EQ

distLt :: Parser Constraint
distLt = distDecGenerator "<" CN_DIST_LT

distGt :: Parser Constraint
distGt = distDecGenerator ">" CN_DIST_GT

angleEq :: Parser Constraint
angleEq = angleDecGenerator "==" CN_ANG_EQ

angleLt :: Parser Constraint
angleLt = angleDecGenerator "<" CN_ANG_LT

angleGt :: Parser Constraint
angleGt = angleDecGenerator ">" CN_ANG_GT

distDecGenerator :: String -> (Distance -> Distance -> Constraint) -> Parser Constraint
distDecGenerator str cons = do
    d1 <- dist
    string str
    whitespace
    d2 <- dist
    return $ cons d1 d2

angleDecGenerator :: String -> (Angle -> Angle -> Constraint) ->  Parser Constraint
angleDecGenerator str cons = do
    ang1 <- angle
    string str
    whitespace
    ang2 <- angle
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

endCommand :: Parser ()
endCommand = do
    whitespace
    endl
    many ignore
    return ()

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

binop :: Parser Char
binop =  try (char '+') <|> try (char '*') <|> try (char '-') <|> char '/'

isWhiteSpace :: Char -> Bool
isWhiteSpace ' ' = True
isWhiteSpace '\t' = True
isWhiteSpace _ = False
