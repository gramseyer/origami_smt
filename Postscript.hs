module Postscript (makeLines) where

import Data.SBV
import Data.SBV.Control
import Data.SBV.Internals
import qualified Data.Map as Map
import qualified Data.List as List
import Numeric

import Debug.Trace
import State

definePaper :: String
definePaper = "<< /PageSize [1000 1000] >> setpagedevice\n\n"

makeLines :: Map.Map String CW -> [(Variable, Variable, Variable, Variable)] -> String
makeLines map lines = definePaper ++ List.concatMap (makeLine (formatMap map)) lines

makeLine :: Map.Map String Float -> (Variable, Variable, Variable, Variable) -> String
makeLine map (x1, y1, x2, y2) = makeLinePs (map Map.! x1, map Map.! y1, map Map.! x2, map Map.! y2)

makeLinePs :: (Float, Float, Float, Float) -> String
makeLinePs (x1, y1, x2, y2) = "newpath\n"
                           ++ show (1000 * x1)       ++ " " ++ show (1000 * y1)       ++ " moveto\n"
                           ++ show (1000 * (x2-x1))  ++ " " ++ show (1000 * (y2-y1))  ++ " rlineto\n"
                           ++ show (-2000 * (x2-x1)) ++ " " ++ show (-2000 * (y2-y1)) ++ " rlineto\n"
                           ++ "2 setlinewidth\n"
                           ++ "stroke\n\n"

formatMap :: Map.Map String CW -> Map.Map String Float
formatMap map = Map.map getAlgRealValue map

getAlgRealValue :: CW -> Float
getAlgRealValue cw = case fromCW cw of
    AlgRational _ r -> fromRational r
    AlgPolyRoot _ (Just str) -> trace str $ read $ dropEllipsis str

dropEllipsis :: String -> String
dropEllipsis str = take ((length str) -3) str

