module Postscript (makeLines) where

import Data.SBV
import Data.SBV.Control
import Data.SBV.Internals
import qualified Data.Map as Map
import qualified Data.List as List
import Numeric

import State

definePaper :: String
definePaper = "<< /Pagesize [1000 1000] >> setpagedevice\n"

makeLines :: Map.Map String CW -> [(Variable, Variable, Variable, Variable)] -> String
makeLines map lines = definePaper ++ List.concatMap (makeLine (formatMap map)) lines

makeLine :: Map.Map String Rational -> (Variable, Variable, Variable, Variable) -> String
makeLine map (x1, y1, x2, y2) = makeLinePs (map Map.! x1, map Map.! y1, map Map.! x2, map Map.! y2)

makeLinePs :: (Rational, Rational, Rational, Rational) -> String
makeLinePs (x1, y1, x2, y2) = "newpath\n"
                           ++ showR (1000 * x1) ++ " " ++ showR (1000 * y1) ++ " moveto\n"
                           ++ showR (1000 * (x2-x1)) ++ " " ++ showR (1000 * (y2-y1)) ++ " rlineto\n"
                           ++ showR (-2000 * (x2-x1)) ++ " " ++ showR (-2000 * (y2-y1)) ++ " rlineto\n"
                           ++ "2 setlinewidth\n"
                           ++ "stroke\n\n"

showR :: Rational -> String
showR = show.getRF --Numeric.showFloat . getRF x

getRF :: Rational -> Double
getRF x = fromRational x

formatMap :: Map.Map String CW -> Map.Map String Rational
formatMap map = Map.map getAlgRealValue map

getAlgRealValue :: CW -> Rational
getAlgRealValue cw = toRational $ (fromCW cw :: AlgReal)
