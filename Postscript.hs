module Postscript (makeDocument) where

import Data.SBV
import Data.SBV.Control
import Data.SBV.Internals
import qualified Data.Map as Map
import qualified Data.List as List
import Numeric

import State

definePaper :: String
definePaper = "<< /PageSize [1100 1100] >> setpagedevice\n\n"

makeLines :: Map.Map String CW -> [(Variable, Variable, Variable, Variable)] -> String
makeLines map lines = definePaper ++ List.concatMap (makeLine (formatMap map)) lines

makeLine :: Map.Map String Float -> (Variable, Variable, Variable, Variable) -> String
makeLine map (x1, y1, x2, y2) = makeLinePs (map Map.! x1, map Map.! y1, map Map.! x2, map Map.! y2)

makeLinePs :: (Float, Float, Float, Float) -> String
makeLinePs (x1, y1, x2, y2) = "newpath\n"
                           ++ show (1000 * x1  + 50   )    ++ " " ++ show (1000 * y1  + 50  )   ++ " moveto\n"
                           ++ show (1000 * (x2-x1))  ++ " " ++ show (1000 * (y2-y1))  ++ " rlineto\n"
                           ++ show (-2000 * (x2-x1)) ++ " " ++ show (-2000 * (y2-y1)) ++ " rlineto\n"
                           ++ "2 setlinewidth\n"
                           ++ "stroke\n\n"

makePage :: Map.Map String CW -> Map.Map String (Variable, Variable, Variable, Variable) -> [String] -> String
makePage map lineMap lineNames = (makeLines map (List.map ((Map.!) lineMap) lineNames )) ++ makeText (List.last lineNames) ++  "showpage\n\n"

makeText :: String -> String
makeText str = "/Times-Roman findfont\n24 scalefont\nsetfont\nnewpath\n100 25 moveto\n(" ++ str ++ ") show\n"

makeDocument :: Map.Map String CW -> [String] -> Map.Map String (Variable, Variable, Variable, Variable) -> String
makeDocument map lineNames lineMap = List.concatMap (makePage map lineMap) $ List.map (flip List.take $ lineNames) [1..(List.length lineNames)]

formatMap :: Map.Map String CW -> Map.Map String Float
formatMap map = Map.map getAlgRealValue map

getAlgRealValue :: CW -> Float
getAlgRealValue cw = case fromCW cw of
    AlgRational _ r -> fromRational r
    AlgPolyRoot _ (Just str) -> read $ dropEllipsis str

dropEllipsis :: String -> String
dropEllipsis str = take ((length str) -3) str

