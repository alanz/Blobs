module Graphics.Blobs.Math
    ( DoublePoint(..), Vector
    , doublePointX, doublePointY
    , intPointToDoublePoint
    , doublePointToIntPoint
    , translatePolar
    , distancePointPoint
    , distanceSegmentPoint
    , subtractDoublePoint
    , subtractDoublePointVector
    , vectorLength
    , vectorAngle
    , origin
    , translate
    , enclosedInRectangle

    -- For XML I/O
    , makeTag
    , tagWithId
    , simpleString
    , escapeString
    , comment
    , commentEscape
    ) where

import Graphics.UI.WX(Point, point, pointX, pointY)
import Text.Parse

import qualified Text.XML.HaXml.XmlContent.Haskell as XML
import Char
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Types

-- ---------------------------------------------------------------------

data DoublePoint = DoublePoint !Double !Double
    deriving (Show, Eq, Read)

instance Parse DoublePoint where
    parse = do { isWord "DoublePoint"
               ; return DoublePoint `apply` parse `apply` parse
               }

data Vector = Vector !Double !Double

doublePointX :: DoublePoint -> Double
doublePointX (DoublePoint x _) = x

doublePointY :: DoublePoint -> Double
doublePointY (DoublePoint _ y) = y

origin :: DoublePoint
origin = DoublePoint 0 0

-- | Compute distance between two points
distancePointPoint :: DoublePoint -> DoublePoint -> Double
distancePointPoint (DoublePoint x0 y0) (DoublePoint x1 y1) =
    sqrt (square (x0 - x1)  + square (y0 - y1))

square :: Double -> Double
square d = d*d

-- | Compute distance from a segment (as opposed to a line) to a point
--   Formulas taken from
--   <http://geometryalgorithms.com/Archive/algorithm_0102/algorithm_0102.htm>
distanceSegmentPoint :: DoublePoint -> DoublePoint -> DoublePoint -> Double
distanceSegmentPoint p0 p1 p =
    let v  = p1 `subtractDoublePointVector` p0
        w  = p  `subtractDoublePointVector` p0
        c1 = dotProduct w v
        c2 = dotProduct v v
    in if c1 <= 0 then distancePointPoint p p0
       else if c2 <= c1 then distancePointPoint p p1
       else distanceLinePoint p0 p1 p

-- | Compute distance from a line to a point
distanceLinePoint :: DoublePoint -> DoublePoint -> DoublePoint -> Double
distanceLinePoint (DoublePoint x0 y0) (DoublePoint x1 y1) (DoublePoint x y) =
    abs ( ( (y0 - y1) * x + (x1 - x0) * y + (x0 * y1 - x1 * y0) ) /
          sqrt (square (x1 - x0) + square (y1 - y0))
        )

subtractDoublePointVector :: DoublePoint -> DoublePoint -> Vector
subtractDoublePointVector (DoublePoint x0 y0) (DoublePoint x1 y1) =
    Vector (x0 - x1) (y0 - y1)

-- | Translate a point relative to a new origin
translate :: DoublePoint -> DoublePoint -> DoublePoint
translate (DoublePoint originX originY) (DoublePoint x y) =
    DoublePoint (x+originX) (y+originY)

subtractDoublePoint :: DoublePoint -> DoublePoint -> DoublePoint
subtractDoublePoint (DoublePoint x0 y0) (DoublePoint x1 y1) =
    DoublePoint (x0 - x1) (y0 - y1)

dotProduct :: Vector -> Vector -> Double
dotProduct (Vector v1 v2) (Vector w1 w2) = v1 * w1 + v2 * w2

translatePolar :: Double -> Double -> DoublePoint -> DoublePoint
translatePolar angle distance (DoublePoint x y) =
    DoublePoint (x + cos angle * distance) (y + sin angle * distance)

doublePointToIntPoint :: DoublePoint -> Point
doublePointToIntPoint (DoublePoint x y) = point (round x) (round y)

intPointToDoublePoint :: Point -> DoublePoint
intPointToDoublePoint pt =
    DoublePoint (fromIntegral (pointX pt)) (fromIntegral (pointY pt))

vectorAngle :: Vector -> Double
vectorAngle (Vector v1 v2) = atan2 v2 v1

vectorLength :: Vector -> Double
vectorLength (Vector v1 v2) = sqrt (square v1 + square v2)

enclosedInRectangle :: DoublePoint -> DoublePoint -> DoublePoint -> Bool
enclosedInRectangle (DoublePoint x y) (DoublePoint x0 y0) (DoublePoint x1 y1) =
    between x x0 x1 && between y y0 y1
  where
    between i j k | j <= k    =  j <= i && i <= k
                  | otherwise =  k <= i && i <= j

-- ---------------------------------------------------------------------
-- Moving orphan instances home
instance XML.HTypeable DoublePoint where
    toHType _ = XML.Defined "DoublePoint" [] [XML.Constr "X" [] [], XML.Constr "Y" [] []]
instance XML.XmlContent DoublePoint where
    toContents (DoublePoint x y) =
        [ simpleString "X"          (show x)
        , simpleString "Y"          (show y)
        ]
    parseContents = do
        { x <- XML.inElement "X" $ fmap read XML.text
        ; y <- XML.inElement "Y" $ fmap read XML.text
        ; return (DoublePoint x y)
        }


---- UTILITY FUNCTIONS

-- Abbreviations
makeTag :: String -> [XML.Content i] -> XML.Content i
makeTag tagName children = XML.CElem (XML.Elem tagName [] children) undefined

tagWithId :: String -> String -> [XML.Content i] -> XML.Content i
tagWithId tagName identity children =
    XML.CElem (XML.Elem tagName [("id", XML.AttValue [Left identity])] children) undefined

-- | A simple string contains no spaces or unsafe characters
simpleString :: String -> String -> XML.Content i
simpleString tag value =
    XML.CElem (XML.Elem tag [] [ XML.CString False value undefined ]) undefined

-- | The string value may contain spaces and unsafe characters
escapeString :: String -> String -> XML.Content i
escapeString key value =
    XML.CElem ((if isSafe value then id else escape) $
             XML.Elem key [] [ XML.CString (any isSpace value) value undefined ])
          undefined
  where
    isSafe cs = all isSafeChar cs
    isSafeChar c = isAlpha c || isDigit c || c `elem` "- ."

    escape :: XML.Element i -> XML.Element i
    escape = xmlEscape stdXmlEscaper

comment :: String -> XML.Content i
comment s = XML.CMisc (Comment (commentEscape s)) undefined

-- Replace occurences of "-->" with "==>" in a string so that the string
-- becomes safe for an XML comment
commentEscape :: String -> String
commentEscape [] = []
commentEscape ('-':'-':'>':xs) = "==>" ++ commentEscape xs
commentEscape (x:xs) = x : commentEscape xs

