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
    ) where

import Graphics.UI.WX(Point, point, pointX, pointY)
import Text.Parse

{-
data DoublePoint = DoublePoint
    { doublePointX :: !Double
    , doublePointY :: !Double
    }
    deriving (Show, Eq, Read)
-}
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
