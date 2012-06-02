module Graphics.Blobs.Shape
       (
         Shape(..)
       , ShapeStyle(..)
       , circle
       , logicalDraw
       , logicalLineSegments
       ) where

import Graphics.Blobs.CommonIO
import qualified Graphics.UI.WX as WX
import Graphics.UI.WXCore hiding (Colour)
--import Graphics.UI.WXCore.Draw
import Graphics.Blobs.Math
import Text.Parse
--import Text.XML.HaXml.XmlContent
--import NetworkFile

import Graphics.Blobs.Colors
import Graphics.Blobs.Constants

data Shape =
    Circle  { shapeStyle :: ShapeStyle, shapeRadius :: Double }
  | Polygon { shapeStyle :: ShapeStyle, shapePerimeter :: [DoublePoint] }
						-- centred on (0,0)
  | Lines   { shapeStyle :: ShapeStyle, shapePerimeter :: [DoublePoint] }
						-- no fill for open shape
  | Composite { shapeSegments :: [Shape] }	-- drawn in given order
  deriving (Eq, Show, Read)

data ShapeStyle = ShapeStyle
    { styleStrokeWidth  :: Int
    , styleStrokeColour :: Colour
    , styleFill		:: Colour
    }
  deriving (Eq, Show, Read)

instance Parse Shape where
  parse = oneOf
      [ do{ isWord "Circle"
          ; return Circle
              `discard` isWord "{" `apply` field "shapeStyle"
              `discard` isWord "," `apply` field "shapeRadius"
              `discard` isWord "}"
          }
      , do{ isWord "Polygon"
          ; return Polygon
              `discard` isWord "{" `apply` field "shapeStyle"
              `discard` isWord "," `apply` field "shapePerimeter"
              `discard` isWord "}"
          }
      , do{ isWord "Lines"
          ; return Lines
              `discard` isWord "{" `apply` field "shapeStyle"
              `discard` isWord "," `apply` field "shapePerimeter"
              `discard` isWord "}"
          }
      , do{ isWord "Composite"
          ; return Composite
              `discard` isWord "{" `apply` field "shapeSegments"
              `discard` isWord "}"
          }
      ] `adjustErr` (++"\nexpected a Shape (Circle,Polygon,Lines,Composite)")

instance Parse ShapeStyle where
  parse = do{ isWord "ShapeStyle"
            ; return ShapeStyle
                `discard` isWord "{" `apply` field "styleStrokeWidth"
                `discard` isWord "," `apply` field "styleStrokeColour"
                `discard` isWord "," `apply` field "styleFill"
                `discard` isWord "}"
            }

{-
instance HTypeable Shape where
  toHType s = Defined "Shape" [] [ Constr "Circle" [] []
                                 , Constr "Polygon" [] []
                                 , Constr "Lines" [] []
                                 , Constr "Composite" [] []
                                 ]
instance XmlContent Shape where
  toContents s@(Circle{}) =
      [ mkElemC "Circle" (toContents (shapeStyle s)
                      ++ [mkElemC "radius" (toContents (shapeRadius s))]) ]
  toContents s@(Polygon{}) =
      [ mkElemC "Polygon" (toContents (shapeStyle s)
                      ++ [mkElemC "perimeter" (concatMap toContents
                                                         (shapePerimeter s))]) ]
  toContents s@(Lines{}) =
      [ mkElemC "Lines" (toContents (shapeStyle s)
                      ++ [mkElemC "perimeter" (concatMap toContents
                                                         (shapePerimeter s))]) ]
  toContents s@(Composite{}) =
      [ mkElemC "Composite" (concatMap toContents (shapeSegments s)) ]
  parseContents = do
      { e@(Elem t _ _) <- element ["Circle","Polygon","Lines","Composite"]
      ; case t of
          "Circle" -> interior e $
                       do{ style <- parseContents
                         ;  r <- inElement "radius" parseContents
                         ; return (Circle {shapeStyle=style, shapeRadius=r})
                         }
          "Polygon" -> interior e $
                       do{ style <- parseContents
                         ; p <- inElement "perimeter" $ many1 parseContents
                         ; return (Polygon {shapeStyle=style, shapePerimeter=p})
                         }
          "Lines" -> interior e $
                       do{ style <- parseContents
                         ; p <- inElement "perimeter" $ many1 parseContents
                         ; return (Lines {shapeStyle=style, shapePerimeter=p})
                         }
          "Composite" -> interior e $ do{ ss <- many1 parseContents
                                        ; return (Composite {shapeSegments=ss})
                                        }
      }

instance HTypeable ShapeStyle where
  toHType s = Defined "ShapeStyle" [] [Constr "ShapeStyle" [] []]
instance XmlContent ShapeStyle where
  toContents s =
      [ mkElemC "ShapeStyle"
          [ mkElemC "StrokeWidth" (toContents (styleStrokeWidth s))
          , mkElemC "StrokeColour" (toContents (styleStrokeColour s))
          , mkElemC "Fill" (toContents (styleFill s))
          ]
      ]
  parseContents = inElement "ShapeStyle" $ do
      { w <- inElement "StrokeWidth" parseContents
      ; c <- inElement "StrokeColour" parseContents
      ; f <- inElement "Fill" parseContents
      ; return (ShapeStyle { styleStrokeWidth=w, styleStrokeColour=c
                           , styleFill=f })
      }
-}

logicalDraw :: Size -> DC () -> DoublePoint -> Shape -> [WX.Prop (DC ())] -> IO ()
logicalDraw ppi dc centrePoint shape options =
    case shape of
      Circle {}   -> WX.circle dc (logicalToScreenPoint ppi centrePoint)
                                  (logicalToScreenX ppi (shapeRadius shape))
                                  (style2options (shapeStyle shape)++options)
      Polygon {}  -> WX.polygon dc (map (logicalToScreenPoint ppi
                                             . translate centrePoint)
                                          (shapePerimeter shape))
                                   (style2options (shapeStyle shape)++options)
      Lines {}    -> logicalLineSegments ppi dc (map (translate centrePoint)
                                                     (shapePerimeter shape))
                                   (style2options (shapeStyle shape)++options)
      Composite {}-> mapM_ (\s-> logicalDraw ppi dc centrePoint s options)
                           (shapeSegments shape)

logicalLineSegments :: Size -> DC () -> [DoublePoint] -> [WX.Prop (DC ())] -> IO ()
logicalLineSegments _   _  [_p]                  _options = return ()
logicalLineSegments _   _  [  ]                  _options = return ()
logicalLineSegments ppi dc (fromPoint:toPoint:ps) options =
  do{ WX.line dc (logicalToScreenPoint ppi fromPoint)
              (logicalToScreenPoint ppi toPoint) options
    ; logicalLineSegments ppi dc (toPoint:ps) options
    }

circle :: Shape
circle = Circle  { shapeStyle = defaultShapeStyle
                 , shapeRadius = kNODE_RADIUS }

style2options :: ShapeStyle -> [WX.Prop (DC ())]
style2options sty =
    [ WX.penWidth WX.:= styleStrokeWidth sty
    , WX.penColor WX.:= wxcolor (styleStrokeColour sty)
    , WX.brushKind WX.:= BrushSolid
    , WX.brushColor WX.:= wxcolor (styleFill sty)
    ]

defaultShapeStyle :: ShapeStyle
defaultShapeStyle =
    ShapeStyle	{ styleStrokeWidth = 1
		, styleStrokeColour = licorice
		, styleFill = nodeColor }
