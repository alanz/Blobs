{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Blobs.Shape
       (
         Shape(..)
       , ShapeStyle(..)
       , circle
       , logicalDraw
       , logicalLineSegments
       ) where

import Data.Data
import Graphics.Blobs.Colors
import Graphics.Blobs.CommonIO
import Graphics.Blobs.Constants
import Graphics.Blobs.Math
import Graphics.UI.WXCore hiding (Colour)
import Text.Parse
import qualified Graphics.UI.WX as WX
import Data.Aeson.TH


data Shape =
    Circle  { shapeStyle :: ShapeStyle, shapeRadius :: Double }
  | Polygon { shapeStyle :: ShapeStyle, shapePerimeter :: [DoublePoint] }
						-- centred on (0,0)
  | Lines   { shapeStyle :: ShapeStyle, shapePerimeter :: [DoublePoint] }
						-- no fill for open shape
  | Composite { shapeSegments :: [Shape] }	-- drawn in given order
  deriving (Eq, Show, Read,Data,Typeable)

data ShapeStyle = ShapeStyle
    { styleStrokeWidth  :: Int
    , styleStrokeColour :: Colour
    , styleFill		:: Colour
    }
  deriving (Eq, Show, Read, Data, Typeable)

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

deriveJSON id ''ShapeStyle
deriveJSON id ''Shape

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

-- ---------------------------------------------------------------------
-- Orphan instances coming home

{- derived by DrIFT -}
{-
instance XML.HTypeable Shape where
    toHType v = XML.Defined "Shape" []
                    [XML.Constr "Circle" [] [XML.toHType aa,XML.toHType ab]
                    ,XML.Constr "Polygon" [] [XML.toHType ac,XML.toHType ad]
                    ,XML.Constr "Lines" [] [XML.toHType ae,XML.toHType af]
                    ,XML.Constr "Composite" [] [XML.toHType ag]]
      where
        (Circle aa ab) = v
        (Polygon ac ad) = v
        (Lines ae af) = v
        (Composite ag) = v
instance XML.XmlContent Shape where
    parseContents = do
        { e@(Elem (N t) _ _) <- XML.element  ["Circle","Polygon","Lines","Composite"]
        ; case t of
          _ | "Polygon" `isPrefixOf` t -> XML.interior e $
                do { ac <- XML.parseContents
                   ; ad <- XML.parseContents
                   ; return (Polygon ac ad)
                   }
            | "Lines" `isPrefixOf` t -> XML.interior e $
                do { ae <- XML.parseContents
                   ; af <- XML.parseContents
                   ; return (Lines ae af)
                   }
            | "Composite" `isPrefixOf` t -> XML.interior e $
                fmap Composite XML.parseContents
            | "Circle" `isPrefixOf` t -> XML.interior e $
                do { aa <- XML.parseContents
                   ; ab <- XML.parseContents
                   ; return (Circle aa ab)
                   }
        }
    toContents v@(Circle aa ab) =
        [XML.mkElemC (XML.showConstr 0 (XML.toHType v)) (concat [XML.toContents aa,
                                                     XML.toContents ab])]
    toContents v@(Polygon ac ad) =
        [XML.mkElemC (XML.showConstr 1 (XML.toHType v)) (concat [XML.toContents ac,
                                                     XML.toContents ad])]
    toContents v@(Lines ae af) =
        [XML.mkElemC (XML.showConstr 2 (XML.toHType v)) (concat [XML.toContents ae,
                                                     XML.toContents af])]
    toContents v@(Composite ag) =
        [XML.mkElemC (XML.showConstr 3 (XML.toHType v)) (XML.toContents ag)]
-}
{- derived by DrIFT -}
{-
instance XML.HTypeable ShapeStyle where
    toHType v = XML.Defined "ShapeStyle" []
                    [XML.Constr "ShapeStyle" [] [XML.toHType aa,XML.toHType ab,XML.toHType ac]]
      where (ShapeStyle aa ab ac) = v
instance XML.XmlContent ShapeStyle where
    parseContents = do
        { XML.inElement  "ShapeStyle" $ do
              { aa <- XML.parseContents
              ; ab <- XML.parseContents
              ; ac <- XML.parseContents
              ; return (ShapeStyle aa ab ac)
              }
        }
    toContents v@(ShapeStyle aa ab ac) =
        [XML.mkElemC (XML.showConstr 0 (XML.toHType v))
                 (concat [XML.toContents aa, XML.toContents ab, XML.toContents ac])]

-}
