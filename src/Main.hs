{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main, gain) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Graphics.Blobs.Colors
import Graphics.Blobs.CommonIO
import Graphics.Blobs.Document
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import Graphics.Blobs.Network
import Graphics.Blobs.Operations
import Graphics.Blobs.Shape
import Graphics.UI.WX
import qualified Data.IntMap as IntMap
import qualified Graphics.Blobs.NetworkUI as NetworkUI
import qualified Graphics.Blobs.Palette as P
import qualified Graphics.Blobs.State as State

main :: IO ()
main = start $
  do{ state <- State.empty
    ; NetworkUI.create state ()		-- global state is just the unit value
                             undefined	-- dummy node state (for typechecker)
                             undefined	-- dummy edge state (for typechecker)
                             -- P.empty    -- initial palette
                             undefined  -- initial network config params
                             palettes   -- initial palette
                             graphOps	-- operations available from menu
    }

-- Some basic kinds of info to store in the nodes/edges
instance InfoKind Int () where
    blank = 0
    check n _ i | i<0 = ["Number should not be negative in "++n]
                | otherwise = []
instance InfoKind [Int] () where
    blank = []
    check _ _ _ = []

instance NetworkConfig () where
  prohibitDoubleEdges _ = True
  prohibitReverseEdges _ = False

-- instance Data (Network () [Int] [Int]) where

instance Descriptor [Int] where
  descriptor xs = {- "[Int]=" ++ -} (show xs)

instance Descriptor Int where
  descriptor x = "Int=" ++ (show x)

instance GuiGlobalEdit [Int] () where
  editDialogWithGlobal parentWindow dialogTitle initial _global = aTextDialog parentWindow dialogTitle initial

instance GuiGlobalEdit Int () where
  editDialogWithGlobal parentWindow dialogTitle initial _global = aTextDialog parentWindow dialogTitle initial

-- A simple range of operations on a graph network.
graphOps :: GraphOps () [Int] [Int] ()
graphOps = GraphOps { ioOps = map pureGraphOp
                                  [ ("push numbers one step", onePush)
                                  , ("clear all numbers", revert) ] }
  where
    onePush (g, nodemap, edgemap) =
            (g, IntMap.mapWithKey (\k v-> (edgemap `accumulateIn` k) v) nodemap
              , IntMap.map (\e-> nodemap `pushAlongEdge` e) edgemap)
    revert  (g, nodemap, edgemap) =
            (g, IntMap.map (setInfo blank) nodemap
              , IntMap.map (setEdgeInfo blank) edgemap)

-- Every edge is augmented with the sum of the numbers in its from-node.
pushAlongEdge :: IntMap.IntMap (Node [Int]) -> Edge [Int] -> Edge [Int]
nodemap `pushAlongEdge` edge = setEdgeInfo (nub (sum n: getEdgeInfo edge)) edge
  where n = (getInfo . fromJust . flip IntMap.lookup nodemap . getEdgeFrom)
            edge

-- Every node is augmented with a list of all the numbers in its incoming edges.
accumulateIn :: IntMap.IntMap (Edge [Int]) -> NodeNr -> Node [Int] -> Node [Int]
(edgemap `accumulateIn` nr) node = setInfo (nub (es++getInfo node)) node
  where es = (concat . IntMap.elems
             . IntMap.map getEdgeInfo
             . IntMap.filter (\e-> getEdgeTo e == nr) )
             edgemap

gain :: IO ()
gain = main -- :-)

-- ---------------------------------------------------------------------

palettes :: [(PaletteId,P.Palette [Int])]
palettes = [(toPaletteId "default",simple),(toPaletteId "reduced",reduced)]

simple :: P.Palette [Int]
simple = P.Palette
  [ paletteEntryCircle
  , paletteEntrySquare
  , paletteEntryTriangleLeft
  , paletteEntryTriangleRight
  , paletteEntryWire
  ]

reduced :: P.Palette [Int]
reduced = P.Palette
  [ paletteEntryCircle
  , paletteEntrySquare
  ]

paletteEntryCircle :: (String, (Shape, Maybe [Int]))
paletteEntryCircle =
  ("circle"
    , ( Circle  { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 128
                                        }
              , shapeRadius = 0.5 }
      , Just [] ))

paletteEntrySquare :: (String, (Shape, Maybe [Int]))
paletteEntrySquare =
  ("square"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 2
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 200 128 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5
                                 , DoublePoint (-0.5) 0.5 ] }
      , Just [] ))

paletteEntryTriangleLeft :: (String, (Shape, Maybe [Int]))
paletteEntryTriangleLeft =
  ("triangle left"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) 0
                                 , DoublePoint 0.5 (-0.5)
                                 , DoublePoint 0.5 0.5 ] }
      , Just [] ))

paletteEntryTriangleRight :: (String, (Shape, Maybe [Int]))
paletteEntryTriangleRight =
  ("triangle right"
    , ( Polygon { shapeStyle = ShapeStyle { styleStrokeWidth = 1
                                        , styleStrokeColour = RGB 0 0 0
                                        , styleFill = RGB 128 200 200
                                        }
              , shapePerimeter = [ DoublePoint (-0.5) (-0.5)
                                 , DoublePoint (-0.5) 0.5
                                 , DoublePoint 0.5 0.0 ] }
      , Just [] ))

paletteEntryWire :: (String, (Shape, Maybe [Int]))
paletteEntryWire =
  ("wire"
    , ( Composite { shapeSegments =
                    [ Lines { shapeStyle = ShapeStyle
                                               { styleStrokeWidth = 2
                                               , styleStrokeColour = RGB 0 0 0
                                               , styleFill = RGB 128 128 128
                                               }
                            , shapePerimeter = [ DoublePoint 0.0 (-0.5)
                                               , DoublePoint (-0.2) 0.5 ] }
                    , Lines { shapeStyle = ShapeStyle
                                               { styleStrokeWidth = 2
                                               , styleStrokeColour = RGB 0 0 0
                                               , styleFill = RGB 128 128 128
                                               }
                            , shapePerimeter = [ DoublePoint 0.2 (-0.5)
                                               , DoublePoint 0.0 0.5 ] }
                    ] }
      , Just [] ))



