module Graphics.Blobs.NetworkView
    ( drawCanvas
    , clickedNode
    , clickedEdge
    , clickedVia
    , edgeContains
    ) where

import Graphics.Blobs.Constants
import Graphics.Blobs.CommonIO
import qualified Graphics.Blobs.Network as Network
import Graphics.Blobs.Document
import Graphics.Blobs.Colors
import Graphics.Blobs.Common
import Graphics.Blobs.Palette

import Graphics.Blobs.Math
import Graphics.UI.WX as WX hiding (Vector)
import Graphics.UI.WXCore hiding (Document, screenPPI, Colour)
import Graphics.UI.WXCore.Draw
import Maybe
import qualified Graphics.Blobs.Shape as Shape
import Graphics.Blobs.DisplayOptions
import Graphics.Blobs.InfoKind

import Prelude hiding (catch)
import Control.Exception
import qualified Data.IntMap as IntMap

drawCanvas :: (InfoKind n g, InfoKind e g, Descriptor g) =>
              Document g n e -> DC () -> DisplayOptions -> IO ()
drawCanvas doc dc opt =
  do{

    -- Scale if the DC we are drawing to has a different PPI from the screen
    -- Printing, nudge, nudge
    ; dcPPI <- dcGetPPI dc
    ; screenPPI <- getScreenPPI
    ; when (dcPPI /= screenPPI) $
        dcSetUserScale dc
            (fromIntegral (sizeW dcPPI ) / fromIntegral (sizeW screenPPI ))
            (fromIntegral (sizeH dcPPI ) / fromIntegral (sizeH screenPPI ))

    -- Set font
    ; set dc [ fontFamily := FontDefault, fontSize := 10 ]

    ; catch (reallyDrawCanvas doc screenPPI dc opt)
        (h1 dc dcPPI )
        {-
        (\e -> logicalText dcPPI dc (DoublePoint 50 50)
                           ("Exception while drawing: "++show e)
                           (Justify LeftJ TopJ)  [] )
        -}
    }

h1 :: DC () -> Size2D Int -> SomeException -> IO ()
h1 dc dcPPI e = logicalText dcPPI dc (DoublePoint 50 50)
                   ("Exception while drawing: "++show e)
                   (Justify LeftJ TopJ)  []


reallyDrawCanvas :: (InfoKind n g, InfoKind e g, Descriptor g) =>
                    Document g n e -> Size -> DC () -> DisplayOptions -> IO ()
reallyDrawCanvas doc ppi dc opt =
  do{
    -- draw global info on diagram
    ; let (width, _height) = Network.getCanvasSize network
    ; when (GlobalInfo `elem` dpShowInfo opt) $
           drawLabel 0 False
                     (descriptor global++": "++(unwords.lines.show) global)
                     (DoublePoint (width/2) 1) (Justify CentreJ TopJ)
                     [ textColor := wxcolor kNodeLabelColour ]
    -- draw edges, highlight the selected ones (if any)
    ; mapM_ (\edge -> drawEdge edge []) (Network.getEdges network)
    ; case theSelection of
        EdgeSelection edgeNr -> do
            drawEdge (Network.getEdge edgeNr network) kSELECTED_OPTIONS
        ViaSelection edgeNr viaNr -> do
            drawVia (Network.getEdge edgeNr network) viaNr kSELECTED_OPTIONS
        MultipleSelection _ _ viaNrs -> do
            mapM_ (\ (e,v)-> drawVia (Network.getEdge e network) v kSELECTED_OPTIONS)
                  viaNrs
        _ -> return ()

    -- draw nodes, highlight the selected ones (if any)
    ; mapM_ (\(nodeNr, _) -> drawNode nodeNr [ ]) (Network.getNodeAssocs network)
    ; case theSelection of
        NodeSelection  nodeNr ->
            drawNode nodeNr (kSELECTED_OPTIONS
                            ++ [ penColor := wxcolor activeSelectionColor ])
        MultipleSelection _ nodeNrs _ ->
            mapM_ (\n-> drawNode n (kSELECTED_OPTIONS
                            ++ [ penColor := wxcolor activeSelectionColor ]))
                  nodeNrs
        _ -> return ()

    -- multiple selection drag area rectangle
    ; case theSelection of
        MultipleSelection (Just (p,q)) _ _ ->
                logicalRect ppi dc (doublePointX p) (doublePointY p)
                                   (doublePointX q - doublePointX p)
                                   (doublePointY q - doublePointY p)
                                   [ penColor  := wxcolor lightGrey
                                   , brushKind := BrushTransparent]
        _ -> return ()

    -- canvas size rectangle
 -- ; let (width, height) = Network.getCanvasSize (getNetwork doc)
 -- ; logicalRect ppi dc 0 0 width height [brushKind := BrushTransparent]
    }
  where
    network           = getNetwork doc
    theSelection      = getSelection doc
    (Palette palette) = Network.getPalette network
    global            = Network.getGlobalInfo network

    drawNode :: Int -> [Prop (DC ())] -> IO ()
    drawNode nodeNr options =
      do{
        -- draw node
        ; Shape.logicalDraw ppi dc center shape options
    --  ; logicalCircle ppi dc center kNODE_RADIUS options
	-- draw label
        ; when (NodeLabel `elem` dpShowInfo opt) $
              drawLabel (offset above) False (Network.getName node) center
                        (justif above) [ textColor := wxcolor kNodeLabelColour ]
	-- draw info
        ; when (NodeInfo `elem` dpShowInfo opt) $
              drawLabel (offset (not above)) False (show (Network.getInfo node))
                        center (justif (not above))
                        [ textColor := wxcolor kNodeInfoColour ]
        }
      where
        node   = Network.getNode nodeNr network
        above  = Network.getNameAbove node
        center = Network.getPosition node
        shape  = either (\name-> maybe Shape.circle fst
                                       (Prelude.lookup name palette))
                        id (Network.getShape node)
        offset b = (if b then negate else id) kNODE_RADIUS
        justif b = Justify CentreJ (if b then BottomJ else TopJ)

    drawLabel :: Double -> Bool -> String -> DoublePoint -> Justify
                 -> [Prop (DC ())] -> IO ()
    drawLabel voffset boxed text (DoublePoint x y) justify opts =
      do{ -- draw background
          when boxed $ do
            { (textWidth, textHeight) <- logicalGetTextExtent ppi dc text
            ; let horizontalMargin = 0.2 -- centimeters
                  verticalMargin = 0.01 -- centimeters
                  topleftY = y+voffset - case justify of
                                           Justify _ TopJ    -> 0
                                           Justify _ MiddleJ -> textHeight/2
                                           Justify _ BottomJ -> textHeight

            ; logicalRect ppi dc
                (x - textWidth/2 - horizontalMargin) (topleftY)
                (textWidth+2*horizontalMargin) (textHeight+2*verticalMargin)
                (solidFill labelBackgroundColor)
            }
        -- draw text
        ; logicalText ppi dc (DoublePoint x (y+voffset)) text justify opts
        }

    drawEdge :: InfoKind e g => Network.Edge e -> [Prop (DC ())] -> IO ()
    drawEdge edge options  =
      do{ Shape.logicalLineSegments ppi dc (pt1:via++[pt2]) options
        -- arrow on the end
        ; logicalPoly ppi dc [pt2, tr1, tr2] (options ++ solidFill licorice)
	-- draw info
        ; when (EdgeInfo `elem` dpShowInfo opt) $
           -- logicalTextRotated ppi dc (middle via) (show info) 45
           --           [ textColor := wxcolor kEdgeInfoColour ]
              drawLabel 0 False (show (Network.getEdgeInfo edge)) (middle via)
                        (Justify CentreJ BottomJ)
                        [ textColor := wxcolor kEdgeInfoColour ]
        }
      where
        fromNode   = Network.getNode (Network.getEdgeFrom edge) network
        toNode     = Network.getNode (Network.getEdgeTo   edge) network

        fromPoint  = Network.getPosition fromNode
        toPoint    = Network.getPosition toNode
        via        = Network.getEdgeVia edge

        fstEdgeVector = (head (via++[toPoint]))
                             `subtractDoublePointVector` fromPoint
        fstTotalLen   = vectorLength fstEdgeVector
        fstAngle      = vectorAngle fstEdgeVector

        penultimatePt = head (reverse (fromPoint:via))
        endEdgeVector = toPoint `subtractDoublePointVector` penultimatePt
        endTotalLen   = vectorLength endEdgeVector
        endAngle      = vectorAngle endEdgeVector

        middle []  = DoublePoint ((doublePointX pt1 + doublePointX pt2)/2)
                                 ((doublePointY pt1 + doublePointY pt2)/2)
        middle [p] = p
        middle ps  = middle (tail (reverse ps))

        pt1 = translatePolar fstAngle kNODE_RADIUS fromPoint
        pt2 = translatePolar endAngle (endTotalLen - kNODE_RADIUS) penultimatePt

        tr1 = translatePolar (endAngle + pi + pi / 6) kARROW_SIZE pt2
        tr2 = translatePolar (endAngle + pi - pi / 6) kARROW_SIZE pt2

    drawVia :: Network.Edge e -> Network.ViaNr -> [Prop (DC ())] -> IO ()
    drawVia e n options =
        let pt = (Network.getEdgeVia e)!!n in
        do logicalCircle ppi dc pt kEDGE_CLICK_RANGE
                (options ++ solidFill violet)

solidFill :: Colour -> [Prop (DC ())]
solidFill colour = [ brushKind := BrushSolid, brushColor := wxcolor colour ]

-- | Finds which node of the network is clicked by the mouse, if any
clickedNode :: DoublePoint -> Document g n e -> Maybe Int
clickedNode clickedPoint doc =
    let network = getNetwork doc
        nodeAssocs = case getSelection doc of
                        NodeSelection nodeNr -> [(nodeNr, Network.getNode nodeNr network)]
                        _ -> []
                  ++ reverse (Network.getNodeAssocs network)
    in case filter (\(_, node) -> node `nodeContains` clickedPoint) nodeAssocs of
        [] -> Nothing
        ((i, _):_) -> Just i

nodeContains :: Network.Node n -> DoublePoint -> Bool
nodeContains node clickedPoint =
    distancePointPoint (Network.getPosition node) clickedPoint
      < kNODE_RADIUS

-- | Finds which edge of the network is clicked by the mouse, if any
clickedEdge :: DoublePoint -> Network.Network g n e -> Maybe Int
clickedEdge clickedPoint network =
    let assocs = Network.getEdgeAssocs network
    in case filter (\(_, edge) -> isJust (edgeContains edge clickedPoint network)) assocs of
        [] -> Nothing
        ((i, _):_) -> Just i

edgeContains :: Network.Edge e -> DoublePoint -> Network.Network g n e -> Maybe Int
edgeContains edge clickedPoint network =
    let p0 = Network.getNodePosition network (Network.getEdgeFrom edge)
        p1 = Network.getNodePosition network (Network.getEdgeTo   edge)
        via= Network.getEdgeVia edge
        p  = clickedPoint
        numberedDistancesToSegments = zip [0..] $
              zipWith (\p0 p1-> distanceSegmentPoint p0 p1 p)
                      (p0:via) (via++[p1])
    in case [ nr | (nr,dist) <- numberedDistancesToSegments
                 , dist < kEDGE_CLICK_RANGE ] of
         []  -> Nothing
         nrs -> Just (head nrs)

-- | Finds which 'via' control point is clicked by the mouse, if any
clickedVia :: DoublePoint -> Network.Network g n e -> Maybe (Int,Int)
clickedVia clickedPoint network =
    let allVia = concatMap (\ (k,e)-> zipWith (\n v->((k,n),v))
                                              [0..] (Network.getEdgeVia e))
                           (IntMap.toList (Network.networkEdges network))
    in case filter (\ (_,v)-> distancePointPoint v clickedPoint
                              < kEDGE_CLICK_RANGE) allVia of
        [] -> Nothing
        ((kn,_):_) -> Just kn

-- Drawing operations in logical coordinates

logicalCircle :: Size -> DC () -> DoublePoint -> Double -> [Prop (DC ())] -> IO ()
logicalCircle ppi dc center radius options =
    WX.circle dc (logicalToScreenPoint ppi center) (logicalToScreenX ppi radius) options

logicalRect :: Size -> DC () -> Double -> Double -> Double -> Double -> [Prop (DC ())] -> IO ()
logicalRect ppi dc x y width height options =
    drawRect dc
        (rect
            (pt (logicalToScreenX ppi x)     (logicalToScreenY ppi y))
            (sz (logicalToScreenX ppi width) (logicalToScreenY ppi height)))
        options

data Justify    = Justify Horizontal Vertical	deriving Eq
data Horizontal = LeftJ | CentreJ | RightJ	deriving Eq
data Vertical   = TopJ  | MiddleJ | BottomJ	deriving Eq

-- can deal with multi-line text
logicalText :: Size -> DC () -> DoublePoint -> String -> Justify
               -> [Prop (DC ())] -> IO ()
logicalText ppi dc (DoublePoint x y) txt (Justify horiz vert) options =
  do{ (width,height) <- logicalGetTextExtent ppi dc txt
    ; eachLine width (startPos height) (lines txt)
    }
  where
    startPos height = case vert of TopJ    -> (x, y)
                                   MiddleJ -> (x, y-height/2)
                                   BottomJ -> (x, y-height)
    eachLine _ _ [] = return ()
    eachLine maxwidth (x,y) (txt:txts) =
      do{ (w,h) <- logicalGetTextExtent ppi dc txt
        ; let thisX = case horiz of LeftJ   -> x-maxwidth/2
                                    CentreJ -> x-w/2
                                    RightJ  -> x+(maxwidth/2)-w
        ; drawText dc txt (logicalToScreenPoint ppi (DoublePoint thisX y))
                   options
        ; eachLine maxwidth (x,y+h) txts
        }

-- currently assumes only single line of text
logicalTextRotated :: Size -> DC () -> DoublePoint -> String -> Double
                      -> [Prop (DC ())] -> IO ()
logicalTextRotated ppi dc pos txt angle options =
    draw dc txt (logicalToScreenPoint ppi pos) options
  where
    draw = if angle<1 && angle>(-1) then drawText
           else (\a b c e -> rotatedText a b c angle e)


{-
logicalLine :: Size -> DC () -> DoublePoint -> DoublePoint -> [Prop (DC ())] -> IO ()
logicalLine ppi dc fromPoint toPoint options =
    line dc (logicalToScreenPoint ppi fromPoint)
            (logicalToScreenPoint ppi toPoint) options

logicalLineSegments :: Size -> DC () -> [DoublePoint] -> [Prop (DC ())] -> IO ()
logicalLineSegments _   _  [p]                    options = return ()
logicalLineSegments ppi dc (fromPoint:toPoint:ps) options =
  do{ line dc (logicalToScreenPoint ppi fromPoint)
              (logicalToScreenPoint ppi toPoint) options
    ; logicalLineSegments ppi dc (toPoint:ps) options
    }
-}

logicalPoly :: Size -> DC () -> [DoublePoint] -> [Prop (DC ())] -> IO ()
logicalPoly ppi dc points options =
    polygon dc (map (logicalToScreenPoint ppi) points) options

logicalGetTextExtent :: Size -> DC () -> String -> IO (Double, Double)
logicalGetTextExtent ppi dc txt =
  do{ textSizes <- mapM (getTextExtent dc) (lines txt)
    ; return
        ( screenToLogicalX ppi (maximum (map sizeW textSizes))
        , screenToLogicalY ppi (sum (map sizeH textSizes))
        )
    }
