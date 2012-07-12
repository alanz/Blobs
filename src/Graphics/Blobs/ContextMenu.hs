module Graphics.Blobs.ContextMenu
    ( canvas, edge, node, via ) where

import Graphics.Blobs.State
import Graphics.Blobs.Network
import Graphics.Blobs.Document
import Graphics.Blobs.NetworkControl
import Graphics.Blobs.SafetyNet
import Graphics.Blobs.CommonIO
import Graphics.Blobs.Math (DoublePoint)
import qualified Graphics.Blobs.PersistentDocument as PD
import Graphics.Blobs.Palette
import Graphics.Blobs.InfoKind
import Text.Parse

import Graphics.UI.WX
import Graphics.UI.WXCore(windowGetMousePosition)

-- | Context menu for empty area of canvas
canvas :: (InfoKind n g, Show g, Parse g, Descriptor g) =>
          Frame () -> State g n e c -> IO ()
canvas theFrame state =
  do{ contextMenu <- menuPane []
    ; menuItem contextMenu
        [ text := "Add node (shift-click)"
        , on command := safetyNet theFrame $ addNodeItem theFrame state
        ]
    -- ; g <- fmap (getGlobalInfo . getNetwork)
    --             (PD.getDocument =<< getDocument state)
    ; g <- fmap (getGlobalInfo)
                (PD.getDocument =<< getDocument state)
    ; menuItem contextMenu
        [ text := ("Edit "++descriptor g)
        , on command := safetyNet theFrame $ changeGlobalInfo theFrame state
        ]

    ; pointWithinWindow <- windowGetMousePosition theFrame
    ; menuPopup contextMenu pointWithinWindow theFrame
    ; objectDelete contextMenu
    }

addNodeItem :: (InfoKind n g) => Frame () -> State g n e c -> IO ()
addNodeItem theFrame state =
  do{ mousePoint <- windowGetMousePosition theFrame
    ; ppi <- getScreenPPI
    ; let doubleMousePoint = screenToLogicalPoint ppi mousePoint
    ; createNode doubleMousePoint state
    }

-- | Context menu for an edge
edge :: (InfoKind n g, InfoKind e g) =>
        EdgeNr -> Frame () -> DoublePoint -> State g n e c -> IO ()
edge edgeNr theFrame mousepoint state =
  do{ contextMenu <- menuPane []

    ; pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network       = getNetwork doc
          theEdge       = getEdge edgeNr network
          fromPort      = getEdgeFromPort theEdge
          toPort        = getEdgeToPort theEdge

    ; menuItem contextMenu
        [ text := "Add control point"
        , on command := safetyNet theFrame $ createVia mousepoint state
        ]
    ; menuItem contextMenu
        [ text := "Delete edge (Del)"
        , on command := safetyNet theFrame $ deleteSelection state
        ]
    ; menuItem contextMenu
        [ text := "Edit info (i)"
        , on command := safetyNet theFrame $ reinfoNodeOrEdge theFrame state
        ]
    ; menuItem contextMenu
        [ text := "Edit info userfunc (u)"
        , on command := safetyNet theFrame $ reinfoNodeOrEdgeUser theFrame state
        ]
    ; menuLine contextMenu
    ; menuItem contextMenu
        [ text := "from port "++show fromPort
        ]
    ; menuItem contextMenu
        [ text := "to port "++show toPort
        ]
    ; pointWithinWindow <- windowGetMousePosition theFrame
    ; menuPopup contextMenu pointWithinWindow theFrame
    ; objectDelete contextMenu
    }

-- | Context menu for a 'via' point
via :: Frame () -> State g n e c -> IO ()
via theFrame state =
  do{ contextMenu <- menuPane []
    ; menuItem contextMenu
        [ text := "Delete control point (Del)"
        , on command := safetyNet theFrame $ deleteSelection state
        ]
    ; pointWithinWindow <- windowGetMousePosition theFrame
    ; menuPopup contextMenu pointWithinWindow theFrame
    ; objectDelete contextMenu
    }

-- | Context menu for a node
node :: (InfoKind n g, InfoKind e g) => Int -> Frame () -> State g n e c -> IO ()
node nodeNr theFrame state =
  do{ contextMenu <- menuPane []

    ; pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network       = getNetwork doc
          theNode       = getNode nodeNr network
          labelAbove    = getNameAbove theNode
          palette       = getPalette network
          theShape      = getShape theNode
          theInfo       = getInfo theNode
          (i,o)         = maybe (0,0) id $ getArity theNode

    ; menuItem contextMenu
        [ text := "Rename (r)"
        , on command := safetyNet theFrame $ renameNode theFrame state
        ]
    ; menuItem contextMenu
        [ text := "Edit info (i)"
        , on command := safetyNet theFrame $ reinfoNodeOrEdge theFrame state
        ]
    ; menuItem contextMenu
        [ text := "Edit info userfunc (u)"
        , on command := safetyNet theFrame $ reinfoNodeOrEdgeUser theFrame state
        ]
    ; menuItem contextMenu
        [ text := "Change arity (in/"++show i++", out/"++show o++")"
        , on command := safetyNet theFrame $ reArityNode theFrame state
        ]
    ; menuItem contextMenu
        [ text := "Delete (Del)"
        , on command := safetyNet theFrame $ deleteSelection state
        ]
    ; menuLine contextMenu

    -- Multi-level/nested/hierarchical editing
    ; menuItem contextMenu
        [ text := "Up"
        , on command := safetyNet theFrame $ levelUpNode theFrame state
        ]

    ; menuItem contextMenu
        [ text := "Down"
        , on command := safetyNet theFrame $ levelDownNode theFrame state
        ]

    ; menuLine contextMenu

    ; menuItem contextMenu
        [ text := "Label position:" ]
    ; menuItem contextMenu
        [ text := "    above (up arrow)"
        , checkable := True
        , checked := labelAbove
        , on command := safetyNet theFrame $ changeNamePosition True state
        ]
    ; menuItem contextMenu
        [ text := "    below (down arrow)"
        , checkable := True
        , checked := not labelAbove
        ,  on command := safetyNet theFrame $ changeNamePosition False state
        ]
--  ; set (if labelAbove then aboveItem else belowItem) [ checked := True ]

    ; menuLine contextMenu

    -- work out whether to keep the info-field whilst changing shape
    -- (change if shape's default; keep if different i.e. user has changed it)
    ; let keepInfo = case theShape of
                        Left n  -> case lookup n (shapes palette) of
                                     Nothing -> const theInfo
                                     Just (_,Nothing) -> const theInfo
                                     Just (_,Just info)  -> if info==theInfo then id
                                                         else const theInfo
                        Right _ -> const theInfo
    ; menuItem contextMenu
        [ text := "Shape:" ]
    ; mapM_ (shapeItem theShape keepInfo contextMenu) (shapes palette)
    ; otherShape theShape contextMenu

    ; pointWithinWindow <- windowGetMousePosition theFrame
    ; menuPopup contextMenu pointWithinWindow theFrame
    ; objectDelete contextMenu

    }
  where
    shapeItem curShape keepInfo contextMenu (name,(_shape,info)) =
      menuItem contextMenu
        [ text := ("    "++name)
        , checkable := True
        , checked := case curShape of { Left n -> n==name; Right _ -> False; }
        , on command := safetyNet theFrame $ changeNodeShape name newinfo state
        ]
        where newinfo = keepInfo (maybe blank id info)
    otherShape curShape contextMenu =
        case curShape of
            Left _  -> return ()
            Right _ -> do{ menuItem contextMenu
                             [ text := "Other shape"
                             , checkable := True
                             , checked := True
                             ]
                         ; return ()
                         }
