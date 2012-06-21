module Graphics.Blobs.NetworkControl
    ( createNode, selectNode
    , createEdge, selectEdge
    , createVia,  selectVia
    , selectNothing, selectMultiple
    , pickupNode,     dragNode,     dropNode
    , pickupVia,      dragVia,      dropVia
    , pickupMultiple, dragMultiple, dropMultiple
    , pickupArea,     dragArea,     dropArea
    , deleteSelection
    , changeNamePosition
    , changeNodeShape
    , renameNode, reinfoNodeOrEdge, reinfoNodeOrEdgeUser
    , reArityNode
    , levelDownNode, levelUpNode
    , changeGlobalInfo
    ) where

import Graphics.Blobs.State
import Graphics.Blobs.StateUtil
import Graphics.Blobs.Network as Network
import Graphics.Blobs.NetworkView (edgeContains)
import Graphics.Blobs.Document
import Graphics.Blobs.Common
import Graphics.Blobs.CommonIO
import Graphics.Blobs.Math
import qualified Graphics.Blobs.Shape as Shape
import qualified Graphics.Blobs.PersistentDocument as PD
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Palette (shapes)
import Text.Parse
import Data.Char (isSpace)

import Graphics.UI.WX hiding (Selection)

changeNamePosition :: Bool -> State g n e -> IO ()
changeNamePosition above state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; case getSelection doc of
        NodeSelection nodeNr ->
          do{ PD.updateDocument "move label"
                (updateNetwork
                    (updateNode nodeNr
                        (setNameAbove above))) pDoc
            ; repaintAll state
            }
        _ -> return ()
    }

changeNodeShape :: InfoKind n g => String -> n -> State g n e -> IO ()
changeNodeShape shapename info state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; case getSelection doc of
        NodeSelection nodeNr ->
          do{ PD.updateDocument "change shape"
                (updateNetwork
                    (updateNode nodeNr
                        (setInfo info . setShape (Left shapename)))) pDoc
            ; repaintAll state
            }
        _ -> return ()
    }

deleteSelection :: State g n e -> IO ()
deleteSelection state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; case getSelection doc of
        NodeSelection nodeNr ->
          do{ PD.updateDocument "delete node"
                ( setSelection NoSelection
                . updateNetwork (removeNode nodeNr)
                ) pDoc
            ; repaintAll state
            }
        EdgeSelection edgeNr ->
          do{ PD.updateDocument "delete edge"
                ( setSelection NoSelection
                . updateNetwork (removeEdge edgeNr)
                ) pDoc
            ; repaintAll state
            }
        ViaSelection edgeNr viaNr ->
          do{ PD.updateDocument "delete control point"
                ( setSelection NoSelection
                . updateNetwork (removeVia edgeNr viaNr)
                ) pDoc
            ; repaintAll state
            }
        _ -> return ()
    }

createNode :: InfoKind n g => DoublePoint -> State g n e -> IO ()
createNode mousePoint state =
  do{ pDoc <- getDocument state
    ; doc1 <- PD.getDocument pDoc
    ; let (shape,info) = case (shapes . getPalette . getNetwork) doc1 of
                           [] -> (Right Shape.circle, blank)
                           ((s,(_,Nothing)):_) -> (Left s, blank)
                           ((s,(_,Just i)):_)  -> (Left s, i)
    ; let (nodeNr, doc2) = updateNetworkEx addNode doc1
          doc3 = updateNetwork (updateNode nodeNr (setPosition mousePoint
                                                  . setShape shape
                                                  . setInfo info))
                               doc2
          doc4 = setSelection (NodeSelection nodeNr) doc3
    ; PD.setDocument "add node" doc4 pDoc
    ; repaintAll state
    }

selectNothing :: State g n e -> IO ()
selectNothing state =
  do{ pDoc <- getDocument state
    ; PD.superficialUpdateDocument (setSelection NoSelection) pDoc
    ; repaintAll state
    }

selectEdge :: Int -> State g n e -> IO ()
selectEdge edgeNr state =
  do{ pDoc <- getDocument state
    ; PD.superficialUpdateDocument (setSelection (EdgeSelection edgeNr)) pDoc
    ; repaintAll state
    }

createEdge :: (InfoKind e g) => Int -> Int -> State g n e -> IO ()
createEdge fromNodeNr toNodeNr state =
  do{ pDoc <- getDocument state
    ; PD.updateDocument "add edge"
        ( setSelection (NodeSelection fromNodeNr)
        . updateNetwork (addEdge fromNodeNr toNodeNr)
        ) pDoc
    ; repaintAll state
    }

createVia :: DoublePoint -> State g n e -> IO ()
createVia mousepoint state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
    ; case getSelection doc of
        EdgeSelection edgeNr ->
          do{ ifJust (edgeContains (getEdge edgeNr network) mousepoint network)
                     $ \viaNr->
              do{ PD.updateDocument "add control point to edge"
                    ( setSelection (ViaSelection edgeNr viaNr)
                    . updateNetwork (newViaEdge edgeNr viaNr mousepoint)
                    ) pDoc
                ; repaintAll state
                }
            }
        _ -> return ()
    }

selectVia :: Int -> Int -> State g n e -> IO ()
selectVia edgeNr viaNr state =
  do{ pDoc <- getDocument state
    ; PD.superficialUpdateDocument (setSelection (ViaSelection edgeNr viaNr))
                                   pDoc
    ; repaintAll state
    }

pickupVia :: Int -> Int -> DoublePoint -> State g n e -> IO ()
pickupVia edgeNr viaNr mousePoint state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
          viaPos  = (getEdgeVia (getEdge edgeNr network))!!viaNr
    ; setDragging (Just (False, mousePoint `subtractDoublePoint` viaPos)) state
    ; selectVia edgeNr viaNr state
    }

selectNode :: Int -> State g n e -> IO ()
selectNode nodeNr state =
  do{ pDoc <- getDocument state
    ; PD.superficialUpdateDocument (setSelection (NodeSelection nodeNr)) pDoc
    ; repaintAll state
    }

pickupNode :: Int -> DoublePoint -> State g n e -> IO ()
pickupNode nodeNr mousePoint state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
          nodePos = getNodePosition network nodeNr
    ; setDragging (Just (False, mousePoint `subtractDoublePoint` nodePos)) state
    ; selectNode nodeNr state
    }

dragNode :: Int -> DoublePoint -> ScrolledWindow () -> State g n e -> IO ()
dragNode nodeNr mousePoint canvas state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; Just (hasMoved, offset) <- getDragging state
    ; let newPosition = mousePoint `subtractDoublePoint` offset
          oldPosition = getNodePosition (getNetwork doc) nodeNr
    ; when (newPosition /= oldPosition) $
      do{ -- The first time the node is moved we have to remember
          -- the document in the undo history
        ; (if not hasMoved then PD.updateDocument "move node"
                           else PD.superficialUpdateDocument)
                (updateNetwork (updateNode nodeNr
                    (setPosition newPosition)))
                pDoc
        ; Graphics.UI.WX.repaint canvas
        ; setDragging (Just (True, offset)) state
                -- yes, the node has really moved
        }
    }

dropNode :: Bool -> Int -> DoublePoint -> DoublePoint -> State g n e -> IO ()
dropNode hasMoved nodeNr offset mousePoint state =
  do{ when hasMoved $
      do{ let newPosition = mousePoint `subtractDoublePoint` offset
        ; pDoc <- getDocument state
        ; PD.superficialUpdateDocument
            (updateNetwork (updateNode nodeNr
                (setPosition newPosition))) pDoc
        }
    ; canvas <- getCanvas state
    ; Graphics.UI.WX.repaint canvas
    ; setDragging Nothing state
    }

dragVia :: Int -> Int -> DoublePoint -> ScrolledWindow () -> State g n e -> IO ()
dragVia edgeNr viaNr mousePoint canvas state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; Just (hasMoved, offset) <- getDragging state
    ; let newPosition = mousePoint `subtractDoublePoint` offset
          oldPosition = (getEdgeVia (getEdge edgeNr (getNetwork doc)))!!viaNr
    ; when (newPosition /= oldPosition) $
      do{ -- The first time the point is moved we have to remember
          -- the document in the undo history
        ; (if not hasMoved then PD.updateDocument "move control point"
                           else PD.superficialUpdateDocument)
                (updateNetwork (updateVia edgeNr viaNr newPosition))
                pDoc
        ; Graphics.UI.WX.repaint canvas
        ; setDragging (Just (True, offset)) state
                -- yes, the point has really moved
        }
    }

dropVia :: Bool -> Int -> Int -> DoublePoint -> DoublePoint -> State g n e -> IO ()
dropVia hasMoved edgeNr viaNr offset mousePoint state =
  do{ when hasMoved $
      do{ let newPosition = mousePoint `subtractDoublePoint` offset
        ; pDoc <- getDocument state
        ; PD.superficialUpdateDocument
            (updateNetwork (updateVia edgeNr viaNr newPosition))
            pDoc
        }
    ; canvas <- getCanvas state
    ; Graphics.UI.WX.repaint canvas
    ; setDragging Nothing state
    }

selectMultiple :: Maybe (DoublePoint,DoublePoint) -> [Int] -> [(Int,Int)]
                  -> State g n e -> IO ()
selectMultiple areaRect nodeNrs viaNrs state =
  do{ pDoc <- getDocument state
    ; PD.superficialUpdateDocument
              (setSelection (MultipleSelection areaRect nodeNrs viaNrs))
              pDoc
    ; repaintAll state
    }

pickupMultiple :: [Int] -> [(Int,Int)] -> DoublePoint -> State g n e -> IO ()
pickupMultiple _nodeNrs _viaNrs mousePoint state =
  do{ setDragging (Just (False, mousePoint)) state
--  ; selectMultiple Nothing nodeNrs viaNrs state	-- already selected
    }

dragMultiple :: [Int] -> [(Int,Int)] -> DoublePoint -> ScrolledWindow ()
                -> State g n e -> IO ()
dragMultiple nodeNrs viaNrs mousePoint canvas state =
  do{ pDoc <- getDocument state
 -- ; doc <- PD.getDocument pDoc
    ; Just (hasMoved, originPoint) <- getDragging state
    ; let offset = mousePoint `subtractDoublePoint` originPoint
    ; when (mousePoint /= originPoint) $
      do{ -- The first time the point is moved we have to remember
          -- the document in the undo history
        ; (if not hasMoved then PD.updateDocument "move control point"
                           else PD.superficialUpdateDocument)
                (updateNetwork (updateMultiple nodeNrs viaNrs offset))
                pDoc
        ; Graphics.UI.WX.repaint canvas
        ; setDragging (Just (True, mousePoint)) state
                -- yes, the point has really moved
        }
    }

updateMultiple :: [Int] -> [(Int,Int)] -> DoublePoint -> Network g n e
                                                      -> Network g n e
updateMultiple ns vs o network =
        ( foldr (\n z-> updateNode n (offsetNode o) . z) id ns
        . foldr (\ (e,v) z-> updateVia e v (offsetVia o e v) . z) id vs
        ) network
  where
    offsetNode off node = setPosition (getPosition node `translate` off) node
    offsetVia off edgeNr via = ((getEdgeVia (getEdge edgeNr network))!!via)
                               `translate` off

dropMultiple :: Bool -> [Int] -> [(Int,Int)] -> DoublePoint -> DoublePoint
                -> State g n e -> IO ()
dropMultiple hasMoved nodeNrs viaNrs originPoint mousePoint state =
  do{ when hasMoved $
      do{ pDoc <- getDocument state
        ; PD.superficialUpdateDocument
            (updateNetwork
                (updateMultiple nodeNrs viaNrs
                                (mousePoint`subtractDoublePoint`originPoint)))
            pDoc
        }
    ; canvas <- getCanvas state
    ; Graphics.UI.WX.repaint canvas
    ; setDragging Nothing state
    }

pickupArea :: DoublePoint -> State g n e -> IO ()
pickupArea mousePoint state =
  do{ setDragging (Just (False, mousePoint)) state
    ; selectMultiple (Just (mousePoint,mousePoint)) [] [] state
    }

-- dragArea is not like dragging a selection.  It does not move anything.
-- It only adds items into a multiple selection.
dragArea :: DoublePoint -> State g n e -> IO ()
dragArea mousePoint state =
  do{ pDoc <- getDocument state
    ; doc  <- PD.getDocument pDoc
    ; Just (_, originPoint) <- getDragging state
    ; let (ns,vs) = itemsEnclosedWithin mousePoint originPoint (getNetwork doc)
    ; selectMultiple (Just (originPoint,mousePoint)) ns vs state
    }
  where
    itemsEnclosedWithin p0 p1 network =
        ( ( Prelude.map fst
          . Prelude.filter (\ (_,n)-> enclosedInRectangle (getPosition n) p0 p1)
          . getNodeAssocs ) network
        , ( Prelude.concatMap (\ (i,e)-> map (\ (j,_)-> (i,j))
                                             (Prelude.filter
                                                 (\ (_,v)-> enclosedInRectangle
                                                                        v p0 p1)
                                                 (zip [0..] (getEdgeVia e))))
          . getEdgeAssocs ) network
        )

dropArea :: DoublePoint -> DoublePoint -> State g n e -> IO ()
dropArea _origin mousePoint state =
  do{ dragArea mousePoint state	-- calculate enclosure area
    ; pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; case getSelection doc of
          MultipleSelection _ [] [] ->
              PD.superficialUpdateDocument (setSelection NoSelection) pDoc
          MultipleSelection _ ns vs ->
              PD.superficialUpdateDocument
                  (setSelection (MultipleSelection Nothing ns vs)) pDoc
          _ -> return ()
    ; setDragging Nothing state
    ; repaintAll state
    }


renameNode :: Frame () -> State g n e -> IO ()
renameNode theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
    ; case getSelection doc of
        NodeSelection nodeNr ->
              do{ let oldName = getNodeName network nodeNr
                ; result <- myTextDialog theFrame SingleLine
                                         "Rename node" oldName True
                ; ifJust result $ \newName ->
                      do{ PD.updateDocument "rename node"
                            (updateNetwork
                              (updateNode nodeNr (setName newName))) pDoc
                        ; repaintAll state
                        }
                }
        _ -> return ()
    }

reArityNode :: Frame () -> State g n e -> IO ()
reArityNode theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
    ; case getSelection doc of
        NodeSelection nodeNr ->
              do{ let oldArity = getNodeArity network nodeNr
                ; result <- myTextDialog theFrame SingleLine
                                         "Change arity of node" (show oldArity)
                                         True
                ; ifJust result $ \newArity ->
                    -- do repaintAll state -- Until we sort out the parser
                  case runParser parse newArity of
                    (Right x, s) ->
                        do{ when (not (null s || all isSpace s)) $
                                errorDialog theFrame "Edit warning"
                                      ("Excess text after parsed value."
                                      ++"\nRemaining text: "++s)
                          ; PD.updateDocument "change node arity"
                              (updateNetwork
                                (updateNode nodeNr (setArity x))) pDoc
                          ; repaintAll state
                          }
                    (Left err, s) -> errorDialog theFrame "Edit warning"
                                          ("Cannot parse entered text."
                                          ++"\nReason: "++err
                                          ++"\nRemaining text: "++s)
                }
        _ -> return ()
    }

reinfoNodeOrEdge :: (InfoKind n g, InfoKind e g) =>
                    Frame () -> State g n e -> IO ()
reinfoNodeOrEdge theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
    ; case getSelection doc of
        NodeSelection nodeNr ->
          do{ let oldInfo = getNodeInfo network nodeNr
            ; result <- myTextDialog theFrame MultiLine
                                     "Edit node info" (show oldInfo) True
            ; ifJust result $ \newInfo ->
                  -- do repaintAll state -- Until we sort out the parser
                  case runParser parse newInfo of
                    (Right x, s) ->
                        do{ when (not (null s || all isSpace s)) $
                                errorDialog theFrame "Edit warning"
                                      ("Excess text after parsed value."
                                      ++"\nRemaining text: "++s)
                          ; case check (getNodeName network nodeNr)
                                       (getGlobalInfo network) x of
                              [] -> return ()
                              e  -> errorDialog theFrame "Validity warning"
                                        ("Validity check fails:\n"
                                        ++unlines e)
                          ; PD.updateDocument "edit node info"
                              (updateNetwork
                                (updateNode nodeNr (setInfo x))) pDoc
                          ; repaintAll state
                          }
                    (Left err, s) -> errorDialog theFrame "Edit warning"
                                          ("Cannot parse entered text."
                                          ++"\nReason: "++err
                                          ++"\nRemaining text: "++s)
            }
        EdgeSelection edgeNr ->
          do{ let oldInfo = getEdgeInfo (getEdge edgeNr network)
            ; result <- myTextDialog theFrame MultiLine
                                     "Edit edge info" (show oldInfo) True
            ; ifJust result $ \newInfo ->
                  -- do repaintAll state -- Until we sort out the parser
                  case runParser parse newInfo of
                    (Right x, s) ->
                        do{ when (not (null s || all isSpace s)) $
                                errorDialog theFrame "Edit warning"
                                      ("Excess text after parsed value."
                                      ++"\nRemaining text: "++s)
                          ; case check "edge"
                                       (getGlobalInfo network) x of
                              [] -> return ()
                              e  -> errorDialog theFrame "Validity warning"
                                        ("Validity check fails:\n"
                                        ++unlines e)
                          ; PD.updateDocument "edit edge info"
                              (updateNetwork
                                (updateEdge edgeNr (setEdgeInfo x))) pDoc
                          ; repaintAll state
                          }
                    (Left err, s) -> errorDialog theFrame "Edit warning"
                                          ("Cannot parse entered text."
                                          ++"\nReason: "++err
                                          ++"\nRemaining text: "++s)
            }
        _ -> return ()
    }

reinfoNodeOrEdgeUser :: (InfoKind n g, InfoKind e g) =>
                    Frame () -> State g n e -> IO ()
reinfoNodeOrEdgeUser theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network    = getNetwork doc
          globalInfo = getGlobalInfo network
    ; case getSelection doc of
        NodeSelection nodeNr ->
          do{ let oldInfo = getNodeInfo network nodeNr
            ; result <- editDialogWithGlobal theFrame "Edit node info" oldInfo globalInfo
            -- ; result <- myTextDialog theFrame MultiLine
            --                          "Edit node info" (show oldInfo) True
            ; case result of
              Just newInfo ->
                do {
                  case check (getNodeName network nodeNr)
                       (getGlobalInfo network) newInfo of
                    [] -> return ()
                    e  -> errorDialog theFrame "Validity warning"
                           ("Validity check fails:\n"
                            ++unlines e)
                  ; PD.updateDocument "edit node info"
                        (updateNetwork
                         (updateNode nodeNr (setInfo newInfo))) pDoc
                  ; repaintAll state
                  }
              Nothing ->
                return ()
            }
        EdgeSelection edgeNr ->
          do{ let oldInfo = getEdgeInfo (getEdge edgeNr network)
            ; result <- editDialogWithGlobal theFrame "Edit edge info" oldInfo globalInfo
            ; ifJust result $ \newInfo ->
                       do { case check "edge"
                                       (getGlobalInfo network) newInfo of
                              [] -> return ()
                              e  -> errorDialog theFrame "Validity warning"
                                        ("Validity check fails:\n"
                                        ++unlines e)
                          ; PD.updateDocument "edit edge info"
                              (updateNetwork
                                (updateEdge edgeNr (setEdgeInfo newInfo))) pDoc
                          ; repaintAll state
                          }
            }
              {-
          do{ let oldInfo = getEdgeInfo (getEdge edgeNr network)
            ; result <- myTextDialog theFrame MultiLine
                                     "Edit edge info" (show oldInfo) True
            ; ifJust result $ \newInfo ->
                  -- do repaintAll state -- Until we sort out the parser
                  case runParser parse newInfo of
                    (Right x, s) ->
                        do{ when (not (null s || all isSpace s)) $
                                errorDialog theFrame "Edit warning"
                                      ("Excess text after parsed value."
                                      ++"\nRemaining text: "++s)
                          ; case check "edge"
                                       (getGlobalInfo network) x of
                              [] -> return ()
                              e  -> errorDialog theFrame "Validity warning"
                                        ("Validity check fails:\n"
                                        ++unlines e)
                          ; PD.updateDocument "edit edge info"
                              (updateNetwork
                                (updateEdge edgeNr (setEdgeInfo x))) pDoc
                          ; repaintAll state
                          }
                    (Left err, s) -> errorDialog theFrame "Edit warning"
                                          ("Cannot parse entered text."
                                          ++"\nReason: "++err
                                          ++"\nRemaining text: "++s)
            }
-}
        _ -> return ()
    }

-- ---------------------------------------------------------------------

levelUpNode :: Frame () -> State g n e -> IO ()
levelUpNode theFrame state = undefined

levelDownNode :: Frame () -> State g n e -> IO ()
levelDownNode theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
    ; case getSelection doc of
        NodeSelection nodeNr ->
              do{ let nodeInfo     = getNodeInfo network nodeNr
                      oldNetworkId = "p2"
                      newSel = toNetworkId "p3"
                ; result <- myTextDialog theFrame SingleLine
                                         "Id of child" (show oldNetworkId)
                                         True
                ; ifJust result $ \newSel ->
                     do{
                       let
                          doc2 = setNetworkAndSel newSel (getEmptyNetwork doc) doc
                          -- doc2 = setNetworkAndSel "p3" (getEmptyNetwork doc) doc
                          -- doc2 = setNetwork (getEmptyNetwork doc) doc

                       ; PD.setDocument "change node child Id"
                               doc2 pDoc
                       ; repaintAll state
                       }
                }
        _ -> return ()
    }


-- ---------------------------------------------------------------------

editEdgeInfo :: (Show g, Parse g, Descriptor g) =>
                    Frame () -> String -> g -> IO (Maybe g)
editEdgeInfo theFrame _title info =

  do{ result <- myTextDialog theFrame MultiLine
                "Edit edge info" (show info) True
    ; case result of
        Just newInfo ->
          case runParser parse newInfo of
                    (Right x, s) ->
                        do{ when (not (null s || all isSpace s)) $
                                errorDialog theFrame "Edit warning"
                                      ("Excess text after parsed value."
                                      ++"\nRemaining text: "++s)
                          ; return (Just x)
                          }
                    (Left err, s) -> do { errorDialog theFrame "Edit warning"
                                            ("Cannot parse entered text."
                                            ++"\nReason: "++err
                                            ++"\nRemaining text: "++s)
                                        ; return Nothing
                                        }
        Nothing -> return Nothing

    }

-- ---------------------------------------------------------------------

changeGlobalInfo :: (Show g, Parse g, Descriptor g, GuiEdit g) =>
                    Frame () -> State g n e -> IO ()
changeGlobalInfo theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
          info    = getGlobalInfo network
    -- ; result <- editGlobalInfo theFrame ("Edit "++descriptor info) info
    ; result <- editDialog theFrame ("Edit "++descriptor info) info
    ; ifJust result $ \newInfo->
                  do {
                    PD.updateDocument ("edit "++descriptor info)
                      (updateNetwork (setGlobalInfo newInfo)) pDoc
                  ; repaintAll state	-- no visible change?
                  }
    }

-- ---------------------------------------------------------------------

editGlobalInfo :: (Show g, Parse g, Descriptor g) =>
                    Frame () -> String -> g -> IO (Maybe g)
editGlobalInfo theFrame _title info =
  do{ result <- myTextDialog theFrame MultiLine ("Edit "++descriptor info)
                             (show info) True
    ; case result of
        Just newInfo->
          case runParser parse newInfo of
            (Right x, s) ->
              do{ when (not (null s || all isSpace s)) $
                  errorDialog theFrame "Edit warning"
                              ("Excess text after parsed value."
                              ++"\nRemaining text: "++s)
                ; return (Just x)
                }
            (Left err, s) -> do { errorDialog theFrame "Edit warning"
                                    ("Cannot parse entered text."
                                     ++"\nReason: "++err
                                     ++"\nRemaining text: "++s)
                                ; return Nothing
                                }
        Nothing -> return Nothing
    }

-- EOF
