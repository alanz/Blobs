module GUIEvents where

import List (nub,(\\))
import NetworkView(clickedNode, clickedEdge, clickedVia)
import NetworkControl
import State
import Common
import CommonIO
import Document
import qualified ContextMenu
import qualified PersistentDocument as PD
import InfoKind
import Text.ParserCombinators.TextParser

import Graphics.UI.WX
import Graphics.UI.WXCore

mouseDown :: (InfoKind n g, InfoKind e g, Show g, Parse g, Descriptor g) =>
             Bool -> Point -> Frame () -> State g n e -> IO ()
mouseDown leftButton mousePoint theFrame state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; ppi <- getScreenPPI
    ; let network = getNetwork doc
          doubleMousePoint = screenToLogicalPoint ppi mousePoint
    ; case clickedNode doubleMousePoint doc of
        Nothing ->
            case clickedVia doubleMousePoint network of
                Nothing     ->
                    case clickedEdge doubleMousePoint network of
                        Nothing     ->
                            if leftButton then
                                 pickupArea doubleMousePoint state
                            else ContextMenu.canvas theFrame state
                        Just edgeNr ->
                            if leftButton then
                                selectEdge edgeNr state
                            else
                              do{ selectEdge edgeNr state
                                ; ContextMenu.edge edgeNr theFrame doubleMousePoint state
                                }
                Just (edgeNr,viaNr) ->
                    if leftButton then
                        case getSelection doc of
                            MultipleSelection _ ns vs
                              | (edgeNr,viaNr) `elem` vs->
                                 pickupMultiple ns vs doubleMousePoint state
                            _ -> pickupVia edgeNr viaNr doubleMousePoint state
                    else
                      do{ selectVia edgeNr viaNr state
                        ; ContextMenu.via theFrame state
                        }
        Just nodeNr ->
            if leftButton then
                case getSelection doc of
                    MultipleSelection _ ns vs | nodeNr `elem` ns ->
                         pickupMultiple ns vs doubleMousePoint state
                    _ -> pickupNode nodeNr doubleMousePoint state
            else
              do{ selectNode nodeNr state
                ; ContextMenu.node nodeNr theFrame state
                }
    }

leftMouseDownWithShift :: (InfoKind n g, InfoKind e g) =>
                          Point -> State g n e -> IO ()
leftMouseDownWithShift mousePoint state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; ppi <- getScreenPPI
    ; let network = getNetwork doc
          doubleMousePoint = screenToLogicalPoint ppi mousePoint
    ; case clickedNode doubleMousePoint doc of
        Nothing ->
            case clickedEdge doubleMousePoint network of
                Nothing ->
                    -- shift click in empty area = create new node
                    createNode doubleMousePoint state
                Just i ->
                    selectEdge i state -- shift click on edge = select
        Just j -> do -- shift click on node = create edge (if possible)
            case getSelection doc of
                NodeSelection i | i /= j ->
                            createEdge i j state
                _ ->        selectNode j state
    }

leftMouseDownWithMeta :: (InfoKind n g, InfoKind e g) =>
                          Point -> State g n e -> IO ()
leftMouseDownWithMeta mousePoint state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; ppi <- getScreenPPI
    ; let network = getNetwork doc
          doubleMousePoint = screenToLogicalPoint ppi mousePoint
    ; case clickedNode doubleMousePoint doc of
        Just j -> do -- meta click on node = toggle whether node in selection
            case getSelection doc of
                NodeSelection i
                    | i == j -> selectNothing state
                    | i /= j -> selectMultiple Nothing (nub [i,j]) [] state
                ViaSelection e v -> selectMultiple Nothing [j] [(e,v)] state
                MultipleSelection _ ns vs
                    | j `elem` ns -> selectMultiple Nothing (ns\\[j]) vs state
                    | otherwise   -> selectMultiple Nothing (j:ns) vs state
                _ -> selectNode j state
        Nothing ->
            case clickedVia doubleMousePoint network of
                Just via@(e,v) -> -- meta click on via point = toggle inclusion
                  case getSelection doc of
                    NodeSelection i -> selectMultiple Nothing [i] [(e,v)] state
                    ViaSelection e' v'
                        | e==e' && v==v' -> selectNothing state
                        | otherwise -> selectMultiple Nothing [] [via,(e',v')]
                                                                 state
                    MultipleSelection _ ns vs
                        | via `elem` vs -> selectMultiple Nothing ns (vs\\[via])
                                                                     state
                        | otherwise -> selectMultiple Nothing ns (via:vs) state
                    _ -> selectVia e v state
                Nothing -> return ()
    }

leftMouseDrag :: Point -> ScrolledWindow () -> State g n e -> IO ()
leftMouseDrag mousePoint canvas state =
  do{ dragging <- getDragging state
    ; ppi <- getScreenPPI
    ; ifJust dragging $ \_ ->
          do{ pDoc <- getDocument state
            ; doc <- PD.getDocument pDoc
            ; let doubleMousePoint = screenToLogicalPoint ppi mousePoint
            ; case getSelection doc of
                NodeSelection nodeNr ->
                    dragNode nodeNr doubleMousePoint canvas state
                ViaSelection edgeNr viaNr ->
                    dragVia edgeNr viaNr doubleMousePoint canvas state
                MultipleSelection Nothing ns vs ->
                    dragMultiple ns vs doubleMousePoint canvas state
                MultipleSelection _ _ _ ->
                    dragArea doubleMousePoint state
                _ -> return ()
            }
    }

leftMouseUp :: Point -> State g n e -> IO ()
leftMouseUp mousePoint state =
  do{ dragging <- getDragging state
    ; ppi <- getScreenPPI
    ; ifJust dragging $ \(hasMoved, offset) ->
          do{ pDoc <- getDocument state
            ; doc <- PD.getDocument pDoc
            ; let doubleMousePoint = screenToLogicalPoint ppi mousePoint
            ; case getSelection doc of
                NodeSelection nodeNr ->
                    dropNode hasMoved nodeNr offset doubleMousePoint state
                ViaSelection edgeNr viaNr ->
                    dropVia hasMoved edgeNr viaNr offset doubleMousePoint state
                MultipleSelection Nothing ns vs ->
                    dropMultiple hasMoved ns vs offset doubleMousePoint state
                MultipleSelection _ _ _ ->
                    dropArea offset doubleMousePoint state
                _ -> return ()
            }
    }

deleteKey :: State g n e -> IO ()
deleteKey state =
    deleteSelection state

backspaceKey :: State g n e -> IO ()
backspaceKey state =
    deleteSelection state

f2Key :: Frame () -> State g n e -> IO ()		-- due for demolition
f2Key theFrame state =
    renameNode theFrame state

pressRKey :: Frame () -> State g n e -> IO ()
pressRKey theFrame state =
    renameNode theFrame state

pressIKey :: (InfoKind n g, InfoKind e g) => Frame () -> State g n e -> IO ()
pressIKey theFrame state =
    reinfoNodeOrEdge theFrame state

upKey :: State g n e -> IO ()
upKey state =
    changeNamePosition True state

downKey :: State g n e -> IO ()
downKey state =
    changeNamePosition False state
