module NetworkUI
    ( create
    , getConfig, Config
    ) where

import GUIEvents
import SafetyNet
import State
import StateUtil
import Network
import NetworkView
import NetworkFile
import Document
import Common
import CommonIO
import qualified PersistentDocument as PD
import qualified PDDefaults as PD
import Palette
import InfoKind
import DisplayOptions
import Text.XML.HaXml.XmlContent (XmlContent)
import Text.Parse
import Operations
import NetworkControl (changeGlobalInfo)

import Graphics.UI.WX hiding (Child, upKey, downKey)
import Graphics.UI.WXCore
import Maybe

data Config = NFC
    { nfcWinDimensions  :: (Int, Int, Int, Int) -- x, y, width, height
    , nfcFileName       :: Maybe String
    , nfcSelection      :: Document.Selection
    }
    deriving (Read, Show)

getConfig :: State g n e -> IO Config
getConfig state =
  do{ theFrame      <- getNetworkFrame state
    ; (x, y)        <- safeGetPosition theFrame
    ; winSize       <- get theFrame clientSize
    ; pDoc          <- getDocument state
    ; maybeFileName <- PD.getFileName pDoc
    ; doc <- PD.getDocument pDoc
    ; return (NFC
        { nfcWinDimensions  = (x, y, sizeW winSize, sizeH winSize)
        , nfcFileName       = maybeFileName
        , nfcSelection      = getSelection doc
        })
    }

create :: (InfoKind n g, InfoKind e g
          , {-XmlContent g,-} Parse g, Show g, Descriptor g) =>
          State g n e -> g -> n -> e -> GraphOps g n e -> IO ()
create state g n e ops =
  do{ theFrame <- frame [ text := "Diagram editor"
                        , position      := pt 200 20
                        , clientSize    := sz 300 240 ]
    ; setNetworkFrame theFrame state

    -- Create page setup dialog and save in state
    ; pageSetupData  <- pageSetupDialogDataCreate
    ; initialPageSetupDialog <- pageSetupDialogCreate theFrame pageSetupData
    ; objectDelete pageSetupData
    ; setPageSetupDialog initialPageSetupDialog state

    -- Drawing area
    ; let (width, height) = getCanvasSize (Network.empty g n e)
    ; ppi <- getScreenPPI
    ; canvas <- scrolledWindow theFrame
        [ virtualSize   := sz (logicalToScreenX ppi width)
                              (logicalToScreenY ppi height)
        , scrollRate    := sz 10 10
        , bgcolor       := wxcolor paneBackgroundColor
        , fullRepaintOnResize := False
        ]
    ; State.setCanvas canvas state

    -- Dummy persistent document to pass around
    ; pDoc <- getDocument state

    -- Attach handlers to drawing area
    ; set canvas
        [ on paint :=    \dc _ -> safetyNet theFrame $ paintHandler state dc
        , on mouse :=    \p    -> safetyNet theFrame $
                                      do mouseEvent p canvas theFrame state
                                     --; focusOn canvas
        , on keyboard := \k    -> safetyNet theFrame $
                                      do keyboardEvent theFrame state k
                                     --; focusOn canvas
        ]

    -- File menu
    ; fileMenu   <- menuPane [ text := "&File" ]
    ; menuItem fileMenu
        [ text := "New\tCtrl+N"
        , on command := safetyNet theFrame $ newItem state g n e
        ]
    ; menuItem fileMenu
        [ text := "Open...\tCtrl+O"
        , on command := safetyNet theFrame $ openItem theFrame state
        ]
    ; saveItem <- menuItem fileMenu
        [ text := "Save\tCtrl+S"
        , on command := safetyNet theFrame $ PD.save pDoc
        ]
    ; menuItem fileMenu
        [ text := "Save as..."
        , on command := safetyNet theFrame $ PD.saveAs pDoc
        ]

    ; menuLine fileMenu

    ; menuItem fileMenu
        [ text := "Page setup..."
        , on command := safetyNet theFrame $
              do{ psd <- getPageSetupDialog state
                ; dialogShowModal psd
                ; return ()
                }
        ]

    ; menuItem fileMenu
        [ text := "Print..."
        , on command := safetyNet theFrame $
                let printFun _ printInfo _ dc _ =
                        do { dcSetUserScale dc
                                (fromIntegral (sizeW (printerPPI printInfo))
                                    / fromIntegral (sizeW (screenPPI printInfo)))
                                (fromIntegral (sizeH (printerPPI printInfo))
                                    / fromIntegral (sizeH (screenPPI printInfo)))
                           ; paintHandler state dc
                           }
                    pageFun _ _ _ = (1, 1)
                in
              do{ psd <- getPageSetupDialog state
                ; printDialog psd "Blobs print" pageFun printFun
                }
        ]

    ; menuItem fileMenu
        [ text := "Print preview"
        , on command := safetyNet theFrame $
                let printFun _ _ _ dc _ = paintHandler state dc
                    pageFun _ _ _ = (1, 1)
                in
              do{ psd <- getPageSetupDialog state
                ; printPreview psd "Blobs preview" pageFun printFun
                }
        ]

    ; menuLine fileMenu

    ; menuItem fileMenu
        [ text := "E&xit"
        , on command := close theFrame
        ]

    -- Edit menu
    ; editMenu   <- menuPane [ text := "&Edit" ]
    ; undoItem <- menuItem editMenu
        [ on command := safetyNet theFrame $ do PD.undo pDoc; repaintAll state ]
    ; redoItem <- menuItem editMenu
        [ on command := safetyNet theFrame $ do PD.redo pDoc; repaintAll state ]
    ; menuLine editMenu
    ; menuItem editMenu
        [ text := "Edit "++descriptor g++"..."
        , on command := safetyNet theFrame $ changeGlobalInfo theFrame state
        ]
    ; menuItem editMenu
        [ text := "Change shape palette..."
        , on command := safetyNet theFrame $ openPalette theFrame state
        ]

    -- View menu
    ; viewMenu   <- menuPane [ text := "&View" ]
    ; (DP opts)  <- getDisplayOptions state
    ; menuItem viewMenu
        [ text := descriptor g
        , checkable := True
        , checked := GlobalInfo `elem` opts
        , on command := safetyNet theFrame $ do
                            { changeDisplayOptions (toggle GlobalInfo) state
                            ; repaintAll state } ]
    ; menuItem viewMenu
        [ text := "Node Labels"
        , checkable := True
        , checked := NodeLabel `elem` opts
        , on command := safetyNet theFrame $ do
                            { changeDisplayOptions (toggle NodeLabel) state
                            ; repaintAll state } ]
    ; menuItem viewMenu
        [ text := "Node Info"
        , checkable := True
        , checked := NodeInfo `elem` opts
        , on command := safetyNet theFrame $ do
                            { changeDisplayOptions (toggle NodeInfo) state
                            ; repaintAll state } ]
    ; menuItem viewMenu
        [ text := "Edge Info"
        , checkable := True
        , checked := EdgeInfo `elem` opts
        , on command := safetyNet theFrame $ do
                            { changeDisplayOptions (toggle EdgeInfo) state
                            ; repaintAll state } ]

    -- Operations menu
    ; opsMenu  <- menuPane [ text := "&Operations" ]
    ; mapM_ (\ (name,_)->
               menuItem opsMenu
                   [ text := name
                   , on command := safetyNet theFrame $ do
                                       { callGraphOp name ops state
                                       ; repaintAll state }
                   ]
            ) (ioOps ops)

    ; PD.initialise pDoc (PD.PD
        { PD.document           = Document.empty g n e
        , PD.history            = []
        , PD.future             = []
        , PD.limit              = Nothing
        , PD.fileName           = Nothing
        , PD.dirty              = False
        , PD.saveToDisk         = saveToDisk theFrame
        , PD.updateUndo         = PD.defaultUpdateUndo undoItem
        , PD.updateRedo         = PD.defaultUpdateRedo redoItem
        , PD.updateSave         = PD.defaultUpdateSave saveItem
        , PD.updateTitleBar     = PD.defaultUpdateTitlebar theFrame "Blobs"
        , PD.saveChangesDialog  = PD.defaultSaveChangesDialog theFrame "Blobs"
        , PD.saveAsDialog       = PD.defaultSaveAsDialog theFrame extensions
        })

    -- Layout the main window
    ; set theFrame
        [ menuBar       := [ fileMenu, editMenu, viewMenu, opsMenu ]
        , layout        := minsize (sz 300 240) $ fill $ widget canvas
        , on closing    := safetyNet theFrame $ exit state
        ]

 -- ; set theFrame
 --     [ position      := pt 200 20
 --     , clientSize    := sz 300 240
 --     ]
    }

paintHandler :: (InfoKind n g, InfoKind e g, Descriptor g) =>
                State g n e -> DC () -> IO ()
paintHandler state dc =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; dp <- getDisplayOptions state
    ; drawCanvas doc dc dp
    }

extensions :: [(String, [String])]
extensions = [ ("Blobs files (.blobs)", ["*.blobs"]) ]

mouseEvent :: (InfoKind n g, InfoKind e g, Show g, Parse g, Descriptor g) =>
              EventMouse -> ScrolledWindow () -> Frame () -> State g n e -> IO ()
mouseEvent eventMouse canvas theFrame state = case eventMouse of
    MouseLeftDown mousePoint mods
        | shiftDown mods    -> leftMouseDownWithShift mousePoint state
        | metaDown mods     -> leftMouseDownWithMeta mousePoint state
        | otherwise         -> mouseDown True mousePoint theFrame state
    MouseRightDown mousePoint _ ->
        mouseDown False mousePoint theFrame state
    MouseLeftDrag mousePoint _ ->
        leftMouseDrag mousePoint canvas state
    MouseLeftUp mousePoint _ ->
        leftMouseUp mousePoint state
    _ ->
        return ()

keyboardEvent :: (InfoKind n g, InfoKind e g) =>
                 Frame () -> State g n e -> EventKey -> IO ()
keyboardEvent theFrame state (EventKey theKey _ _) =
    case theKey of
        KeyDelete                       -> deleteKey state
        KeyBack                         -> backspaceKey state
        KeyF2                           -> f2Key theFrame state
        KeyChar 'r'                     -> pressRKey theFrame state
        KeyChar 'i'                     -> pressIKey theFrame state
        KeyUp                           -> upKey state
        KeyDown                         -> downKey state
        _                               -> propagateEvent

closeDocAndThen :: State g n e -> IO () -> IO ()
closeDocAndThen state action =
  do{ pDoc <- getDocument state
    ; continue <- PD.isClosingOkay pDoc
    ; when continue $ action
    }

newItem :: (InfoKind n g, InfoKind e g) => State g n e -> g -> n -> e -> IO ()
newItem state g n e =
    closeDocAndThen state $
      do{ pDoc <- getDocument state
        ; PD.resetDocument Nothing (Document.empty g n e) pDoc
        ; repaintAll state
        }

openItem :: (InfoKind n g, InfoKind e g{-, XmlContent g-}) =>
            Frame () ->  State g n e -> IO ()
openItem theFrame state =
  do{ mbfname <- fileOpenDialog
        theFrame
        False -- change current directory
        True -- allowReadOnly
        "Open File"
        extensions
        "" "" -- no default directory or filename
    ; ifJust mbfname $ \fname -> openNetworkFile fname state (Just theFrame)
    }

-- Third argument: Nothing means exceptions are ignored (used in Configuration)
--              Just f means exceptions are shown in a dialog on top of frame f
openNetworkFile :: (InfoKind n g, InfoKind e g{-, XmlContent g-}) =>
                   String -> State g n e -> Maybe (Frame ()) -> IO ()
openNetworkFile fname state exceptionsFrame =
  closeDocAndThen state $
  flip catch
    (\exc -> case exceptionsFrame of
                Nothing -> return ()
                Just f  -> errorDialog f "Open network"
                    (  "Error while opening '" ++ fname ++ "'. \n\n"
                    ++ "Reason: " ++ show exc)
    ) $
  do{ contents <- strictReadFile fname
    ; let errorOrNetwork = NetworkFile.fromString contents
    ; case errorOrNetwork of {
        Left err -> ioError (userError err);
        Right (network, warnings, oldFormat) ->
  do{ -- "Open" document
    ; let newDoc = setNetwork network (Document.empty undefined undefined undefined)
    ; pDoc <- getDocument state
    ; PD.resetDocument (if null warnings then Just fname else Nothing)
                       newDoc pDoc
    ; applyCanvasSize state
    ; when (not (null warnings)) $
        case exceptionsFrame of
            Nothing -> return ()
            Just f ->
              do{ errorDialog f "File read warnings"
                    (  "Warnings while reading file " ++ show fname ++ ":\n\n"
                    ++ unlines (  map ("* " ++) (take 10 warnings)
                               ++ if length warnings > 10 then ["..."] else []
                               )
                    ++ unlines
                    [ ""
                    , "Most likely you are reading a file that is created by a newer version of Blobs. If you save this file with"
                    , "this version of Blobs information may be lost. For safety the file name is set to \"untitled\" so that you do"
                    , "not accidentaly overwrite the file"
                    ]
                    )
                ; PD.setFileName pDoc Nothing
                }
    ; when oldFormat $
          do{ case exceptionsFrame of
                Nothing -> return ()
                Just f ->
                    errorDialog f "File read warning" $
                       unlines
                       [ "The file you opened has the old Blobs file format which will become obsolete in newer versions of Blobs."
                       , "When you save this network, the new file format will be used. To encourage you to do so the network has"
                       , "been marked as \"modified\"."
                       ]
            ; PD.setDirty pDoc True
            }
    ; -- Redraw
    ; repaintAll state
    }}}

openPalette :: (InfoKind n g, Parse n) => Frame () ->  State g n e -> IO ()
openPalette theFrame state =
  do{ mbfname <- fileOpenDialog
        theFrame
        False -- change current directory
        True -- allowReadOnly
        "Open File"
        [ ("Shape palettes (.blobpalette)", ["*.blobpalette"]) ]
        "" "" -- no default directory or filename
    ; ifJust mbfname $ \fname -> openPaletteFile fname state (Just theFrame)
    }

-- Third argument: Nothing means exceptions are ignored (used in Configuration)
--              Just f means exceptions are shown in a dialog on top of frame f
openPaletteFile :: (InfoKind n g, Parse n) =>
                   String -> State g n e -> Maybe (Frame ()) -> IO ()
openPaletteFile fname state exceptionsFrame =
  flip catch
    (\exc -> case exceptionsFrame of
                Nothing -> return ()
                Just f  -> errorDialog f "Open shape palette"
                    (  "Error while opening '" ++ fname ++ "'. \n\n"
                    ++ "Reason: " ++ show exc)
    ) $
  do{ contents <- readFile fname
    -- ; return () -- Dummy out for now
    ; case fst (runParser parse contents) of {
        Left msg -> ioError (userError ("Cannot parse shape palette file: "
                                       ++fname++"\n\t"++msg));
        Right p  -> do{ pDoc <- getDocument state
                      ;  PD.updateDocument "change palette"
                             (updateNetwork (setPalette p))
				-- really ought to go through network and
				-- change all nodes' stored shape.
                             pDoc
                      }
    }
    }

-- | Get the canvas size from the network and change the size of
--   the widget accordingly
applyCanvasSize :: State g n e -> IO ()
applyCanvasSize state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
          (width, height) = getCanvasSize network
    ; canvas <- getCanvas state
    ; ppi <- getScreenPPI
    ; set canvas [ virtualSize := sz (logicalToScreenX ppi width)
                                     (logicalToScreenY ppi height) ]
    }

saveToDisk :: (InfoKind n g, InfoKind e g{-, XmlContent g-}) =>
              Frame () -> String -> Document.Document g n e -> IO Bool
saveToDisk theFrame fileName doc =
    safeWriteFile theFrame fileName (NetworkFile.toString (getNetwork doc))

exit :: State g n e -> IO ()
exit state =
    closeDocAndThen state $ propagateEvent
