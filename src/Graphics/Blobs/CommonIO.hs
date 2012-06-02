module Graphics.Blobs.CommonIO where

import Graphics.Blobs.Math
import Graphics.Blobs.Common(ifJust, internalError, tabDelimited, safeIndex, systemGrey)
import Graphics.Blobs.SafetyNet

import Graphics.UI.WX
import Graphics.UI.WXCore
import List(elemIndex)
import System.Directory
import System.IO

ignoreResult :: IO a -> IO ()
ignoreResult action = do { action; return () }

-- | Writes file to disk. If writing fails, an error
--   dialog is shown and False is returned
safeWriteFile :: Window a -> String -> String -> IO Bool
safeWriteFile parentWindow fileName contents =
  do{ let tmpName = fileName ++ ".tmp"

    ; -- try to write to .tmp file
    ; writeOkay <-
        catch
            (do { writeFile tmpName contents
                ; return True
                })
            (\ioExc ->
              do{ errorDialog parentWindow "Save failed"
                            (  "Saving " ++ fileName ++ " failed.\n\n"
                            ++ "Technical reason: " ++ show ioExc ++ "\n\n"
                            ++ "Tip: do you have write permissions and enough disk space?"
                            )
                ; return False
                }
            )
    ; if not writeOkay then
        return False
      else
  do{ -- remove old file if it exists and then rename .tmp to the real name
    ; catch (do { exists <- doesFileExist fileName
                ; when exists $ removeFile fileName
                ; renameFile tmpName fileName
                ; return True
                })
        (\ioExc ->
          do{ errorDialog parentWindow "Save failed"
                (  "The file has been saved to " ++ show tmpName ++ "\nbut "
                ++ "renaming it to " ++ show fileName ++ " failed.\n\n"
                ++ "Technical reason: " ++ show ioExc
                )
            ; return False
            }
        )
    }}

strictReadFile :: String -> IO String
strictReadFile fname =
  do{ contents <- readFile fname
    ; seq (length contents) $ return contents -- force reading of entire file
    }

data TextCtrlSize = SingleLine | MultiLine

myTextDialog :: Window a -> TextCtrlSize -> String -> String -> Bool
                -> IO (Maybe String)
myTextDialog parentWindow lineSize dialogTitle initial selectAll =
  do{ d <- dialog parentWindow [text := dialogTitle]
    ; textInput <- (case lineSize of SingleLine -> textEntry;
                                     MultiLine  -> textCtrl)
                         d [ alignment := AlignLeft, text := initial ]
    ; ok    <- button d [text := "Ok"]
    ; can   <- button d [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok
    ; set d [layout :=  column 5 [ fill $ widget textInput
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
        --  ,clientSize := case size of SingleLine -> sz 300 40
        --                              MultiLine ->  sz 500 200
            ,area := case lineSize of SingleLine -> rect (pt 50 50) (sz 300 80)
                                      MultiLine ->  rect (pt 50 50) (sz 500 250)
            ]
    ; when (selectAll)     $ do textCtrlSetSelection textInput 0 250
    ; when (not selectAll) $ do textCtrlSetInsertionPointEnd textInput
                                set d [ visible := True ]
    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do theText <- get textInput text
                                             stop1 (Just theText)]
                   set can [on command := safetyNet parentWindow $ stop1 Nothing]
    }

-- Dialog for selecting a multiple Strings (0 or more)
-- Returns Nothing if Cancel was pressed, otherwise it returns the selected strings
multiSelectionDialog :: Window a -> String -> [String] -> [String]
                     -> IO (Maybe [String])
multiSelectionDialog parentWindow dialogTitle strings initialSelection =
  do{ d <- dialog parentWindow
            [ text := dialogTitle
            , resizeable := True
            ]
    ; p <- panel d []
    ; theListBox <- multiListBox p
        [ items := strings
        , selections :=
            [ case maybeIndex of
                Nothing -> internalError "CommonIO" "multiSelectionDialog"
                            (  "initial selection " ++ show s
                            ++ " can not be found in " ++ show strings )
                Just i  -> i
            | s <- initialSelection
            , let maybeIndex = elemIndex s strings
            ]
        ]
    ; selectAll <- button p
        [ text := "Select all"
        , on command := safetyNet parentWindow $ set theListBox [ selections := take (length strings) [0..] ]
        ]
    ; selectNone <- button p
        [ text := "Select none"
        , on command := safetyNet parentWindow $ set theListBox [ selections := [] ]
        ]
    ; ok    <- button p [text := "Ok"]
    ; can   <- button p [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok
    ; set d [ layout := container p $
                        column 10 [ vfill $ widget theListBox
                                  , row 5 [widget selectAll, widget selectNone, widget ok, widget can]
                                  ]
            , clientSize := sz 300 400
            ]
    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                              do indices <- get theListBox selections
                                 stop1 (Just (map (safeIndex "CommonIO.multiSelectionDialog" strings) indices))]
                   set can [on command := safetyNet parentWindow $
                                          stop1 Nothing]
    }

-- Dialog for selecting a single String
-- Returns Nothing if Cancel was pressed, otherwise it returns the selected string
singleSelectionDialog :: Window a -> String -> [String] -> (Maybe String)
                      -> IO (Maybe String)
singleSelectionDialog _ _ [] _ =
    internalError "CommonIO" "singleSelectionDialog" "no strings"
singleSelectionDialog parentWindow dialogTitle strings initialSelection =
  do{ d <- dialog parentWindow [ text := dialogTitle, resizeable := True ]
    ; p <- panel d []
    ; theListBox <- singleListBox p [ items := strings, selection := 0]
    ; ifJust initialSelection $ \selString ->
        case elemIndex selString strings of
            Nothing -> internalError "CommonIO" "singleSelectionDialog"
                            (  "initial selection " ++ show selString
                            ++ " can not be found in " ++ show strings )
            Just i -> set theListBox [ selection := i ]
    ; ok    <- button p [text := "Ok"]
    ; can   <- button p [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok
    ; set d [ layout := container p $
                        column 10 [ vfill $ widget theListBox
                                  , row 5 [widget ok, widget can]
                                  ]
            , clientSize := sz 300 400
            ]
    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do index <- get theListBox selection
                                             stop1 (Just (safeIndex "CommonIO.singleSelectionDialog" strings index))]
                   set can [on command := safetyNet parentWindow $
                                          stop1 Nothing]
    }

-- | Fill a grid from a list of lists of texts. Each list inside the
--   big list represents a row. Also set the given number or rows and
--   columns to be header: grey background and not editable.
--   This function assumes that the normal spreadsheet-like grid header row
--   and column have been made invisible.
fillGridFromList :: Grid () -> Int -> Int -> [[String]] -> IO ()
fillGridFromList _ _ _ [] = return ()
fillGridFromList theGrid nrHeaderRows nrHeaderCols list =
  do{ nrOfCols <- gridGetNumberCols theGrid
    ; nrOfRows <- gridGetNumberRows theGrid
    ; when (length list > nrOfRows || maximum (map length list) > nrOfCols) $
        internalError "Common" "fillGridFromList" "grid is not big enough"
    ; sequence_ . concat $
        [ [   do{ gridSetCellValue theGrid rowNr colNr txt
                ; let isHeaderCell = rowNr < nrHeaderRows || colNr < nrHeaderCols
                ; gridSetCellBackgroundColour theGrid rowNr colNr
                        (if isHeaderCell then systemGrey else white)
                ; gridSetReadOnly theGrid rowNr colNr isHeaderCell
                }
          | (txt, colNr) <- zip theRow [0..]
          ]
        | (theRow, rowNr) <- zip list [0..]
        ]
    }

-- | Export some data (a list of lists of strings) to a tab delimited
--   file. The user is asked to choose a location
exportToTabFile :: Window a -> String -> String -> [[String]] -> IO ()
exportToTabFile parentWindow description fileName theData =
 do { mFilename <- fileSaveDialog
                       parentWindow
                       False -- remember current directory
                       True  -- overwrite prompt
                       ("Export " ++ description)
                       [("Tab delimited files",["*.txt"])]
                       "" -- directory
                       fileName
    ; ifJust mFilename $ \filename ->
            ignoreResult (safeWriteFile parentWindow filename (tabDelimited theData))
    }

getScreenPPI :: IO Size
getScreenPPI =
  do{ dc <- screenDCCreate
    ; s <- dcGetPPI dc
    ; screenDCDelete dc
    ; return s
    }

screenToLogicalPoint :: Size -> Point -> DoublePoint
screenToLogicalPoint ppi p =
    DoublePoint (screenToLogicalX ppi (pointX p))
                (screenToLogicalY ppi (pointY p))

logicalToScreenPoint :: Size -> DoublePoint -> Point
logicalToScreenPoint ppi doublePoint =
    pt (logicalToScreenX ppi (doublePointX doublePoint))
       (logicalToScreenY ppi (doublePointY doublePoint))

screenToLogicalX :: Size ->  Int -> Double
screenToLogicalX ppi x =
    fromIntegral x / (fromIntegral (sizeW ppi) / 2.54)

logicalToScreenX :: Size -> Double -> Int
logicalToScreenX ppi x =
    truncate (x * fromIntegral (sizeW ppi) / 2.54)

screenToLogicalY :: Size -> Int -> Double
screenToLogicalY ppi y =
    fromIntegral y / (fromIntegral (sizeH ppi) / 2.54)

logicalToScreenY :: Size -> Double -> Int
logicalToScreenY ppi y =
    truncate (y * fromIntegral (sizeH ppi) / 2.54)

-- Create a grid of which the standard labels (A,B,C... for columns
-- and 1,2,3... for rows) are invisible
mkNoLabelGrid :: Window a -> Int -> Int -> IO (Grid ())
mkNoLabelGrid thePanel nrOfRows nrOfCols =
  do{ theGrid <- gridCreate thePanel idAny rectNull 0
    ; gridCreateGrid theGrid nrOfRows nrOfCols 0
    ; gridSetColLabelSize theGrid 0
    ; gridSetRowLabelSize theGrid 0
    ; return theGrid
    }

resizeGrid :: Grid () -> Int -> Int -> IO ()
resizeGrid theGrid nrOfRows nrOfCols =
  do{ oldNrOfRows <- gridGetNumberRows theGrid
    ; oldNrOfCols <- gridGetNumberCols theGrid
    ; when (nrOfRows > oldNrOfRows) . ignoreResult $
        gridAppendRows theGrid (nrOfRows - oldNrOfRows) False
    ; when (nrOfRows < oldNrOfRows) . ignoreResult $
        gridDeleteRows theGrid nrOfRows (oldNrOfRows - nrOfRows) False
    ; when (nrOfCols > oldNrOfCols) . ignoreResult $
        gridAppendCols theGrid (nrOfCols - oldNrOfCols) False
    ; when (nrOfCols < oldNrOfCols) . ignoreResult $
        gridDeleteCols theGrid nrOfCols (oldNrOfCols - nrOfCols) False
    }

-- | Get the position of a frame, if the frame is minimized or maximized
--   it is restored to its normal size first. Otherwise, you get
--   (-32000, -32000) for a minimized window :-)
safeGetPosition :: Frame a -> IO (Int, Int)
safeGetPosition f =
  do{ -- isMax <- frameIsMaximized f
      isMax <- frameIsFullScreen f
    -- ; isMin <- frameIsIconized  f
    -- ; when (isMax || isMin) $ frameRestore f
    ; when (isMax) $ frameRestore f
    ; p <- get f position
    ; return (pointX p, pointY p)
    }

-- Show a dialog with a grid and a save button
gridDialogWithSave :: Window a -> String -> Maybe String -> [[String]]
                   -> IO () -> IO ()
gridDialogWithSave parentWindow title maybeNote matrixContents saveAction =
  do{
    -- Create dialog and panel
    ; theDialog <- dialog parentWindow
        [ text := title
        , resizeable := True
        ]
    ; p <- panel theDialog []

    -- Create and fill grid
    ; theGrid <- mkNoLabelGrid p height width
    ; gridEnableEditing theGrid False
    ; fillGridFromList theGrid 0 0 matrixContents
    ; gridAutoSizeColumns theGrid False

    -- File menu
    ; saveButton <- button p
        [ text := "Save as..."
        , on command := safetyNet parentWindow $ saveAction
        ]

    -- Dialog layout
    ; set theDialog
        [ layout := minsize (sz 600 400) $ column 5
                    ( case maybeNote of
                        Just note -> [ hfill $ label note ]
                        Nothing   -> []
                    ++ [ container p $
                            column 5 [ fill $ widget theGrid
                                     , row 0 [ widget saveButton, glue ]
                                     ]
                       ]
                    )
        , visible := True
        ]
    }
 where
    width  = maximum . map length $ matrixContents
    height = length matrixContents


-- | Using bootstrapUI, a record containing all widgets and variables can be created
-- at the end of the create function, but still referred to before creation
-- NOTE: widgets should not be referred to in a strict way because this will
-- cause a loop
bootstrapUI :: (uistate -> IO uistate) -> IO ()
bootstrapUI fIO =
 do { fixIO fIO
    ; return ()
    }
