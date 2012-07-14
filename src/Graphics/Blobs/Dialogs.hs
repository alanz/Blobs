module Graphics.Blobs.Dialogs
       (
         newPageDialog
       ) where

import Data.List(elemIndex)
import Graphics.Blobs.Common(ifJust, internalError, safeIndex)
import Graphics.Blobs.Document
import Graphics.Blobs.SafetyNet
import Graphics.UI.WX
import Graphics.UI.WXCore

-- ---------------------------------------------------------------------

newPageDialog :: Window a -> String -> (String,[PaletteId]) -> Maybe PaletteId
                 -> IO (Maybe (String,PaletteId))
-- newPageDialog  _ _ [] _ =
--     internalError "Dialogs" "newPageDialog" "no palettes"
newPageDialog parentWindow dialogTitle (name,strings) initialSelection =
  do{ d <- dialog parentWindow [ text := dialogTitle, resizeable := True ]
    ; p <- panel d []
    ; theListBox <- singleListBox p [ items := strings, selection := 0]
    ; ifJust initialSelection $ \selString ->
        case elemIndex selString strings of
            Nothing -> internalError "Dialogs" "newPageDialog"
                            (  "initial selection " ++ show selString
                            ++ " can not be found in " ++ show strings )
            Just i -> set theListBox [ selection := i ]
    ; textInput <- textEntry p [ alignment := AlignLeft, text := name ]
    ; ok    <- button p [text := "Ok"]
    ; can   <- button p [text := "Cancel", identity := wxID_CANCEL]
    ; buttonSetDefault ok
    ; set d [ layout := container p $
                        column 10 [ vfill $ widget theListBox
                                  , hfill $ widget textInput
                                  , row 5 [widget ok, widget can]
                                  ]
            , clientSize := sz 300 400
            ]
    ; showModal d $ \stop1 ->
                do set ok  [on command := safetyNet parentWindow $
                                          do index   <- get theListBox selection
                                             theText <- get textInput text
                                             stop1 (Just (theText, (safeIndex "Dialogs.newPageDialog" strings index)))]
                   set can [on command := safetyNet parentWindow $
                                          stop1 Nothing]
    }


