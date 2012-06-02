{-| Module      :  PDDefaults
    Author      :  Arjan van IJzendoorn
    License     :  do whatever you like with this

    Maintainer  :  afie@cs.uu.nl

    Some defaults for the field of the persistent document
    record. For example, the default undo update function
    changes the text of a menu item to reflect what will be
    undo and disables it if there is nothing to be undone.
    You might want more than the defaults if you have a
    more advanced GUI. Let's say you also have a button
    in a toolbar to undo, then you might want to gray out
    that button, too, if there is nothing to be undone.
-}

module Graphics.Blobs.PDDefaults where

import Graphics.UI.WX
import Graphics.UI.WXCore(wxID_CANCEL)

type Extensions = [(String, [String])]

-- Update the menu item "Undo" to show which
-- action will be undone. If there is nothing
-- to undo the corresponding menu item is disabled
defaultUpdateUndo :: MenuItem () -> Bool -> String -> IO ()
defaultUpdateUndo undoItem enable message =
    set undoItem
        [ text := "Undo " ++ message ++ "\tCtrl+Z"
        , enabled := enable
        ]

defaultUpdateRedo :: MenuItem () -> Bool -> String -> IO ()
defaultUpdateRedo redoItem enable message =
    set redoItem
        [ text := "Redo " ++ message ++ "\tCtrl+Y"
        , enabled := enable
        ]

-- Enable the save item only if the document is dirty
defaultUpdateSave :: MenuItem () ->  Bool -> IO ()
defaultUpdateSave saveItem enable =
    set saveItem [ enabled := enable ]

-- Update the title bar: program name - document name followed by "(modified)" if
-- the document is dirty
defaultUpdateTitlebar :: Frame () -> String -> Maybe String -> Bool -> IO ()
defaultUpdateTitlebar theFrame programName theFileName modified =
    let newTitle = programName
                  ++ " - "
                  ++ (case theFileName of Nothing -> "untitled"; Just name -> name)
                  ++ (if modified then " (modified)" else "")
    in set theFrame [ text := newTitle ]

-- | defaultSaveChangesDialog shows a dialog with three buttons with corresponding
--   return values: Don't Save -> Just False, Save -> Just True
--   Cancel -> Nothing
defaultSaveChangesDialog :: Frame () -> String -> IO (Maybe Bool)
defaultSaveChangesDialog parentWindow theProgramName =
  do{ d <- dialog parentWindow [text := theProgramName]
    ; p <- panel d []
    ; msg      <- staticText p [text := "Do you want to save the changes?"]
    ; dontsaveB <- button p [text := "Don't Save"]
    ; saveB     <- button p [text := "Save"]
    ; cancelB   <- button p [text := "Cancel", identity := wxID_CANCEL ]
    ; set d [layout :=  margin 10 $ container p $
                column 10 [ hfill $ widget msg
                          , row 50 [ floatBottomLeft  $ widget dontsaveB
                                   , floatBottomRight $ row 5 [ widget saveB, widget cancelB]
                                   ]
                          ]
            ]
    -- ; set p [ defaultButton := saveB ]
    ; set d [ defaultButton := saveB ]
    ; showModal d $ \stop ->
                do set dontsaveB  [on command := stop (Just False) ]
                   set saveB      [on command := stop (Just True) ]
                   set cancelB    [on command := stop Nothing ]
    }

defaultSaveAsDialog :: Frame () -> Extensions -> Maybe String -> IO (Maybe String)
defaultSaveAsDialog theFrame extensions theFileName =
    fileSaveDialog
        theFrame
        False -- remember current directory
        True -- overwrite prompt
        "Save file"
        extensions
        "" -- directory
        (case theFileName of Nothing -> ""; Just name -> name) -- initial file name

