{-| Module      :  PersistentDocument
    Author      :  Arjan van IJzendoorn
    License     :  do whatever you like with this

    Maintainer  :  afie@cs.uu.nl

    The persistent document abstraction takes care of dealing
    with a document you want to open from and save to disk and
    that supports undo. This functionality can be used by editors
    of arbitrary documents and saves you a lot of quite subtle
    coding. You only need to initialise a record with things like
    your document, the file name and call-back functions. After
    this, the framework takes care of the hard work. The framework
    is highly parametrisable but there are defaults for many
    parameters.

    The features in detail:
    - unlimited undo & redo buffers (or limited, if you choose to)
    - undo and redo items show what will be undone / redone
        (e.g. "Undo delete node")
    - undo and redo items are disabled if there is nothing to undo or redo
    - maintains a dirty bit that tells you whether the document has
      changed with respect to the version on disk
    - the save menu item can be disabled if the document is not dirty
    - the title bar can be updated to show the program name, the file name
      and whether the document is dirty (shown as "modified")
    - when trying to close the document, the user is asked whether he/she
      wants to save the changes (if needed)
    - handles interaction between saving a document and the dirty bits
      of the document and of the documents in the history and future
    - properly handles Cancel or failure at any stage, e.g. the user
      closes a dirty document with no file name, "Do you want to save
      the changes" dialog is shown, user selects "Save", a Save as
      dialog is opened, user selects a location that happens to be
      read-only, saving fails and the closing of the document is
      cancelled.
-}

module Graphics.Blobs.PersistentDocument
    ( PersistentDocument, PDRecord(..)

    -- , PersistentDocument.dummy
    , dummy
    , initialise
    , resetDocument

    , setDocument, updateDocument
    , superficialSetDocument, superficialUpdateDocument

    , getDocument
    , getFileName, setFileName
    ,              setDirty

    , undo, redo
    , save, saveAs, isClosingOkay
    ) where

--import IOExts(IORef, newIORef, writeIORef, readIORef)
import Data.IORef(IORef, newIORef, writeIORef, readIORef)
import Control.Monad(when)

-- | A persistent document is a mutable variable. This way functions
--   operating on a document do not have to return the new value but
--   simply update it.
type PersistentDocument a = IORef (PDRecord a)

-- | The persistent document record maintains all information needed
--   for undo, redo and file management
data PDRecord a = PD
    { document      :: a

    -- UNDO & REDO
    , history       :: [(String, Bool, a)]
        -- ^ A history item contains a message (what will be undone),
        --   the dirty bit and a copy of the document
    , future        :: [(String, Bool, a)]
        -- ^ See history
    , limit         :: Maybe Int
        -- ^ Maximum number of items of undo history. Or no limit
        --   in the case of Nothing

    -- FILE MANAGEMENT
    , fileName      :: Maybe String
        -- ^ Nothing means no file name yet (untitled)
    , dirty         :: Bool
        -- ^ Has the document changed since saving?

    -- CALL-BACK FUNCTIONS
    , updateUndo    :: Bool -> String -> IO ()
        -- ^ This callback is called when the undo status changes. First parameter
        --   means enable (True) or disable (False). Second parameter is the message
        --   of the first item in the history
    , updateRedo    :: Bool -> String -> IO ()
        -- ^ See updateUndo
    , updateSave    :: Bool -> IO ()
        -- ^ This call-back is called when the save status changes. The boolean
        --   indicates whether save is enabled (dirty document) or disabled (not dirty)
    , updateTitleBar :: Maybe String -> Bool -> IO ()
        -- ^ This call-back is called when the title bar information changes:
        --   file name and modified or not.
    , saveToDisk    :: String -> a -> IO Bool
        -- ^ This callback should actually save the document to disk. It should
        --   return False if saving fails (no permission, disk full...)
    , saveChangesDialog :: IO (Maybe Bool)
        -- ^ This call-back is called when the user should be prompted whether
        --   he/she wants to save the changes or not. Results:
        --   Don't Save -> Just False, Save -> Just True, Cancel -> Nothing
    , saveAsDialog :: Maybe String -> IO (Maybe String)
        -- ^ This call-back is called when the user should specify a
        --   location and a name for the file. The parameter is the current
        --   file name of the document
    }

-- | A dummy persistent document is needed because you need something to pass
--   to the command handlers of menu items BEFORE you can initialse the
--   persistent document with those menu items
dummy :: IO (PersistentDocument a)
dummy = newIORef (error $ "PersistentDocument.empty: call initialise before using "
                        ++ "the persistent document")

-- | Initialise the persistent document with menu items (undo, redo, save),
--   information needed for open & save dialogs, for saving and for updating the
--   title bar
initialise :: PersistentDocument a ->  PDRecord a -> IO ()
initialise pDocRef pDoc =
  do{ writeIORef pDocRef pDoc
    ; updateGUI pDocRef
    }

-- | Clear the document and start with a given document with given file name
--   This function is typically called when you open a new document from disk
--   or start a fresh document that should replace the current document
resetDocument :: Maybe String -> a -> PersistentDocument a -> IO ()
resetDocument theFileName doc pDocRef =
  do{ updateIORef pDocRef (\pDoc -> pDoc
            { document  = doc
            , history   = []
            , future    = []
            , fileName  = theFileName
            , dirty     = False
            })
    ; updateGUI pDocRef
    }

-- | Get the actual document stored within the persistent document
getDocument :: PersistentDocument a -> IO a
getDocument pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; return (document pDoc)
    }

-- | Get the file name stored within the persistent document
getFileName :: PersistentDocument a -> IO (Maybe String)
getFileName pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; return (fileName pDoc)
    }

-- | Get the file name stored within the persistent document
setFileName :: PersistentDocument a -> Maybe String -> IO ()
setFileName pDocRef maybeName =
  do{ pDoc <- readIORef pDocRef
    ; writeIORef pDocRef (pDoc { fileName = maybeName })
    ; updateGUI pDocRef
    }

setDirty :: PersistentDocument a -> Bool -> IO ()
setDirty pDocRef newDirtyBit =
  do{ pDoc <- readIORef pDocRef
    ; writeIORef pDocRef (pDoc { dirty = newDirtyBit })
    ; updateGUI pDocRef
    }

-- | Replace the document inside the persistent document. The current
--   document is remembered in the history list along with the given
--   message. The future list is cleared.
setDocument :: String -> a -> PersistentDocument a -> IO ()
setDocument message newDoc pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; let applyLimit = case limit pDoc of
                        Nothing -> id
                        Just nr -> take nr
          newPDoc =
            pDoc
            { document  = newDoc
            , history   = applyLimit $ (message,dirty pDoc,document pDoc):history pDoc
            , future    = []
            , dirty     = True
            }
    ; writeIORef pDocRef newPDoc
    ; updateGUI pDocRef
    }


-- | Get document, apply function, set document
updateDocument :: String -> (a -> a) -> PersistentDocument a -> IO ()
updateDocument message fun pDocRef =
  do{ doc <- getDocument pDocRef
    ; setDocument message (fun doc) pDocRef
    }

-- | Replace the document without remembering the old document in
--   the history. Superficial updates are useful if something as
--   volatile as a selection is part of your document. If the selection
--   changes you don't want to be able to undo it or to mark
--   the document as dirty
superficialSetDocument :: a -> PersistentDocument a -> IO ()
superficialSetDocument newDoc pDocRef =
    updateIORef pDocRef (\pDoc -> pDoc { document  = newDoc })

-- | Get document, apply function, superficial set document
superficialUpdateDocument :: (a -> a) -> PersistentDocument a -> IO ()
superficialUpdateDocument fun pDocRef =
  do{ doc <- getDocument pDocRef
    ; superficialSetDocument (fun doc) pDocRef
    }

-- | Check whether closing the document is okay. If the document
--   is dirty, the user is asked whether he/she wants to save the
--   changes. Returns False if this process is cancelled or fails
--   at any point.
isClosingOkay :: PersistentDocument a -> IO Bool
isClosingOkay pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; if not (dirty pDoc) then return True else
  do{ result <- saveChangesDialog pDoc
    ; case result of
        Nothing -> return False
        Just True ->
          do{ hasBeenSaved <- save pDocRef
            ; return hasBeenSaved
            }
        Just False -> return True
    }}

-- | Save should be called when "Save" is selected from the file menu.
--   If there is no file name yet, this function acts as if "Save as"
--   was called. It returns False if saving is cancelled or fails.
save :: PersistentDocument a -> IO Bool
save pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; case fileName pDoc of
        Nothing -> saveAs pDocRef
        Just name -> performSave name pDocRef
    }

-- | saveAs should be called when "Save As" is selected from the file menu.
--   A dialog is shown where the user can select a location to save document.
--   This function returns False if saving is cancelled or fails.
saveAs :: PersistentDocument a -> IO Bool
saveAs pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; mbfname <- saveAsDialog pDoc (fileName pDoc)
    ; case mbfname of
        Just fname -> performSave fname pDocRef
        Nothing -> return False
    }


-- | The current document is stored in the future list
--   and the first element of the history list is taken
--   as the new document
undo :: PersistentDocument a -> IO ()
undo pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; when (not (null (history pDoc))) $
  do{ let (msg, newDirty, newDoc) = head (history pDoc)
          newPDoc = pDoc
            { document  = newDoc
            , dirty     = newDirty
            , history   = tail (history pDoc)
            , future    = (msg, dirty pDoc, document pDoc) : future pDoc
            }
    ; writeIORef pDocRef newPDoc
    ; updateGUI pDocRef
    }}

-- | The current document is stored in the history list
--   and the first element of the future list is taken
--   as the new document
redo :: PersistentDocument a -> IO ()
redo pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; when (not (null (future pDoc))) $
  do{ let (msg, newDirty, newDoc) = head (future pDoc)
          newPDoc = pDoc
            { document  = newDoc
            , dirty     = newDirty
            , future    = tail (future pDoc)
            , history   = (msg, dirty pDoc, document pDoc) : history pDoc
            }
    ; writeIORef pDocRef newPDoc
    ; updateGUI pDocRef
    }}

-- FUNCTIONS THAT ARE NOT EXPORTED

updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef var fun = do { x <- readIORef var; writeIORef var (fun x) }

-- Perform the actual save to disk. If this fails False is returned
-- otherwise the file name is set and the dirty bit is cleared. The
-- dirty bits of history and future documents are set.
performSave :: String -> PersistentDocument a -> IO Bool
performSave name pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; hasBeenSaved <- (saveToDisk pDoc) name (document pDoc)
    ; if not hasBeenSaved then return False else
  do{ writeIORef pDocRef (pDoc { fileName = Just name })
    ; updateDirtyBitsOnSave pDocRef
    ; updateGUI pDocRef
    ; return True
    }}

-- updateDirtyBitsOnSave clears the dirty bit for the
-- current document and sets the dirty bits of all
-- documents in history and future lists
updateDirtyBitsOnSave :: PersistentDocument a -> IO ()
updateDirtyBitsOnSave pDocRef =
    updateIORef pDocRef (\pDoc -> pDoc
        { history = map makeDirty (history pDoc)
        , future  = map makeDirty (future  pDoc)
        , dirty   = False
        })
 where
    makeDirty (msg, _, doc) = (msg, True, doc)

-- Shorthand to call all call-backs that update the GUI
updateGUI :: PersistentDocument a -> IO ()
updateGUI pDocRef =
  do{ pDoc <- readIORef pDocRef
    ; case history pDoc of
        []              -> updateUndo pDoc False ""
        ((msg, _, _):_) -> updateUndo pDoc True msg
    ; case future pDoc of
        []              -> updateRedo pDoc False ""
        ((msg, _, _):_) -> updateRedo pDoc True msg
    ; updateSave pDoc (dirty pDoc)
    ; updateTitleBar pDoc (fileName pDoc) (dirty pDoc)
    }
