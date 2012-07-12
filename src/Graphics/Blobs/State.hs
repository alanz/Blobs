module Graphics.Blobs.State
    ( State
    , Graphics.Blobs.State.empty
    , ToolWindow(..)

    , getDocument
    , getDragging,          setDragging
    , getCanvas,            setCanvas
    , getNetworkFrame,      setNetworkFrame
    , getPageSetupDialog,   setPageSetupDialog
    , getDisplayOptions,    setDisplayOptions
    , changeDisplayOptions
    ) where

import Graphics.Blobs.Document
import Graphics.Blobs.Math
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Document, ToolWindow)
import qualified Graphics.Blobs.DisplayOptions as DisplayOptions
import qualified Graphics.Blobs.PersistentDocument as PD

-- ---------------------------------------------------------------------

type State g n e c = Var (StateRecord g n e c)

data StateRecord g n e c = St
    { stDocument        :: PD.PersistentDocument (Document g n e c)
    , stDragging        :: Maybe (Bool, DoublePoint) -- ^ (really moved?, offset from center of node)
    , stNetworkFrame    :: Frame ()
    , stCanvas          :: ScrolledWindow ()
    , stPageSetupDialog :: PageSetupDialog ()
    , stDisplayOptions  :: DisplayOptions.DisplayOptions
    }

data ToolWindow = TW
    { twRepaint :: IO ()
    , twFrame   :: Frame ()
    }

empty :: IO (State g n e c)
empty =
  do{ dummy <- PD.dummy

    ; varCreate (St
        { stDocument        = dummy
        , stNetworkFrame    = error "State.empty: network frame has not been set"
        , stDragging        = Nothing
        , stCanvas          = error "State.empty: canvas has not been set"
        , stPageSetupDialog = error "State.empty: page setup dialog has not been set"
        , stDisplayOptions  = DisplayOptions.standard
        })
    }


-- Getters

getDocument :: State g n e c -> IO (PD.PersistentDocument (Document g n e c))
getDocument = getFromState stDocument

getDragging :: State g n e c -> IO (Maybe (Bool, DoublePoint))
getDragging = getFromState stDragging

getNetworkFrame :: State g n e c -> IO (Frame ())
getNetworkFrame = getFromState stNetworkFrame

getCanvas :: State g n e c -> IO (ScrolledWindow ())
getCanvas = getFromState stCanvas

getPageSetupDialog :: State g n e c -> IO (PageSetupDialog ())
getPageSetupDialog = getFromState stPageSetupDialog

getDisplayOptions :: State g n e c -> IO DisplayOptions.DisplayOptions
getDisplayOptions = getFromState stDisplayOptions

-- Setters

setDragging :: Maybe (Bool, DoublePoint)  -> State g n e c -> IO ()
setDragging theDragging stateRef =
    varUpdate_ stateRef (\state -> state { stDragging = theDragging })

setNetworkFrame :: Frame () -> State g n e c -> IO ()
setNetworkFrame networkFrame stateRef =
    varUpdate_ stateRef (\state -> state { stNetworkFrame = networkFrame })

setCanvas :: ScrolledWindow () -> State g n e c -> IO ()
setCanvas canvas stateRef =
    varUpdate_ stateRef (\state -> state { stCanvas = canvas })

setPageSetupDialog :: PageSetupDialog () -> State g n e c -> IO ()
setPageSetupDialog thePageSetupDialog stateRef =
    varUpdate_ stateRef (\state -> state { stPageSetupDialog = thePageSetupDialog })

setDisplayOptions :: DisplayOptions.DisplayOptions -> State g n e c -> IO ()
setDisplayOptions dp stateRef =
    varUpdate_ stateRef (\state -> state { stDisplayOptions = dp })

changeDisplayOptions :: (DisplayOptions.DisplayOptions->DisplayOptions.DisplayOptions) -> State g n e c -> IO ()
changeDisplayOptions dpf stateRef =
    varUpdate_ stateRef
        (\state -> state { stDisplayOptions = dpf (stDisplayOptions state) })

-- Utility functions

getFromState :: (StateRecord g n e c -> a) -> State g n e c -> IO a
getFromState selector stateRef = do
    state <- varGet stateRef
    return (selector state)

varUpdate_ :: Var a -> (a -> a) -> IO ()
varUpdate_ var fun = do { varUpdate var fun; return () }
