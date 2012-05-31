module State
    ( State
    , State.empty
    , ToolWindow(..)

    , getDocument
    , getDragging,          setDragging
    , getCanvas,            setCanvas
    , getNetworkFrame,      setNetworkFrame
    , getPageSetupDialog,   setPageSetupDialog
    , getDisplayOptions,    setDisplayOptions
    , changeDisplayOptions
    ) where

import Document
import Math
import qualified PersistentDocument as PD
import DisplayOptions

import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Document, ToolWindow)

type State g n e = Var (StateRecord g n e)

data StateRecord g n e = St
    { stDocument        :: PD.PersistentDocument (Document g n e)
    , stDragging        :: Maybe (Bool, DoublePoint) -- ^ (really moved?, offset from center of node)
    , stNetworkFrame    :: Frame ()
    , stCanvas          :: ScrolledWindow ()
    , stPageSetupDialog :: PageSetupDialog ()
    , stDisplayOptions  :: DisplayOptions
    }

data ToolWindow = TW
    { twRepaint :: IO ()
    , twFrame   :: Frame ()
    }

empty :: IO (State g n e)
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

getDocument :: State g n e -> IO (PD.PersistentDocument (Document g n e))
getDocument = getFromState stDocument

getDragging :: State g n e -> IO (Maybe (Bool, DoublePoint))
getDragging = getFromState stDragging

getNetworkFrame :: State g n e -> IO (Frame ())
getNetworkFrame = getFromState stNetworkFrame

getCanvas :: State g n e -> IO (ScrolledWindow ())
getCanvas = getFromState stCanvas

getPageSetupDialog :: State g n e -> IO (PageSetupDialog ())
getPageSetupDialog = getFromState stPageSetupDialog

getDisplayOptions :: State g n e -> IO DisplayOptions
getDisplayOptions = getFromState stDisplayOptions

-- Setters

setDragging :: Maybe (Bool, DoublePoint)  -> State g n e -> IO ()
setDragging theDragging stateRef =
    varUpdate_ stateRef (\state -> state { stDragging = theDragging })

setNetworkFrame :: Frame () -> State g n e -> IO ()
setNetworkFrame networkFrame stateRef =
    varUpdate_ stateRef (\state -> state { stNetworkFrame = networkFrame })

setCanvas :: ScrolledWindow () -> State g n e -> IO ()
setCanvas canvas stateRef =
    varUpdate_ stateRef (\state -> state { stCanvas = canvas })

setPageSetupDialog :: PageSetupDialog () -> State g n e -> IO ()
setPageSetupDialog thePageSetupDialog stateRef =
    varUpdate_ stateRef (\state -> state { stPageSetupDialog = thePageSetupDialog })

setDisplayOptions :: DisplayOptions -> State g n e -> IO ()
setDisplayOptions dp stateRef =
    varUpdate_ stateRef (\state -> state { stDisplayOptions = dp })

changeDisplayOptions :: (DisplayOptions->DisplayOptions) -> State g n e -> IO ()
changeDisplayOptions dpf stateRef =
    varUpdate_ stateRef
        (\state -> state { stDisplayOptions = dpf (stDisplayOptions state) })

-- Utility functions

getFromState :: (StateRecord g n e -> a) -> State g n e -> IO a
getFromState selector stateRef = do
    state <- varGet stateRef
    return (selector state)

varUpdate_ :: Var a -> (a -> a) -> IO ()
varUpdate_ var fun = do { varUpdate var fun; return () }
