module StateUtil
    ( repaintAll
    , getNetworkName
    ) where

import State
import Common
import qualified PersistentDocument as PD

import Maybe
import Graphics.UI.WX

repaintAll :: State g n e -> IO ()
repaintAll state =
  do{ canvas <- getCanvas state
    ; Graphics.UI.WX.repaint canvas
    }

getNetworkName :: State g n e -> IO String
getNetworkName state =
 do { pDoc <- getDocument state
    ; mFilename <- PD.getFileName pDoc
    ; case mFilename of
        Just filename -> return $ removeExtension filename
        Nothing       -> return "Untitled"
    }
