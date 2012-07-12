module Graphics.Blobs.StateUtil
    ( repaintAll
    , getNetworkName
    ) where

import Graphics.Blobs.State
import Graphics.Blobs.Common
import qualified Graphics.Blobs.PersistentDocument as PD

-- import Maybe
import Graphics.UI.WX

repaintAll :: State g n e c -> IO ()
repaintAll state =
  do{ canvas <- getCanvas state
    ; Graphics.UI.WX.repaint canvas
    }

getNetworkName :: State g n e c -> IO String
getNetworkName state =
 do { pDoc <- getDocument state
    ; mFilename <- PD.getFileName pDoc
    ; case mFilename of
        Just filename -> return $ removeExtension filename
        Nothing       -> return "Untitled"
    }
