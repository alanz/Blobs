module SafetyNet where

import Graphics.UI.WX hiding (window)
import Prelude hiding (catch)
import Exception

safetyNet :: Window a -> IO b -> IO ()
safetyNet window computation =
  do{ catch
        (do { computation; return () })
        (handler window)
    ; return ()
    }

handler :: Window a -> Exception -> IO ()
handler window exception =
  do{ putStrLn $ "SafetyNet exception: " ++ show exception
    ; errorDialog window "Exception"
        (  "An exception occurred; please report the following text exactly to the makers: \n\n"
        ++ show exception ++ "\n\n"
        ++ "Please save the network under a different name and quit Blobs"
        )
    }
