{-| Module      :  Document
    Maintainer  :  afie@cs.uu.nl

    This module contains functions to create documents
    and to get and set components of the Document datatype.
-}

module Graphics.Blobs.Document
    ( Document
    , Selection(..)
    , empty
    , getNetwork,       setNetwork, unsafeSetNetwork
    , getSelection,     setSelection

    , updateNetwork, updateNetworkEx
    ) where

import qualified Graphics.Blobs.Network as Network
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import qualified Data.Map as Map


{--------------------------------------------------
 -- TYPES
 --------------------------------------------------}

type PageId = String
toPageId :: String -> PageId
toPageId s = s

data Document g n e = Document
    { docNetwork        :: Map.Map PageId (Network.Network g n e)
    , docCurrentNetwork :: PageId
    , docSelection      :: Selection
    } deriving Show

data Selection
    = NoSelection
    | NodeSelection Int
    | EdgeSelection Int
    | ViaSelection  Int Int
    | MultipleSelection (Maybe (DoublePoint,DoublePoint)) [Int] [(Int,Int)]
	-- DoublePoint pair is for displaying dragged selection rectangle
    deriving (Show, Read, Eq)

{--------------------------------------------------
 -- CREATION
 --------------------------------------------------}



-- | An empty document
empty :: (InfoKind e g, InfoKind n g) => g -> n -> e -> Document g n e
empty g n e =
    Document
    { docNetwork    = Map.fromList [(toPageId "p1", Network.empty g n e)]
    , docCurrentNetwork = toPageId "p1"
    , docSelection  = NoSelection
    }

{--------------------------------------------------
 -- GETTERS
 --------------------------------------------------}

getNetwork              :: Document g n e -> Network.Network g n e
getSelection            :: Document g n e -> Selection

getNetwork              doc = (docNetwork doc) Map.! (docCurrentNetwork doc)
getSelection            doc = docSelection doc

{--------------------------------------------------
 -- SETTERS
 --------------------------------------------------}

-- | setNetwork clears the selection because the node may not exist
--   in the new network
setNetwork :: Network.Network g n e -> Document g n e -> Document g n e
setNetwork theNetwork doc =
    doc { docNetwork = Map.insert (docCurrentNetwork doc) theNetwork (docNetwork doc)
        , docSelection = NoSelection
        }

setSelection :: Selection -> Document g n e -> Document g n e
setSelection theSelection doc = doc { docSelection = theSelection }

updateNetwork :: (Network.Network g n e -> Network.Network g n e)
                 -> Document g n e -> Document g n e
updateNetwork networkFun doc
    = unsafeSetNetwork (networkFun (getNetwork doc))
    $ doc

updateNetworkEx :: (Network.Network g n e -> (b, Network.Network g n e))
                   -> Document g n e -> (b, Document g n e)
updateNetworkEx networkFun doc =
    let (result, newNetwork) = networkFun (getNetwork doc)
    in ( result
       , unsafeSetNetwork newNetwork doc
       )

-- | Doesn't clear the selection
unsafeSetNetwork :: Network.Network g n e -> Document g n e -> Document g n e
unsafeSetNetwork theNetwork doc = doc { docNetwork = Map.insert (docCurrentNetwork doc) theNetwork (docNetwork doc) }
