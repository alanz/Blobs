{-| Module      :  Document
    Maintainer  :  afie@cs.uu.nl

    This module contains functions to create documents
    and to get and set components of the Document datatype.
-}

module Graphics.Blobs.Document
    ( Document
    , Selection(..)
    , NetworkId(..)
    , toNetworkId
    , empty
    , getNetwork,       setNetwork, unsafeSetNetwork
    , getEmptyNetwork
    , getSelection,     setSelection

    , getNetworkSel, setNetworkAndSel, setNetworkSel

    , getNetworkAssocs, setNetworkAssocs

    , updateNetwork, updateNetworkEx
    ) where

import qualified Graphics.Blobs.Network as Network
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import qualified Data.Map as Map


{--------------------------------------------------
 -- TYPES
 --------------------------------------------------}

type NetworkId = String
toNetworkId :: String -> NetworkId
toNetworkId s = s

data Document g n e = Document
    { docNetwork        :: Map.Map NetworkId (Network.Network g n e)
    , docNetworkSel     :: NetworkId -- ^Currently selected network
    , docSelection      :: Selection
    , docEmptyNetwork   :: Network.Network g n e
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
    { docNetwork    = Map.fromList [(toNetworkId "p1", Network.empty g n e)]
    , docNetworkSel = toNetworkId "p1"
    , docSelection  = NoSelection
    , docEmptyNetwork = Network.empty g n e
    }

{--------------------------------------------------
 -- GETTERS
 --------------------------------------------------}

getNetwork              :: Document g n e -> Network.Network g n e
getEmptyNetwork         :: Document g n e -> Network.Network g n e
getSelection            :: Document g n e -> Selection
getNetworkSel           :: Document g n e -> NetworkId

getNetwork              doc = (docNetwork doc) Map.! (docNetworkSel doc)
getEmptyNetwork         doc = docEmptyNetwork doc
getSelection            doc = docSelection doc
getNetworkSel           doc = docNetworkSel doc

-- | Get a list of pairs where each pair contains a network id number and the corresponding network
getNetworkAssocs :: Document g n e -> [(NetworkId,Network.Network g n e)]
getNetworkAssocs doc = Map.assocs (docNetwork doc)

{--------------------------------------------------
 -- SETTERS
 --------------------------------------------------}

setNetworkAssocs :: [(NetworkId, Network.Network g n e)] -> Document g n e -> Document g n e
setNetworkAssocs networkAssocs doc =
    doc { docNetwork   = Map.fromList networkAssocs
        , docNetworkSel = case networkAssocs of
                           [] -> "p1"
                           _  -> fst $ head networkAssocs
        , docSelection = NoSelection
        }


-- | setNetwork clears the selection because the node may not exist
--   in the new network
setNetwork :: Network.Network g n e -> Document g n e -> Document g n e
setNetwork theNetwork doc =
    doc { docNetwork = Map.insert (docNetworkSel doc) theNetwork (docNetwork doc)
        , docSelection = NoSelection
        }

-- | setNetworkAndSel clears the selection, and creates an empty new
-- page if it does not currently exist
setNetworkAndSel :: NetworkId -> Network.Network g n e -> Document g n e -> Document g n e
setNetworkAndSel sel theNetwork doc =
    doc { docNetwork   = Map.insert sel theNetwork (docNetwork doc)
        , docNetworkSel = sel
        , docSelection = NoSelection
        }

-- | setNetworkSel sets the network selector, and clones the currently
-- selected network if the selector does not exist, and clears the selection
setNetworkSel :: NetworkId -> Document g n e -> Document g n e
setNetworkSel sel doc =
    doc { docNetwork   = case Map.member sel (docNetwork doc) of
                              True -> (docNetwork doc)
                              False -> Map.insert sel (docEmptyNetwork doc) (docNetwork doc)
        , docNetworkSel = sel
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
unsafeSetNetwork theNetwork doc = doc { docNetwork = Map.insert (docNetworkSel doc) theNetwork (docNetwork doc) }
