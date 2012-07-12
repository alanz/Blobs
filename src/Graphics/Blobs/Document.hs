{-| Module      :  Document
    Maintainer  :  afie@cs.uu.nl

    This module contains functions to create documents
    and to get and set components of the Document datatype.
-}

module Graphics.Blobs.Document
    ( Document
    , Selection(..)
    , NetworkId
    , toNetworkId
    , empty
    , getNetwork,       setNetwork, unsafeSetNetwork
    , setNetworkAndGlobal
    , getEmptyNetwork
    , getGlobalInfo,    setGlobalInfo
    , getSelection,     setSelection

    , getNetworkSel, setNetworkAndSel, setNetworkSel, getNetworkSelectors

    , getNetworkAssocs, setNetworkAssocs

    , updateNetwork, updateNetworkEx
    ) where

import qualified Graphics.Blobs.Network as Network
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import qualified Graphics.Blobs.Palette as P
import qualified Data.Map as Map


{--------------------------------------------------
 -- TYPES
 --------------------------------------------------}

type NetworkId = String
toNetworkId :: String -> NetworkId
toNetworkId s = s

data Document g n e c = Document
    { docNetwork        :: Map.Map NetworkId (Network.Network g n e c)
    , docNetworkSel     :: NetworkId -- ^Currently selected network
    , docSelection      :: Selection
    , docEmptyNetwork   :: Network.Network g n e c
    , docGlobalInfo     :: g
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
empty :: (InfoKind e g, InfoKind n g) => g -> n -> e -> c -> P.Palette n -> Document g n e c
empty g n e c p =
    Document
    { docNetwork      = Map.fromList [(toNetworkId "p1", Network.empty g n e c p)]
    , docNetworkSel   = toNetworkId "p1"
    , docSelection    = NoSelection
    , docEmptyNetwork = Network.empty g n e c p
    , docGlobalInfo   = g
    }

-- | A document with an initial network
initial :: (InfoKind e g, InfoKind n g) => g -> n -> e -> c -> P.Palette n -> Document g n e c
initial g n e c p =
    Document
    { docNetwork      = Map.fromList [(toNetworkId "p1", Network.empty g n e c p)]
    , docNetworkSel   = toNetworkId "p1"
    , docSelection    = NoSelection
    , docEmptyNetwork = Network.empty g n e c p
    , docGlobalInfo   = g
    }

{-
-- | An empty document
empty :: (InfoKind e g, InfoKind n g) => g -> n -> e -> Document g n e
empty g n e =
    Document
    { docNetwork    = Map.fromList [(toNetworkId "p1", Network.empty g n e)]
    , docNetworkSel = toNetworkId "p1"
    , docSelection  = NoSelection
    , docEmptyNetwork = Network.empty g n e
    }
-}

{--------------------------------------------------
 -- GETTERS
 --------------------------------------------------}

getNetwork              :: Document g n e c -> Network.Network g n e c
getEmptyNetwork         :: Document g n e c -> Network.Network g n e c
getSelection            :: Document g n e c -> Selection
getNetworkSel           :: Document g n e c -> NetworkId
getNetworkSelectors     :: Document g n e c -> [NetworkId]
getGlobalInfo           :: Document g n e c -> g

getNetwork              doc = (docNetwork doc) Map.! (docNetworkSel doc)
getEmptyNetwork         doc = docEmptyNetwork doc
getSelection            doc = docSelection doc
getNetworkSel           doc = docNetworkSel doc
getNetworkSelectors     doc = Map.keys (docNetwork doc)
getGlobalInfo           doc = docGlobalInfo doc

-- | Get a list of pairs where each pair contains a network id number and the corresponding network
getNetworkAssocs :: Document g n e c -> [(NetworkId,Network.Network g n e c)]
getNetworkAssocs doc = Map.assocs (docNetwork doc)

{--------------------------------------------------
 -- SETTERS
 --------------------------------------------------}

setNetworkAssocs :: [(NetworkId, Network.Network g n e c)] -> Document g n e c -> Document g n e c
setNetworkAssocs networkAssocs doc =
    doc { docNetwork   = Map.fromList networkAssocs
        , docNetworkSel = case networkAssocs of
                           [] -> "p1"
                           _  -> fst $ head networkAssocs
        , docSelection = NoSelection
        }


-- | setNetwork clears the selection because the node may not exist
--   in the new network
setNetwork :: Network.Network g n e c -> Document g n e c -> Document g n e c
setNetwork theNetwork doc =
    doc { docNetwork = Map.insert (docNetworkSel doc) theNetwork (docNetwork doc)
        , docSelection = NoSelection
        }

-- | setNetwork clears the selection because the node may not exist
--   in the new network
setNetworkAndGlobal :: Network.Network g n e c -> g -> Document g n e c -> Document g n e c
setNetworkAndGlobal theNetwork theGlobal doc = doc''
  where
    doc' = setNetwork theNetwork doc
    doc'' = setGlobalInfo theGlobal doc'


-- | setNetworkAndSel clears the selection, and creates an empty new
-- page if it does not currently exist
setNetworkAndSel :: NetworkId -> Network.Network g n e c -> Document g n e c -> Document g n e c
setNetworkAndSel sel theNetwork doc =
    doc { docNetwork   = Map.insert sel theNetwork (docNetwork doc)
        , docNetworkSel = sel
        , docSelection = NoSelection
        }

-- | setNetworkSel sets the network selector, and clones the currently
-- selected network if the selector does not exist, and clears the selection
setNetworkSel :: NetworkId -> Document g n e c -> Document g n e c
setNetworkSel sel doc =
    doc { docNetwork   = case Map.member sel (docNetwork doc) of
                              True -> (docNetwork doc)
                              False -> Map.insert sel (docEmptyNetwork doc) (docNetwork doc)
        , docNetworkSel = sel
        , docSelection = NoSelection
        }


setSelection :: Selection -> Document g n e c -> Document g n e c
setSelection theSelection doc = doc { docSelection = theSelection }


setGlobalInfo :: g -> Document g n e c -> Document g n e c
setGlobalInfo ninfo doc = doc { docGlobalInfo = ninfo }

updateNetwork :: (Network.Network g n e c -> Network.Network g n e c)
                 -> Document g n e c -> Document g n e c
updateNetwork networkFun doc
    = unsafeSetNetwork (networkFun (getNetwork doc))
    $ doc

updateNetworkEx :: (Network.Network g n e c -> (b, Network.Network g n e c))
                   -> Document g n e c -> (b, Document g n e c)
updateNetworkEx networkFun doc =
    let (result, newNetwork) = networkFun (getNetwork doc)
    in ( result
       , unsafeSetNetwork newNetwork doc
       )

-- | Doesn't clear the selection
unsafeSetNetwork :: Network.Network g n e c -> Document g n e c -> Document g n e c
unsafeSetNetwork theNetwork doc = doc { docNetwork = Map.insert (docNetworkSel doc) theNetwork (docNetwork doc) }
