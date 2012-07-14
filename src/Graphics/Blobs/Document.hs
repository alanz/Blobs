{-# LANGUAGE TemplateHaskell #-}
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
    , PaletteId
    , toPaletteId
    , empty
    , getNetwork,       setNetwork, unsafeSetNetwork
    , setNetworkAndGlobal
    , getEmptyNetwork
    , getGlobalInfo,    setGlobalInfo
    , getSelection,     setSelection

    , getNetworkSel, setNetworkAndSel, setNetworkSel, getNetworkSelectors

    , getPaletteSel, setPaletteAndSel, setPaletteSel, getPaletteSelectors, setPaletteAssocs

    , getNetworkAssocs, setNetworkAssocs

    , updateNetwork, updateNetworkEx
    ) where

import Data.Aeson.TH
import Graphics.Blobs.InfoKind
import Graphics.Blobs.Math
import qualified Data.Map as Map
import qualified Graphics.Blobs.Network as Network
import qualified Graphics.Blobs.Palette as P


{--------------------------------------------------
 -- TYPES
 --------------------------------------------------}

type NetworkId = String
toNetworkId :: String -> NetworkId
toNetworkId s = s

type PaletteId = String
toPaletteId :: String -> PaletteId
toPaletteId s = s

data Document g n e c = Document
    { docNetwork        :: Map.Map NetworkId (Network.Network g n e c)
    , docNetworkSel     :: NetworkId -- ^Currently selected network
    , docSelection      :: Selection
    , docEmptyNetwork   :: Network.Network g n e c
    , docPalettes       :: Map.Map PaletteId (P.Palette n)
    , docPaletteSel     :: PaletteId -- ^Palette to be used for creating new network
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

deriveJSON id ''Document
deriveJSON id ''Selection

{--------------------------------------------------
 -- CREATION
 --------------------------------------------------}

-- | An empty document
empty :: (InfoKind e g, InfoKind n g, NetworkConfig c) => g -> n -> e -> c -> P.Palette n -> Document g n e c
empty g n e c p =
    Document
    { docNetwork      = Map.fromList [(toNetworkId "p1", Network.empty g n e c p)]
    , docNetworkSel   = toNetworkId "p1"
    , docSelection    = NoSelection
    , docEmptyNetwork = Network.empty g n e c p
    , docPalettes     = Map.fromList [("default", p)]
    , docPaletteSel   = toPaletteId "default"
    , docGlobalInfo   = g
    }

{-
-- | A document with an initial network
initial :: (InfoKind e g, InfoKind n g, NetworkConfig c) => g -> n -> e -> c -> P.Palette n -> Document g n e c
initial g n e c p =
    Document
    { docNetwork      = Map.fromList [(toNetworkId "p1", Network.empty g n e c p)]
    , docNetworkSel   = toNetworkId "p1"
    , docSelection    = NoSelection
    , docEmptyNetwork = Network.empty g n e c p
    , docPalettes = Map.fromList [("default", p)]
    , docGlobalInfo   = g
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
getPaletteSel           :: Document g n e c -> PaletteId
getPaletteSelectors     :: Document g n e c -> [PaletteId]
getPalette              :: Document g n e c -> P.Palette n


getNetwork              doc = (docNetwork doc) Map.! (docNetworkSel doc)
getEmptyNetwork         doc = Network.setPalette (getPalette doc) (docEmptyNetwork doc)
getSelection            doc = docSelection doc
getNetworkSel           doc = docNetworkSel doc
getNetworkSelectors     doc = Map.keys (docNetwork doc)
getGlobalInfo           doc = docGlobalInfo doc

getPaletteSel           doc = docPaletteSel doc
getPaletteSelectors     doc = Map.keys (docPalettes doc)
getPalette              doc = (docPalettes doc) Map.! (docPaletteSel doc)


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
                              False -> Map.insert sel (getEmptyNetwork doc) (docNetwork doc)
        , docNetworkSel = sel
        , docSelection = NoSelection
        }


-- page if it does not currently exist
setPaletteAndSel :: NetworkId -> Network.Network g n e c -> Document g n e c -> Document g n e c
setPaletteAndSel sel theNetwork doc =
    doc { docNetwork   = Map.insert sel theNetwork (docNetwork doc)
        , docNetworkSel = sel
        , docSelection = NoSelection
        }

-- | setPaletteSel sets the palette selector, and clones the currently
-- selected network if the selector does not exist, and clears the selection
setPaletteSel :: PaletteId -> Document g n e c -> Document g n e c
setPaletteSel sel doc =
    doc { docPalettes   = case Map.member sel (docPalettes doc) of
                              True -> (docPalettes doc)
                              False -> Map.insert sel (getPalette doc) (docPalettes doc)
        , docPaletteSel = sel
        }

setPaletteAssocs :: [(PaletteId, P.Palette n)] -> Document g n e c -> Document g n e c
setPaletteAssocs ps doc = doc { docPalettes = Map.fromList ps, docPaletteSel = initialSel }
  where
    (initialSel,_) = head ps



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
