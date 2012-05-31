module Network
    (
    -- * Types
      Network, Node, Edge
    , NodeNr, EdgeNr, ViaNr
    , networkNodes  -- dangerous
    , networkEdges  -- dangerous

    -- * Creating and printing a network
    , Network.empty
    , dumpNetwork

    , getNodeNrs
    , getNodeAssocs,    setNodeAssocs
    , getEdgeAssocs,    setEdgeAssocs
    , getCanvasSize,    setCanvasSize
    , getPalette,       setPalette
    , getGlobalInfo,    setGlobalInfo

    , getNode
    , getEdge
    , getNodes
    , getEdges
    , getChildren
    , getParents
    , getParentMap, ParentMap

    , nodeExists, edgeExists
    , findEdge, findNodeNrsByName

    , updateNode
    , updateEdge
    , updateVia

    , mapNodeNetwork

    , addNode,      addNodes,    removeNode, addNodeEx
    , addEdge,      addEdges,    removeEdge, addEdgeWithPorts
    , removeAllEdges
    , newViaEdge,   removeVia

    , constructNode
    , getNodeInfo, getNodeName, getNodePosition, getNodeNameAbove, getNodeShape
    , setNodeInfo, setNodeName, setNodePosition, setNodeNameAbove, setNodeShape
    , getNodeArity
    , setNodeArity
    , getInfo, getName, getPosition, getNameAbove, getShape, getArity
    , setInfo, setName, setPosition, setNameAbove, setShape, setArity

    , constructEdge
    , getEdgeFrom, getEdgeTo, getEdgeVia, getEdgeInfo
    , setEdgeFrom, setEdgeTo, setEdgeVia, setEdgeInfo
    , getEdgeFromPort, getEdgeToPort
    , setEdgeFromPort, setEdgeToPort
    ) where

import Common
import Math
import InfoKind
import Shape
import Palette hiding (delete)

import IntMap hiding (map)

data Network g n e = Network
    { networkNodes      :: !(IntMap (Node n)) -- ^ maps node numbers to nodes
    , networkEdges      :: !(IntMap (Edge e)) -- ^ maps edge numbers to edges
    , networkPalette    :: Palette n
    , networkCanvasSize :: (Double, Double)
    , networkInfo       :: g
    } deriving Show

data Edge e = Edge
    { edgeFrom :: !NodeNr -- ^ the number of the node where the edge starts
    , edgeTo   :: !NodeNr -- ^ the number of the node the edge points to
    , edgeVia  :: [DoublePoint] -- ^ intermediate vertices when drawing
    , edgeInfo :: e
    , edgeFromPort :: !PortNr	-- ^ the connection port on the 'from' node
    , edgeToPort   :: !PortNr	-- ^ the connection port on the 'to' node
    } deriving (Show, Read, Eq)

data Node n = Node
    { nodePosition  :: DoublePoint  -- ^ the position of the node on screen
    , nodeName      :: !String
    , nodeNameAbove :: Bool         -- ^ should the name be displayed above (True) of below (False)
    , nodeShape     :: Either String Shape	-- ^ name from palette, or shape
    , nodeInfo      :: n
    , nodeArity     :: Maybe (PortNr,PortNr)	-- ^ number of in/out connection ports
    } deriving (Show, Read)

type NodeNr = Int
type EdgeNr = Int
type ViaNr  = Int
type PortNr = Int

-- | Create an empty network
empty :: (InfoKind n g, InfoKind e g) => g -> n -> e -> Network g n e
empty g _ _ = Network
    { networkNodes      = IntMap.empty
    , networkEdges      = IntMap.empty
    , networkPalette    = Palette.empty
    , networkCanvasSize = (15, 9)
    , networkInfo       = g
    }

-- | Map a function over the nodes, possibly changes the type
--   of the Network (i.e. the kind of values stored in the
--   probability tables)
mapNodeNetwork :: InfoKind m g =>
                  (Node n->Node m) -> Network g n e -> Network g m e
mapNodeNetwork nodeFun network =
    let numberedNodes = getNodeAssocs network
        newNodes = [ (nr, nodeFun node) | (nr, node) <- numberedNodes ]
    in Network
        { networkNodes = IntMap.fromList newNodes
        , networkEdges = networkEdges network
        , networkPalette = fmap (const blank) $ networkPalette network
        , networkCanvasSize = networkCanvasSize network
        , networkInfo = networkInfo network
        }

constructEdge :: NodeNr -> PortNr -> NodeNr -> PortNr
                 -> [DoublePoint] -> e -> Edge e
constructEdge fromNr fromPort toNr toPort via info =
    Edge
        { edgeFrom = fromNr
        , edgeTo   = toNr
        , edgeVia  = via
        , edgeInfo = info
        , edgeFromPort = fromPort
        , edgeToPort   = toPort
        }

getEdgeFrom :: Edge e -> NodeNr
getEdgeFrom = edgeFrom

getEdgeFromPort :: Edge e -> PortNr
getEdgeFromPort = edgeFromPort

getEdgeTo :: Edge e -> NodeNr
getEdgeTo = edgeTo

getEdgeToPort :: Edge e -> PortNr
getEdgeToPort = edgeToPort

getEdgeVia :: Edge e -> [DoublePoint]
getEdgeVia = edgeVia

getEdgeInfo :: Edge e -> e
getEdgeInfo = edgeInfo

setEdgeFrom :: NodeNr -> Edge e -> Edge e
setEdgeFrom fromNr edge = edge { edgeFrom = fromNr }

setEdgeFromPort :: PortNr -> Edge e -> Edge e
setEdgeFromPort fromPortNr edge = edge { edgeFromPort = fromPortNr }

setEdgeTo :: NodeNr -> Edge e -> Edge e
setEdgeTo toNr edge = edge { edgeTo = toNr }

setEdgeToPort :: PortNr -> Edge e -> Edge e
setEdgeToPort toPortNr edge = edge { edgeToPort = toPortNr }

setEdgeVia :: [DoublePoint] -> Edge e -> Edge e
setEdgeVia via edge = edge { edgeVia = via }

setEdgeInfo :: e -> Edge oldInfo -> Edge e
setEdgeInfo info edge = edge { edgeInfo = info }

constructNode :: (InfoKind n g) =>
                 String -> DoublePoint -> Bool
                 -> Either String Shape -> n -> Maybe (PortNr,PortNr) -> Node n
constructNode name position nameAbove shape info arity =
    Node
        { nodeName      = name
        , nodePosition  = position
        , nodeNameAbove = nameAbove
        , nodeShape     = shape
        , nodeInfo      = info
        , nodeArity     = arity
        }

getNodeName :: Network g n e -> NodeNr -> String
getNodeName network nodeNr = nodeName (networkNodes network ! nodeNr)

setNodeName :: NodeNr -> String -> Network g n e -> Network g n e
setNodeName nodeNr name network =
    network { networkNodes = insert nodeNr (node { nodeName = name }) (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNodePosition :: Network g n e -> NodeNr -> DoublePoint
getNodePosition network nodeNr = nodePosition (networkNodes network ! nodeNr)

setNodePosition :: NodeNr -> DoublePoint -> Network g n e -> Network g n e
setNodePosition nodeNr position network =
    network { networkNodes = insert nodeNr (node { nodePosition = position }) (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNodeNameAbove :: Network g n e -> NodeNr -> Bool
getNodeNameAbove network nodeNr = nodeNameAbove (networkNodes network ! nodeNr)

setNodeNameAbove :: NodeNr -> Bool -> Network g n e -> Network g n e
setNodeNameAbove nodeNr nameAbove network =
    network { networkNodes = insert nodeNr (node { nodeNameAbove = nameAbove }) (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNodeShape :: Network g n e -> NodeNr -> Either String Shape
getNodeShape network nodeNr = nodeShape (networkNodes network ! nodeNr)

setNodeShape :: NodeNr -> Either String Shape -> Network g n e -> Network g n e
setNodeShape nodeNr shape network =
    network { networkNodes = insert nodeNr (node { nodeShape = shape })
                                           (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNodeInfo :: Network g n e -> NodeNr -> n
getNodeInfo network nodeNr = nodeInfo (networkNodes network ! nodeNr)

setNodeInfo :: NodeNr -> n -> Network g n e -> Network g n e
setNodeInfo nodeNr info network =
    network { networkNodes = insert nodeNr (node { nodeInfo = info }) (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNodeArity :: Network g n e -> NodeNr -> Maybe (PortNr,PortNr)
getNodeArity network nodeNr = nodeArity (networkNodes network ! nodeNr)

setNodeArity :: NodeNr -> Maybe (PortNr,PortNr) -> Network g n e
                -> Network g n e
setNodeArity nodeNr arity network =
    network { networkNodes = insert nodeNr (node { nodeArity = arity })
                                           (networkNodes network) }
  where node = networkNodes network ! nodeNr

getNameAbove :: Node a -> Bool
getNameAbove node = nodeNameAbove node

getName :: Node a -> String
getName node = nodeName node

getShape :: Node a -> Either String Shape
getShape node = nodeShape node

getPosition :: Node a -> DoublePoint
getPosition node = nodePosition node

getInfo :: Node a -> a
getInfo node = nodeInfo node

getArity :: Node a -> Maybe (PortNr,PortNr)
getArity node = nodeArity node

-- | Set whether the name should appear above (True) or below (False) the node
setNameAbove :: Bool -> Node a -> Node a
setNameAbove above node = node { nodeNameAbove = above }

setName :: String -> Node a -> Node a
setName name node = node { nodeName = name }

setShape :: Either String Shape -> Node a -> Node a
setShape s node = node { nodeShape = s }

setPosition :: DoublePoint -> Node a -> Node a
setPosition position node = node { nodePosition = position }

setInfo :: a -> Node a -> Node a
setInfo info node = node { nodeInfo = info }

setArity :: Maybe (PortNr,PortNr) -> Node a -> Node a
setArity arity node = node { nodeArity = arity }

-- | Get the next unused node number
getUnusedNodeNr :: Network g n e -> NodeNr
getUnusedNodeNr network | null used = 1
                        | otherwise = maximum used + 1
  where
    used = keys (networkNodes network)

-- | Get the next unused edge number
getUnusedEdgeNr :: Network g n e -> EdgeNr
getUnusedEdgeNr network | null used = 1
                        | otherwise = maximum used + 1
  where
    used = keys (networkEdges network)

-- | Get the node numbers of the parents of a given node
getParents :: Network g n e -> NodeNr -> [NodeNr]
getParents network child =
    [ parent
    | edge <- getEdges network
    , edgeTo edge == child
    , let parent = edgeFrom edge
    ]

type ParentMap = IntMap.IntMap [NodeNr]

-- | getParents is quite expensive (see above) and so
--   we store the parent relationship in an IntMap
getParentMap :: Network g n e -> ParentMap
getParentMap network =
    IntMap.fromList
        [ (nodeNr, getParents network nodeNr)
        | nodeNr <- getNodeNrs network
        ]

-- | Get the node numbers of the children of a given node
getChildren :: Network g n e -> NodeNr -> [NodeNr]
getChildren network parent =
    [ child
    | edge <- getEdges network
    , edgeFrom edge == parent
    , let child = edgeTo edge
    ]


-- | Get node with given index, raises exception if node number does not exist
getNode :: NodeNr -> Network g n e -> Node n
getNode nodeNr network
    | member nodeNr nodesMap = nodesMap ! nodeNr
    | otherwise = internalError "Network" "getNode" "illegal node number"
  where
    nodesMap = networkNodes network

-- | Get edge with given index, raises exception if edge number does not exist
getEdge :: EdgeNr -> Network g n e -> Edge e
getEdge edgeNr network = networkEdges network ! edgeNr

-- | Get all of the nodes in the network
getNodes :: Network g n e -> [Node n]
getNodes network = elems (networkNodes network)

-- | Get all of the edges in the network
getEdges :: Network g n e -> [Edge e]
getEdges network = elems (networkEdges network)

-- | Get all of the node numbers in the network
getNodeNrs :: Network g n e -> [NodeNr]
getNodeNrs network = keys (networkNodes network)

getPalette :: Network g n e -> Palette n
getPalette network = networkPalette network

getCanvasSize :: Network g n e -> (Double, Double)
getCanvasSize network = networkCanvasSize network

getGlobalInfo :: Network g n e -> g
getGlobalInfo network = networkInfo network

-- | Find the number of an edge given start and end node number
findEdge :: NodeNr -> NodeNr -> Network g n e -> Maybe EdgeNr
findEdge fromNodeNr toNodeNr network =
    let hits = IntMap.filter
                    (sameFromAndTo (Edge { edgeFrom = fromNodeNr
                                         , edgeTo = toNodeNr
                                         , edgeVia = undefined
                                         , edgeInfo = undefined
                                         , edgeFromPort = 0
                                         , edgeToPort = 0 }))
                    (networkEdges network)
    in case IntMap.keys hits of
        [key] -> Just key
        _ -> Nothing

-- | Find node numbers given a node name
findNodeNrsByName :: String -> Network g n e -> [NodeNr]
findNodeNrsByName theNodeName network =
    [ nodeNr
    | nodeNr <- getNodeNrs network
    , getNodeName network nodeNr == theNodeName
    ]

-- | Get a list of pairs where each pair contains a node number and the corresponding node
getNodeAssocs :: Network g n e -> [(NodeNr, Node n)]
getNodeAssocs network = assocs (networkNodes network)

setNodeAssocs :: [(NodeNr, Node n)] -> Network g n e -> Network g n e
setNodeAssocs nodeAssocs network =
    network { networkNodes = IntMap.fromList nodeAssocs }

-- | Get a list of pairs where each pair contains a edge number and the corresponding edge
getEdgeAssocs :: Network g n e -> [(EdgeNr, Edge e)]
getEdgeAssocs network = assocs (networkEdges network)

setEdgeAssocs :: [(EdgeNr, Edge e)] -> Network g n e -> Network g n e
setEdgeAssocs edgeAssocs network =
    network { networkEdges = IntMap.fromList edgeAssocs }

-- | Create a string that describes the network
dumpNetwork :: InfoKind e g => Network g String e -> String
dumpNetwork network = show (getNodeAssocs network) ++ "\n" ++ show (getEdgeAssocs network)

-- | Test for existence of a node number
nodeExists :: NodeNr ->  Network g n e -> Bool
nodeExists nodeNr network =
    member nodeNr (networkNodes network)

-- | Test for existence of an edge number
edgeExists :: EdgeNr ->  Network g n e -> Bool
edgeExists edgeNr network =
    member edgeNr (networkEdges network)

{-----------------------------------
  Functions that change the network
 -----------------------------------}

-- | Add a node to the network
addNode :: InfoKind n g
        => Network g n e           -- ^ the network to add the node to
        -> (NodeNr, Network g n e) -- ^ the number of the new node and
                                   --   the extended network
addNode network =
    addNodeEx   ("Node " ++ show nodeNr)
                (DoublePoint 0.0 0.0)
                True
                (Right Shape.circle)
                blank
                Nothing
                network
  where
    nodeNr = getUnusedNodeNr network

addNodes :: InfoKind n g => Int -> Network g n e -> ([NodeNr], Network g n e)
addNodes 0 network = ([], network)
addNodes n network1 =
    let (nodeNr, network2) = addNode network1
        (nodeNrs, network3) = addNodes (n-1) network2
    in (nodeNr:nodeNrs, network3)

addNodeEx :: InfoKind n g =>
             String -> DoublePoint -> Bool -> Either String Shape -> n
             -> Maybe (PortNr,PortNr)
             -> Network g n e -> (NodeNr, Network g n e)
addNodeEx name position labelAbove shape info arity network =
    ( nodeNr
    , network { networkNodes = insert nodeNr node (networkNodes network) }
    )
  where
    nodeNr = getUnusedNodeNr network
    node = constructNode name position labelAbove shape info arity


-- | Add an edge to the network.
addEdge :: InfoKind e g => NodeNr -> NodeNr -> Network g n e -> Network g n e
addEdge fromNodeNr toNodeNr network
    | any (sameFromAndTo edge) edgesList || -- prohibit double edges
      any (sameFromAndTo (reverseEdge edge)) edgesList = -- prohibit edges in opposite direction
        network
    | otherwise =
        let edgeNr = getUnusedEdgeNr network
        in setNodeArity fromNodeNr (updateFromArity fromArity) $
           setNodeArity toNodeNr   (updateToArity toArity) $
           network { networkEdges = insert edgeNr edge (networkEdges network) }
  where
    edge = constructEdge fromNodeNr fromPortNr toNodeNr toPortNr [] blank
    edgesList = elems (networkEdges network)
    fromArity  = getNodeArity network fromNodeNr
    toArity    = getNodeArity network toNodeNr
    fromPortNr = 1 + (maybe 0 snd $ fromArity)
    toPortNr   = 1 + (maybe 0 fst $ toArity)
    updateFromArity Nothing      = Just (0,1)
    updateFromArity (Just (n,m)) = Just (n,m+1)
    updateToArity Nothing        = Just (1,0)
    updateToArity (Just (n,m))   = Just (n+1,m)

-- | Add an edge to the network, with specific connection ports.
addEdgeWithPorts :: InfoKind e g =>
                    NodeNr -> PortNr -> NodeNr -> PortNr
                    -> Network g n e -> Network g n e
addEdgeWithPorts fromNodeNr fromPortNr toNodeNr toPortNr network
    | any (sameFromAndTo edge) edgesList || -- prohibit double edges
      any (sameFromAndTo (reverseEdge edge)) edgesList = -- prohibit edges in opposite direction
        network
    | otherwise =
        let edgeNr = getUnusedEdgeNr network
            networkPlusEdge = network { networkEdges = insert edgeNr edge (networkEdges network) }
        in networkPlusEdge
  where
    edge = constructEdge fromNodeNr fromPortNr toNodeNr toPortNr [] blank
 -- edge = Edge { edgeFrom = fromNodeNr, edgeTo = toNodeNr, edgeVia = []
 --             , edgeInfo = blank, edgeFromPort = fromPortNr
 --             , edgeToPort = toPortNr }
    edgesList = elems (networkEdges network)

addEdges :: InfoKind e g => [(NodeNr,NodeNr)] -> Network g n e -> Network g n e
addEdges edgeTuples network =
  foldr (\(fromNr, toNr) net -> addEdge fromNr toNr net) network edgeTuples

-- | Insert a new 'via' control point in the middle of an edge.
newViaEdge :: EdgeNr -> ViaNr -> DoublePoint
              -> Network g n e -> Network g n e
newViaEdge edgeNr viaNr point network =
    network { networkEdges = adjust (\e->e{ edgeVia= take viaNr (edgeVia e)
                                                     ++[point]
                                                     ++drop viaNr (edgeVia e) })
                                    edgeNr
                                    (networkEdges network) }

-- | Remove node with given index, raises exception if node number does not exist.
--   This function also removes all edges that start or end in this node.
removeNode :: NodeNr ->  Network g n e -> Network g n e
removeNode nodeNr network =
    let involvedEdges = [ i
                        | (i, edge) <- getEdgeAssocs network
                        , edgeFrom edge == nodeNr || edgeTo edge == nodeNr
                        ]
        networkWithoutEdges = foldr removeEdge network involvedEdges
        networkWithoutNode = networkWithoutEdges { networkNodes = delete nodeNr (networkNodes networkWithoutEdges) }
    in networkWithoutNode

-- | Remove an edge from the network. The probability table of the target node is updated:
--   the corresponding dimension is removed and all values are zeroed.
--   An exception is raised if edge number does not exist.
removeEdge :: EdgeNr -> Network g n e -> Network g n e
removeEdge edgeNr network =
    setNodeArity fromNodeNr (Just (fi,fo-1)) $
    setNodeArity toNodeNr   (Just (ti-1,to)) $
    network { networkEdges = delete edgeNr (networkEdges network) }
  where
    (fi,fo)      = maybe (0,1) id $ getNodeArity network fromNodeNr
    (ti,to)      = maybe (1,0) id $ getNodeArity network toNodeNr
    edge         = getEdge edgeNr network
    fromNodeNr   = getEdgeFrom edge
    toNodeNr     = getEdgeTo edge
    

-- | Remove all edges from the network. The probability tables of all node are zeroed.
removeAllEdges :: Network g n e -> Network g n e
removeAllEdges network =
    let networkWithoutEdges       = network { networkEdges = IntMap.empty }
    in  networkWithoutEdges

-- | Remove a control point from an edge.
removeVia :: EdgeNr -> ViaNr -> Network g n e -> Network g n e
removeVia edgeNr viaNr network =
    let remove n e = e { edgeVia = take n (edgeVia e)
                                   ++ drop (n+1) (edgeVia e) } in
    network { networkEdges = adjust (remove viaNr)
                                    edgeNr (networkEdges network) }

setPalette :: Palette n -> Network g n e -> Network g n e
setPalette palette network = network { networkPalette = palette }

setCanvasSize :: (Double, Double) -> Network g n e -> Network g n e
setCanvasSize canvasSize network = network { networkCanvasSize = canvasSize }

setGlobalInfo :: g -> Network g n e -> Network g n e
setGlobalInfo info network = network { networkInfo = info }

{-----------------------------------
  Local functions
 -----------------------------------}

sameFromAndTo :: Edge e -> Edge e -> Bool
sameFromAndTo edge1 edge2 =
    edgeFrom edge1 == edgeFrom edge2 && edgeTo edge1 == edgeTo edge2

reverseEdge :: Edge e -> Edge e
reverseEdge edge =
    edge { edgeFrom = edgeTo edge, edgeTo = edgeFrom edge }

-- | Update node with given number by applying the function to it
--   Dangerous (wrt network consistency, do not export)
updateNode :: NodeNr -> (Node n -> Node n) -> Network g n e -> Network g n e
updateNode nodeNr nodeFunction network =
    let node = getNode nodeNr network in
    network { networkNodes = insert nodeNr (nodeFunction node)
                                           (networkNodes network) }

updateEdge :: EdgeNr -> (Edge e -> Edge e) -> Network g n e -> Network g n e
updateEdge edgeNr edgeFunction network =
    network { networkEdges = adjust edgeFunction edgeNr
                                    (networkEdges network) }

updateVia :: EdgeNr -> ViaNr -> DoublePoint -> Network g n e -> Network g n e
updateVia edgeNr viaNr v network =
    network { networkEdges =
                  adjust (\e-> e { edgeVia = take viaNr (edgeVia e)
                                             ++[v]++drop (viaNr+1) (edgeVia e) })
                         edgeNr (networkEdges network) }
