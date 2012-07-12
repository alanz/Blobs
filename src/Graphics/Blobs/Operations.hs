module Graphics.Blobs.Operations where

import Graphics.Blobs.Network
import Graphics.Blobs.State
import Graphics.Blobs.Document
import qualified Graphics.Blobs.PersistentDocument as PD

import qualified Data.IntMap as IntMap

-- | @GraphOps@ is a data structure holding a bunch of named operations
--   on the graph network.  An operation is simply executed in the I/O monad,
--   taking the entire state as argument - it is up to the action to do any
--   state updates it wants to.
data GraphOps g n e c = GraphOps { ioOps :: [ (String, IOOp g n e c) ] }

callGraphOp :: String -> GraphOps g n e c -> State g n e c -> IO ()
callGraphOp opName graphOps state =
  maybe (return ()) ($ state) (Prelude.lookup opName (ioOps graphOps))

type PureOp g n e c = -- (InfoKind n g, InfoKind e g)
                       (g, IntMap.IntMap (Node n), IntMap.IntMap (Edge e))
                    -> (g, IntMap.IntMap (Node n), IntMap.IntMap (Edge e))
type IOOp g n e c   = -- (InfoKind n g, InfoKind e g) =>
                       State g n e c
                    -> IO ()

-- | In general, operations can be classified into pure and I/O variants.
--   A pure operation takes a graph and returns a new graph, which is
--   stored back into the current document (can be reverted with the
--   standard 'undo' menu item), and displayed immediately.  Use this
--   helper 'pureGraphOp' to turn your pure function into an I/O action
--   for the Operations menu.
pureGraphOp :: (String, PureOp g n e c) -> (String, IOOp g n e c)
pureGraphOp (opName,operation) =
  (opName, \state-> do{ pDoc <- getDocument state
                      ; doc  <- PD.getDocument pDoc
                      ; let network = getNetwork doc
                            g = getGlobalInfo doc
                            n = networkNodes network
                            e = networkEdges network
                            (g',n',e') = operation (g,n,e)
                            network' = setNodeAssocs (IntMap.assocs n')
                                       $ setEdgeAssocs (IntMap.assocs e')
                                       -- $ setGlobalInfo g'
                                       $ network
                      ; PD.updateDocument opName (setNetworkAndGlobal network' g') pDoc
                      }
  )

