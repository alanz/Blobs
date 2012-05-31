module Operations where

import InfoKind
import Network
import State
import Document
import qualified PersistentDocument as PD

import IntMap

-- | @GraphOps@ is a data structure holding a bunch of named operations
--   on the graph network.  An operation is simply executed in the I/O monad,
--   taking the entire state as argument - it is up to the action to do any
--   state updates it wants to.
data GraphOps g n e = GraphOps { ioOps :: [ (String, IOOp g n e) ] }

callGraphOp :: String -> GraphOps g n e -> State g n e -> IO ()
callGraphOp opName graphOps state =
  maybe (return ()) ($ state) (Prelude.lookup opName (ioOps graphOps))

type PureOp g n e = -- (InfoKind n g, InfoKind e g)
                       (g, IntMap (Node n), IntMap (Edge e))
                    -> (g, IntMap (Node n), IntMap (Edge e))
type IOOp g n e   = -- (InfoKind n g, InfoKind e g) =>
                       State g n e
                    -> IO ()

-- | In general, operations can be classified into pure and I/O variants.
--   A pure operation takes a graph and returns a new graph, which is
--   stored back into the current document (can be reverted with the
--   standard 'undo' menu item), and displayed immediately.  Use this
--   helper 'pureGraphOp' to turn your pure function into an I/O action
--   for the Operations menu.
pureGraphOp :: (String, PureOp g n e) -> (String, IOOp g n e)
pureGraphOp (opName,operation) =
  (opName, \state-> do{ pDoc <- getDocument state
                      ; doc  <- PD.getDocument pDoc
                      ; let network = getNetwork doc
                            g = getGlobalInfo network
                            n = networkNodes network
                            e = networkEdges network
                            (g',n',e') = operation (g,n,e)
                            network' = setNodeAssocs (assocs n')
                                       $ setEdgeAssocs (assocs e')
                                       $ setGlobalInfo g'
                                       $ network
                      ; PD.updateDocument opName (setNetwork network') pDoc
                      }
  )

