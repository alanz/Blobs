{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Blobs.NetworkFile where

import qualified Graphics.Blobs.Network as N
import qualified Graphics.Blobs.Document as D
import Graphics.Blobs.InfoKind
import Data.Data

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8

-- | Print the network data structure to an XML text
toString :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e)) =>
            N.Network g n e -> String
-- toString network = encodeXML network
toString network = C8.unpack $ encode network

-- | Print the network data structure to an XML text
toStringAssocs :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e)) =>
            -- N.Network g n e -> String
            [(D.NetworkId, N.Network g n e)] -> String
--toStringAssocs assocs = encodeXML assocs
toStringAssocs assocs = C8.unpack $ encode assocs


-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromString :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e)) =>
              String -> Either String (N.Network g n e, [String], Bool)
fromString xml =
    -- case decodeXML xml of
    --     Left err -> Left err -- lexical or initial (generic) parse error
    --     Right v -> Right (v,[],False)
    case decode (C8.pack xml) of
      Nothing -> Left "no parse"
      Just v -> Right (v,[],False)

-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromStringAssocs :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e)) =>
              String -> Either String ([(D.NetworkId, N.Network g n e)], [String], Bool)
fromStringAssocs xml =
    -- case decodeXML xml of
    --     Left err -> Left err -- lexical or initial (generic) parse error
    --     Right v -> case N.networkValid v of
    --       Left err -> Left err
    --       Right _  -> Right (v,[],False)
    case decode (C8.pack xml) of
      Nothing -> Left "no parse"
      Just v -> Right (v,[],False)


