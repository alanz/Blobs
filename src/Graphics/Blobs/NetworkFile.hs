{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Blobs.NetworkFile where

import qualified Graphics.Blobs.Network as N
import qualified Graphics.Blobs.Document as D
import Graphics.Blobs.InfoKind
import Data.Data

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8

{-
-- | Print the network data structure to an XML text
toString :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
             ToJSON c) =>
            N.Network g n e c -> String
-- toString network = encodeXML network
toString network = C8.unpack $ encode network

-- | Print the network data structure to an XML text
toStringAssocs :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
                   ToJSON c) =>
            -- N.Network g n e c -> String
            [(D.NetworkId, N.Network g n e c)] -> String
--toStringAssocs assocs = encodeXML assocs
toStringAssocs assocs = C8.unpack $ encode assocs
-}

-- | Print the document  data structure to a JSON text
toStringWholeDoc :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
                   ToJSON (D.Document g n e c)) =>
            -- N.Network g n e c -> String
            -- [(D.NetworkId, N.Network g n e c)] -> String
            D.Document g n e c -> String
toStringWholeDoc assocs = C8.unpack $ encode assocs

{-
-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromString :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
               FromJSON c) =>
              String -> Either String (N.Network g n e c, [String], Bool)
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
fromStringAssocs :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
                     FromJSON c) =>
              String -> Either String ([(D.NetworkId, N.Network g n e c)], [String], Bool)
fromStringAssocs xml =
    -- case decodeXML xml of
    --     Left err -> Left err -- lexical or initial (generic) parse error
    --     Right v -> case N.networkValid v of
    --       Left err -> Left err
    --       Right _  -> Right (v,[],False)
    case decode (C8.pack xml) of
      Nothing -> Left "no parse"
      Just v -> Right (v,[],False)
-}

-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromStringWholeDoc :: (InfoKind n g, InfoKind e g, Data g, Typeable g, Data (N.Network g n e c),
                       FromJSON (D.Document g n e c)) =>
              -- String -> Either String ([(D.NetworkId, N.Network g n e c)], [String], Bool)
              String -> Either String (D.Document g n e c, [String], Bool)
fromStringWholeDoc str =
    case decode (C8.pack str) of
      Nothing -> Left "no parse"
      Just v -> Right (v,[],False)


