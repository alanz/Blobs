{-# LANGUAGE UndecidableInstances #-}
module Graphics.Blobs.NetworkFile where

import qualified Graphics.Blobs.Network as N
import qualified Graphics.Blobs.Document as D
import Graphics.Blobs.InfoKind

import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn (noPos)
import Text.XML.HaXml.Parse hiding (element)
import Text.XML.HaXml.XmlContent.Haskell as XML
import Text.PrettyPrint.HughesPJ
import qualified Text.XML.HaXml.Pretty as Pretty

-- | Print the network data structure to an XML text
toString :: (InfoKind n g, InfoKind e g, XmlContent g) =>
            N.Network g n e -> String
toString network = render . Pretty.document $
    Document (Prolog Nothing [] (Just (toDTD (toHType network))) []) emptyST
             (f (toContents network)) []
  where
    f [CElem e _] = e
    f _ = error "bad"	-- shouldn't happen


-- | Print the network data structure to an XML text
toStringAssocs :: (InfoKind n g, InfoKind e g, XmlContent g) =>
            -- N.Network g n e -> String
            [(D.NetworkId, N.Network g n e)] -> String
toStringAssocs assocs = render . Pretty.document $
    Document (Prolog Nothing [] (Just (toDTD (toHType assocs))) []) emptyST
             (f (toContents assocs)) []
  where
    f [CElem e _] = e
    f _ = error "bad"	-- shouldn't happen




-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromString :: (InfoKind n g, InfoKind e g, XmlContent g) =>
              String -> Either String (N.Network g n e, [String], Bool)
fromString xml =
    case xmlParse' "input file" xml of
        Left err -> Left err -- lexical or initial (generic) parse error
        Right (Document _ _ e _) ->
            case runParser parseContents [CElem e noPos] of
                (Left err, _) -> Left err  -- secondary (typeful) parse error
                (Right v, _)  -> Right (v,[],False)


-- | Parses a string to the network data structure
--   Returns either an error message (Left) or the network,
--   a list of warnings (Right) and a boolean indicating whether
--   the file was an old Dazzle file
fromStringAssocs :: (InfoKind n g, InfoKind e g, XmlContent g) =>
              String -> Either String ([(D.NetworkId, N.Network g n e)], [String], Bool)
fromStringAssocs xml =
    case xmlParse' "input file" xml of
        Left err -> Left err -- lexical or initial (generic) parse error
        Right (Document _ _ e _) ->
            case runParser parseContents [CElem e noPos] of
                (Left err, _) -> Left err  -- secondary (typeful) parse error
                (Right v, _)  -> Right (v,[],False)


