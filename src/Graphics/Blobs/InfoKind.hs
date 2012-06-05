{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Blobs.InfoKind where

import Text.Parse
import Text.XML.HaXml.XmlContent.Haskell
import Graphics.UI.WX
import Graphics.Blobs.CommonIO

-- | The @InfoKind@ class is a predicate that ensures we can always create
--   at least a blank (empty) information element, that we can read and
--   write them to/from the user, and that there exists some method of
--   determining the correctness of the value (completeness/consistency etc)
--   against some global type.
class (Eq a, Show a, Parse a, XmlContent a) => InfoKind a g | a -> g where
    blank :: a
    check :: String -> g -> a -> [String]		-- returns warnings
	-- ^ first arg is container label for error reporting.
	--   second arg is global value
    editDialog :: Window a1 -- ^ Parent frame
                  -> String -- ^ Window title
                  -> a      -- ^ Existing value
                  -> IO (Maybe a) -- ^ Updated value if changed


-- A basic instance representing "no info"
instance InfoKind () () where
    blank = ()
    check _ _ () = []
    editDialog parentWindow dialogTitle initial = aTextDialog parentWindow dialogTitle initial


-- Assume that info is mandatory, but not supplied a priori.
instance InfoKind a b => InfoKind (Maybe a) b where
    blank = Nothing
    check n _ Nothing  = ["No info value stored with "++n]
    check n g (Just a) = check n g a

-- A "showType"-style class.  Descriptor should always ignore its argument,
-- and return a constant string describing the type instead.
class (Show a) => Descriptor a where
    descriptor :: a -> String
    descriptor _ = "type descriptor was left undefined"
instance Descriptor () where
    descriptor _ = "null global info type"

-- -----------------------------------------------
{-
instance XmlContent () where
  toContents = undefined
  parseContents = undefined
-}
