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
class (Eq a, Show a, Parse a, XmlContent a, GuiEdit a, GuiEdit g) => InfoKind a g | a -> g where
    blank :: a
    check :: String -> g -> a -> [String]		-- returns warnings
	-- ^ first arg is container label for error reporting.
	--   second arg is global value
    {-
    editDialog :: Window a1 -- ^ Parent frame
                  -> String -- ^ Window title
                  -> a      -- ^ Existing value
                  -> IO (Maybe a) -- ^ Updated value if changed
    -}


-- A basic instance representing "no info"
instance InfoKind () () where
    blank = ()
    check _ _ () = []

instance GuiEdit () where
    editDialog parentWindow dialogTitle initial = aTextDialog parentWindow dialogTitle initial


-- Assume that info is mandatory, but not supplied a priori.
instance InfoKind a b => InfoKind (Maybe a) b where
    blank = Nothing
    check n _ Nothing  = ["No info value stored with "++n]
    check n g (Just a) = check n g a

instance Descriptor a => Descriptor (Maybe a) where
  descriptor Nothing  =  "Nothing"
  descriptor (Just x) =  "Just (" ++ (descriptor x) ++ ")"

instance (Show a,Parse a, Descriptor a) => GuiEdit (Maybe a) where
  editDialog a = undefined



-- A "showType"-style class.  Descriptor should always ignore its argument,
-- and return a constant string describing the type instead.
class (Show a) => Descriptor a where
    descriptor :: a -> String
    descriptor _ = "type descriptor was left undefined"
instance Descriptor () where
    descriptor _ = "null global info type"

-- -----------------------------------------------

-- A class of things that can be edited in a GUI, falling back to a
-- simple text editor using Parse/Show
class (Show a,Parse a,Descriptor a) => GuiEdit a where
-- class GuiEdit a where
    editDialog :: Window a1 -- ^ Parent frame
                  -> String -- ^ Window title
                  -> a      -- ^ Existing value
                  -> IO (Maybe a) -- ^ Updated value if changed


-- -----------------------------------------------
{-
instance XmlContent () where
  toContents = undefined
  parseContents = undefined
-}
