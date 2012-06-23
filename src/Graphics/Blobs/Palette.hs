{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Blobs.Palette where

import Data.Data
import Data.List (nub, (\\))
import Text.Parse
import qualified Graphics.Blobs.Shape as Shape
import Data.Aeson.TH


data Palette a = Palette [ (String, (Shape.Shape, Maybe a)) ]
  deriving (Eq, Show, Read, Data, Typeable)

shapes :: Palette a -> [ (String,(Shape.Shape,Maybe a)) ]
shapes (Palette p) = p

join :: Eq a => Palette a -> Palette a -> Palette a
join (Palette p) (Palette q) = Palette (nub (p++q))

delete :: Eq a => Palette a -> Palette a -> Palette a
delete (Palette p) (Palette q) = Palette (p\\q)

-- cannot be completely empty, always one default shape
empty :: Palette a
empty = Palette [("circle", (Shape.circle, Nothing))]

instance Functor Palette where
    fmap _ (Palette p) = Palette (map (\ (n,(s,_i))-> (n,(s,Nothing))) p)

instance Parse a => Parse (Palette a) where
    parse = do{ isWord "Palette"; fmap Palette $ parse }

deriveJSON id ''Palette

-- ---------------------------------------------------------------------
-- orphan instances coming home

{- handwritten -}
{-
instance XML.HTypeable a => XML.HTypeable (Palette a) where
    toHType p = XML.Defined "Palette" [XML.toHType a] [XML.Constr "Palette" [] []]
              where (Palette ((_,(_,Just a)):_)) = p
instance XML.XmlContent a => XML.XmlContent (Palette a) where
    toContents (Palette xs) =
        [ XML.mkElemC "Palette" (concatMap XML.toContents xs) ]
    parseContents = do
        { XML.inElement "Palette" $ fmap Palette (XML.many1 XML.parseContents) }

-}