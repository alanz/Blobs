module Palette where

import List (nub, (\\))
import Shape
import Text.ParserCombinators.TextParser as Parse

data Palette a = Palette [ (String, (Shape, Maybe a)) ]
  deriving (Eq, Show, Read)

shapes :: Palette a -> [ (String,(Shape,Maybe a)) ]
shapes (Palette p) = p

join :: Eq a => Palette a -> Palette a -> Palette a
join (Palette p) (Palette q) = Palette (nub (p++q))

delete :: Eq a => Palette a -> Palette a -> Palette a
delete (Palette p) (Palette q) = Palette (p\\q)

-- cannot be completely empty, always one default shape
empty :: Palette a
empty = Palette [("circle", (Shape.circle, Nothing))]

instance Functor Palette where
    fmap _ (Palette p) = Palette (map (\ (n,(s,i))-> (n,(s,Nothing))) p)

instance Parse a => Parse (Palette a) where
    parse = do{ isWord "Palette"; fmap Palette $ parse }

