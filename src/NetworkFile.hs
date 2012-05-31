{-# LANGUAGE UndecidableInstances #-}
module NetworkFile where

import Network
import Math
import Common
import Colors
import Shape
import InfoKind
import Palette

fromString :: (InfoKind n g, InfoKind e g{-, XmlContent g-}) =>
              String -> Either String (Network g n e, [String], Bool)
fromString x = undefined

toString :: (InfoKind n g, InfoKind e g{-, XmlContent g-}) =>
            Network g n e -> String
toString x = "fpp"