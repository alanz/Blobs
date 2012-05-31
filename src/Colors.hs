module Colors where

import Graphics.UI.WX
import Text.ParserCombinators.TextParser

-- Different spelling of colour/color to distinguish local/wx datatypes.
data Colour = RGB !Int !Int !Int deriving (Eq,Show,Read)

instance Parse Colour where
  parse = do { isWord "RGB"
             ; return RGB `apply` parse `apply` parse `apply` parse
             }

-- translate local to wx
wxcolor :: Colour -> Color
wxcolor (RGB r g b) = rgb r g b

nodeColor, labelBackgroundColor, evidenceColor, evidenceHatchColor,
  wrongProbabilitiesColor, paneBackgroundColor, activeSelectionColor,
  inactiveSelectionColor :: Colour
nodeColor = lightBlue
evidenceColor = lightYellow
evidenceHatchColor = licorice
labelBackgroundColor = lightYellow
paneBackgroundColor = coconut
activeSelectionColor = licorice
inactiveSelectionColor = lightGrey
wrongProbabilitiesColor = lightRed

testSelectionTestColor, testSelectionTargetColor :: Colour
testSelectionTestColor = RGB 0 255 0
testSelectionTargetColor = RGB 255 0 0

lightYellow, lightBlue, lightRed, lightGrey, pink :: Colour
lightYellow = RGB 236 236 169
lightBlue = RGB 200 255 255
lightGrey = RGB 150 150 150
lightRed = RGB 255 200 200
pink = RGB 255 200 200

systemGrey :: Color	-- wx type
systemGrey = colorSystem Color3DFace

licorice, coconut :: Colour	-- names black and white already taken by wx
licorice = RGB 0   0   0
coconut  = RGB 255 255 255

darkGreen, darkBlue, violet, indigo, darkRed, darkMagenta, darkOrange,
  orange, lightPink, purple, lightGreen, mediumPurple, darkViolet, gray,
  darkGrey, darkGray, lightGray, silver, whiteSmoke, aqua, teal, maroon,
  olive, sienna, brown, fuchsia, turquoise, orangeRed, gold,darkSlateGray
      :: Colour
darkGreen = RGB 0 100 0
darkBlue = RGB 0 0 139
violet = RGB 238 130 238
indigo = RGB 75 0 130
darkRed = RGB 139 0 0
darkMagenta = RGB 139 0 139
darkOrange = RGB 255 140 0
orange = RGB 255 165 0
lightPink = RGB 255 182 193
purple = RGB 128 0 128
lightGreen = RGB 144 238 144
mediumPurple = RGB 147 112 219
darkViolet = RGB 148 0 211

gray = RGB 128 128 128
darkGrey = RGB 169 169 169 -- lighter than grey?
darkGray = RGB 169 169 169
lightGray = RGB 211 211 211
silver = RGB 192 192 192
whiteSmoke = RGB 245 245 245

aqua = RGB 0 255 255
teal = RGB 0 128 128
maroon = RGB 128 0 0
olive = RGB 128 128 0
sienna = RGB 160 82 45
brown = RGB 165 42 42
fuchsia = RGB 255 0 255
turquoise = RGB 64 224 208
orangeRed = RGB 255 69 0
gold = RGB 255 215 0
darkSlateGray = RGB 47 79 79
