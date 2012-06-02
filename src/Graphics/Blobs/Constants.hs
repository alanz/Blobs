module Graphics.Blobs.Constants where

import Graphics.UI.WX
import Graphics.Blobs.Colors

kSELECTED_WIDTH :: Int
kSELECTED_WIDTH = 3

kEDGE_CLICK_RANGE, kNODE_RADIUS, kARROW_SIZE:: Double
kEDGE_CLICK_RANGE   = 0.2
kNODE_RADIUS        = 0.5
kARROW_SIZE         = 0.3

kSELECTED_OPTIONS :: [Prop (DC ())]
kSELECTED_OPTIONS = [ penWidth := kSELECTED_WIDTH ]

kNodeLabelColour :: Colour
kNodeLabelColour = licorice

kNodeInfoColour :: Colour
kNodeInfoColour = darkViolet

kEdgeInfoColour :: Colour
kEdgeInfoColour = orangeRed


