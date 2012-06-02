module Graphics.Blobs.DisplayOptions where

import List ((\\))

type ShowInfo = [What]
data What     = GlobalInfo | NodeLabel | NodeInfo | EdgeInfo	deriving (Eq)

data DisplayOptions = DP
	{ dpShowInfo :: ShowInfo
	}

standard :: DisplayOptions
standard = DP [GlobalInfo, NodeLabel, NodeInfo, EdgeInfo]

toggle :: What -> DisplayOptions -> DisplayOptions
toggle w (DP opts) = DP (if w `elem` opts then opts\\[w] else w:opts)
