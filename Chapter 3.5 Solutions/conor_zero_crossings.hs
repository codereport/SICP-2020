-- Makes use of the W and B combinators (Warbler and Bluebird)
import Data.List.HT (mapAdjacent)

zeroCrossings = map (truncate . signum) 
              . mapAdjacent (flip (-)) 
              . map signum
