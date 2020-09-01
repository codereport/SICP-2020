-- Makes use of the C and B combinators (Cardinal and Bluebird)
import Data.List.HT (mapAdjacent)

zeroCrossings = map (truncate . signum) 
              . mapAdjacent (flip (-)) 
              . map signum
