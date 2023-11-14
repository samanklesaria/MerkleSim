module Main where
import Control.Concurrent
import Sim
import Chain
import Patricia
import Msg
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main = do
  setNumCapabilities 7
  (times, avgs) <- simulate (noMsgs :: Chain') 2 100 1000
  let list1 = V.fromList $ zip times avgs
  (times2, avgs2) <- list1 `seq` simulate (noMsgs :: Patricia) 2 100 1000
  let list2 = V.fromList $ zip times2 avgs2
  toFile def "result.png" $ do
    plot $ points "chain" $ V.toList list1
    plot $ points "patricia" $ V.toList list2
