module Main where
import Control.Concurrent
import Sim
import Chain
import Patricia
import Msg
import Control.Parallel
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main = do
  setNumCapabilities 7
  (times, avgs) <- simulate (noMsgs :: Chain') 2 100 200
  (times2, avgs2) <- avgs `par` simulate (noMsgs :: Patricia) 2 100 200
  toFile def "result.png" $ do
    plot $ points "chain" $ zip times avgs
    plot $ points "patricia" $ zip times2 avgs2
