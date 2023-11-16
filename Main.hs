module Main where
import Control.Concurrent
import Sim
import Chain
import Patricia
import PatChain (PatChain, Rate(..))
import Msg
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

instance Rate () where
    rate _ = 0.5 + 1.3

-- Rename these to be 'scale', not rate. 

main = do
  setNumCapabilities 7
  (times, avgs) <- simulate (noMsgs :: Chain') 1.3 100 200
  let list1 = V.fromList $ zip times avgs
  (times2, avgs2) <- list1 `seq` simulate (noMsgs :: Patricia) 1.3 100 200
  let list2 = V.fromList $ zip times2 avgs2
  (times3, avgs3) <- list2 `seq` simulate (noMsgs :: PatChain ()) 1.3 100 200
  let list3 = V.fromList $ zip times3 avgs3
  toFile def "result.png" $ do
    plot $ points "chain" $ V.toList list1
    plot $ points "patricia" $ V.toList list2
    plot $ points "patcChain" $ V.toList list3
