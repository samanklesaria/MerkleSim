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
import Control.Parallel

instance Rate () where
    rate _ = 0.5 + 1.3

-- Rename these to be 'scale', not rate. 

materialize = V.fromList . uncurry zip

main = do
  setNumCapabilities 7
  list1 <- materialize <$> simulate (noMsgs :: Chain') 1.3 100 200
  list2 <- par list1 $ materialize <$> simulate (noMsgs :: Patricia) 1.3 100 200
  -- list3 <- par list2 $ materialize <$> simulate (noMsgs :: PatChain ()) 1.3 100 200
  toFile def "result.png" $ do
    plot $ points "chain" $ V.toList list1
    plot $ points "patricia" $ V.toList list2
    -- plot $ points "patcChain" $ V.toList list3
