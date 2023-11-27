{-# LANGUAGE StrictData, RankNTypes #-}
module Main where
import Control.Concurrent
import Control.Parallel
import Sim
import Chain
import Patricia
import PatChain (PatChain)
import Msg
import Graphics.Rendering.Chart.Easy hiding (Vector, colors, shapes)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.DeepSeq
import Data.List (foldl')
import Control.Monad.State
import Dag (Dag)

materialize = V.fromList . uncurry zip

main = do
  v <- materialize <$> simulate (noMsgs :: Dag) 1 0.5 1 100 200
  print v
  toFile def ("tester.png") $ do
    plot $ points "dag" $ V.toList v
