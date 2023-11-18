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

materialize = V.fromList . uncurry zip

test v b a st = materialize <$> simulate st v b a 100 200

parallel xs = foldl' (flip par) xs xs

shapes = [PointShapeCircle, PointShapePlus, PointShapeCross]

colors = map opaque [blue, orangered, green]

testAll :: (forall a. Msg a => a -> IO (Vector (Double, Double)))
  -> IO [Vector (Double, Double)]
testAll f = parallel <$> sequence [
  f (noMsgs :: Patricia), f (noMsgs :: Chain'), f (noMsgs :: PatChain)]

theTests = sequence [
  sequence [testAll $ test v 0.5 1 | v <- [0.5, 1, 2]],
  sequence [testAll $ test 1 b 1 | b <- [0.5, 1, 2]],
  sequence [testAll $ test 1 0.5 a | a <- [0.5, 1, 2]]]

main = do
  setNumCapabilities 27
  results <- theTests
  forM_ (zip ["Time Skew Scale", "Gossip Scale", "Sending Scale"] results) $ \(l, st)-> do
    toFile def (l ++ ".png") $ do
      modify $ (layout_x_axis . laxis_title .~ "seconds") . (layout_y_axis . laxis_title .~ "nodes examined")
      layout_title .= l ++ " Comparison"
      forM_ (zip3 ["0.5", "1", "2"] st shapes) $ \(val, strat, sh)-> do
        setShapes [sh]
        forM_ (zip3 ["patricia", "chain", "patchain"] strat colors) $ \(lbl, f, c)-> do
          setColors [c]
          plot $ points (lbl ++ " " ++ val) $ V.toList f

