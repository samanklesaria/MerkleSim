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

data Ty = ChainC | PatChainC | PatC | DagC

materialize = V.fromList . uncurry zip

test v b a ChainC = materialize <$> simulate (noMsgs :: Chain') v b a 100 200
test v b a PatChainC = materialize <$> simulate (noMsgs :: PatChain) v b a 100 200
test v b a PatC = materialize <$> simulate (noMsgs :: Patricia) v b a 100 200
test v b a DagC = materialize <$> simulate (noMsgs :: Dag) v b a 100 200

parallel xs = foldl' (flip par) xs xs

shapes = [PointShapeCircle, PointShapePlus, PointShapeCross]

colors = map opaque [blue, orangered, green, black]

p = fmap parallel . sequence

dyns = [ChainC, PatChainC, PatC, DagC]

vdyns = [ChainC, PatChainC]

theTests :: IO [[Vector (Double, Double)]]
theTests = p [
  p [test 1 0.5 1 s | s <- dyns],
  p [test v 0.5 1 s | v <- [0.5, 2], s <- vdyns],
  p [test 1 b 1 s | b <- [1, 2], s <- dyns],
  p [test 1 0.5 a s | a <- [0.5, 2], s <- dyns]]

axes = modify $ (layout_x_axis . laxis_title .~ "seconds") . (layout_y_axis . laxis_title .~ "nodes examined")

labels = ["chain", "patchain", "patricia", "dag"]

plotBaselines baselines labels = do
  axes
  setShapes [head shapes]
  forM_ (zip3 baselines labels colors) $ \(f, st, c)-> do
    setColors [c]
    plot $ points st $ V.toList f

main = do
  setNumCapabilities 7
  [baselines, vtest, btest, atest] <- theTests
  toFile def ("Time Skew Scale.png") $ do
    layout_title .= "Time Skew Scale Comparison"
    plotBaselines baselines ["chain-1", "patchain-1", "patricia", "dag"]
    let spec = [(v, f) | v <- zip ["0.5, 2"] (tail shapes), f <- zip ["chain", "patchain"] colors]
    forM_ (zip spec vtest) $ \(((v,sh), (st, c)), f)-> do
      setShapes [sh]
      setColors [c]
      plot $ points (st ++ " " ++ v) $ V.toList f
  toFile def ("Gossip Scale.png") $ do
    layout_title .= "Gossip Scale Comparison"
    plotBaselines baselines ["chain-0.5", "patchain-0.5", "patricia-0.5", "dag-0.5"]
    let spec = [(v, f) | v <- zip ["1, 2"] (tail shapes), f <- zip labels colors]
    forM_ (zip spec btest) $ \(((v,sh), (st, c)), f)-> do
      setShapes [sh]
      setColors [c]
      plot $ points (st ++ " " ++ v) $ V.toList f
  toFile def ("Sending Scale.png") $ do
    layout_title .= "Sending Scale Comparison"
    plotBaselines baselines ["chain-1", "patchain-1", "patricia-1", "dag-1"]
    let spec = [(v, f) | v <- zip ["0.5, 2"] (tail shapes), f <- zip labels colors]
    forM_ (zip spec atest) $ \(((v,sh), (st, c)), f)-> do
      setShapes [sh]
      setColors [c]
      plot $ points (st ++ " " ++ v) $ V.toList f