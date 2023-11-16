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
import qualified PatriciaSha as PS

import Criterion.Main

patriciaRun = do
  (times, avgs) <- simulate (noMsgs :: Patricia) 1 80 120
  return avgs

patriciaShaRun = do
  (times, avgs) <- simulate (noMsgs :: PS.Patricia) 1 80 120
  return avgs

chainRun = do
  (times, avgs) <- simulate (noMsgs :: Chain') 1 80 120
  return avgs

chainLo = do
  (times, avgs) <- simulate (noMsgs :: Chain') 0.001 80 120
  return avgs

main = do
  setNumCapabilities 7
  defaultMain [
    bench "patriciaMD5"  $ nfIO patriciaRun,
    bench "patriciaSha"  $ nfIO patriciaShaRun,
    bench "chain"  $ nfIO chainRun,
    bench "chainLo"  $ nfIO chainLo]