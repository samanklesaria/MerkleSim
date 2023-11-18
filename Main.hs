{-# LANGUAGE StrictData, RankNTypes #-}
module Main where
import Control.Concurrent
import Sim
import Chain
import Patricia
import PatChain (PatChain)
import Msg
import Graphics.Rendering.Chart.Easy hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Control.Monad
import Control.DeepSeq

materialize = V.fromList . uncurry zip

test v b a st = materialize <$> simulate st v b a 100 200

testAll :: (forall a. Msg a => a -> IO (Vector (Double, Double)))
  -> IO [Vector (Double, Double)]
testAll f = force <$> sequence [
  f (noMsgs :: Patricia), f (noMsgs :: Chain'), f (noMsgs :: PatChain)]

main = do
  setNumCapabilities 7
  a <- testAll $ test 0.5 0.5 1
  toFile def ("result.png") $ do
    forM_ (zip ["patricia", "chain", "patchain"] a) $ \(lbl, f)->
        plot $ points lbl $ V.toList f


-- theTests = sequence [
--   sequence [testAll $ test v 0.5 1 | v <- [0.5, 1, 2]],
--   sequence [testAll $ test 1 b 1 | b <- [0.5, 1, 2]],
--   sequence [testAll $ test 1 0.5 a | a <- [0.5, 1, 2]]]

-- main = do
--   setNumCapabilities 7
--   results <- theTests
--   forM_ (zip ["delay", "gossip", "send"] results) $ \(l, st)-> do
--     toFile def (l ++ ".png") $ do
--       forM_ (zip ["0.5", "1", "2"] st) $ \(val, strat)-> do
--         forM_ (zip ["patricia", "chain", "patchain"] strat) $ \(lbl, f)->
--           plot $ points (lbl ++ " " ++ val) $ V.toList f

