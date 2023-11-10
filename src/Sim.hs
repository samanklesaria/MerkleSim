module Sim where
import Data.Random
import Data.Random.Distribution.Exponential
import System.Random.SplitMix
import qualified Data.Vector.Mutable as V
import Data.Vector (unsafeFreeze, Vector)
import qualified Data.List.Ordered as O
import Control.Monad
import Control.Parallel
import Control.Monad.Writer.Strict
import Patricia
import Msg

type MVec = V.IOVector

data Action = Send {from::Int, to::Int, time::Double} |
              Create {from::Int, time::Double} deriving Eq

instance Ord Action where
  compare a b = compare (time a) (time b)
 
-- | Generates an infinite Poisson process
poisson :: SMGen -> [Double]
poisson = scanl1 (+) . loop where
  loop gen = t : loop gen' where
    (t, gen') = samplePure (Exp 0.2) gen

unif :: Int -> SMGen -> [Int]
unif n gen = t : unif n gen' where
  (t, gen') = samplePure (Uniform 0 (n-1)) gen

mkSend :: Int -> Int -> [Int] -> [Double] -> [Action]
mkSend n_ i_= zipWith (inner n_ i_) where
  inner n i j t
    | i == j = Send i (n-1) t
    | otherwise = Send i j t

process :: Msg a => MVec (Counting a) -> Action -> IO (MVec (Counting a))
process v (Create i t) = V.modify v (\a-> join (lub <$> a <*> atTime t)) i >> return v
process v (Send i j _) = do
  (a, wa) <- runWriter <$> V.read v i
  (b, wb) <- runWriter <$> V.read v j
  let ab = lub a b
  V.write v i (tell wa >> ab)
  V.write v j (tell wb >> ab)
  return (ab `par` v)

mkCreate :: Int -> [Double] -> [Action]
mkCreate i times = Create i <$> times

simulate :: Msg a => Int -> IO (Vector (Counting a))
simulate n = do
  createTimes <- map poisson <$> replicateM n newSMGen
  sendTimes <- map poisson <$> replicateM n newSMGen
  sendTo <- map (unif (n-1)) <$> replicateM n newSMGen
  let creates = zipWith mkCreate [0..n-1] createTimes
      sendPairs = zipWith3 (mkSend n) [0..n-1] sendTo sendTimes
      events = O.mergeAll (creates <> sendPairs)
  start <- V.replicate n noMsgs
  mvec <- foldM process start (takeWhile (\x-> time x < 5) events)
  unsafeFreeze mvec
