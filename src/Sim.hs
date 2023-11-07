module Sim where
import Data.Random
import Data.Random.Distribution.Exponential
import System.Random.SplitMix
import qualified Data.Vector.Mutable as V
import Data.Vector (unsafeFreeze, Vector)
import qualified Data.List.Ordered as O
import Control.Monad
import Data.Semigroup
import Control.Parallel
import Control.Concurrent
import System.IO.Unsafe

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
mkSend n i js ts = zipWith (inner n i) js ts where
  inner n i j t
    | i == j = Send i (n-1) t
    | otherwise = Send i j t

class Semigroup a => Msg a where
  atTime :: Double -> a

process :: Msg a => MVec a -> Action -> IO (MVec a)
process v (Create i t) = V.modify v (<> atTime t) i >> return v
process v (Send i j _) = do
  result <- (<>) <$> V.read v i <*> V.read v j
  V.write v i result >> V.write v j result
  return (result `par` v)

mkCreate :: Int -> [Double] -> [Action]
mkCreate i times = Create i <$> times

simulate :: Msg a => Int -> a -> IO (Vector a)
simulate n init = do
  createTimes <- map poisson <$> replicateM n newSMGen
  sendTimes <- map poisson <$> replicateM n newSMGen
  sendTo <- map (unif (n-1)) <$> replicateM n newSMGen
  let creates = zipWith mkCreate [0..n-1] createTimes
      sendPairs = zipWith3 (mkSend n) [0..n-1] sendTo sendTimes
      events = O.mergeAll (creates <> sendPairs)
  start <- V.replicate n init
  mvec <- foldM process start (takeWhile (\x-> time x < 5) events)
  unsafeFreeze mvec
