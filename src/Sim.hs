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
import Data.Word
import Msg
import Compat

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

process :: Msg a => MVec (a, Word64) -> Action -> IO (MVec (a, Word64), Double)
process v (Create i t) = do
  (a, w) <- V.read v i
  let (a', dw) = fmap getSum . runWriter $ lub a (atTime t)
  V.write v i (a', w + dw)
  return (v, fromIntegral dw / fromIntegral (V.length v))
process v (Send i j _) = do
  (a, wa) <- V.read v i
  (b, wb) <- V.read v j
  let (ab, dw) = fmap getSum . runWriter $ lub a b
  V.write v i (ab, wa + dw)
  V.write v j (ab, wb + dw)
  return $ ab `par` (v, 2 * fromIntegral dw / fromIntegral (V.length v))


mkCreate :: Int -> [Double] -> [Action]
mkCreate i times = Create i <$> times

simulate :: Msg a => a -> Int -> Double -> IO ([Double], [Double])
simulate a n t = do
  createTimes <- map poisson <$> replicateM n newSMGen
  sendTimes <- map poisson <$> replicateM n newSMGen
  sendTo <- map (unif (n-1)) <$> replicateM n newSMGen
  let creates = zipWith mkCreate [0..n-1] createTimes
      sendPairs = zipWith3 (mkSend n) [0..n-1] sendTo sendTimes
      events = foldr O.merge [] (creates <> sendPairs)
  start <- V.replicate n (a, 0)
  let times = (takeWhile (\x-> time x < t) events)
  (counts, _) <- mapAccumM process start times
  return (time <$> times, scanl1 (+) counts)

-- we also need to account for hash collisions.
-- are the number of hash collisions and the number
-- of time collisions roughly comparable?

-- It also seems like the event times aren't necessarily increasing. 