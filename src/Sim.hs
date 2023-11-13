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
import Control.Applicative
import Patricia
import Data.Word
import Msg
import Compat

type MVec = V.IOVector

data Action = Send {from::Int, to::Int, time::Double, ptime :: Double} |
              Create {from::Int, time::Double, ptime :: Double} deriving Eq

instance Ord Action where
  compare a b = compare (time a) (time b)
 
-- | Generates an infinite Poisson process
poisson :: Double -> SMGen -> [Double]
poisson r = scanl1 (+) . samples (Exp r)

-- | Generates a stream of samples from the given dist
samples :: (RandomGen t, Distribution d a) => d a -> t -> [a]
samples d = loop where
    loop gen = t : loop gen' where
      (t, gen') = samplePure d gen

mkSend :: Int -> Int -> Int -> Double -> Double -> Action
mkSend n i j t e
    | i == j = Send i (n-1) t (t+e)
    | otherwise = Send i j t (t+e)

process :: Msg a => MVec (a, Word64) -> Action -> IO (MVec (a, Word64), Double)
process v (Create i _ t) = do
  (a, w) <- V.read v i
  let (a', dw) = fmap getSum . runWriter $ lub a (atTime t)
  V.write v i (a', w + dw)
  return (v, fromIntegral dw / fromIntegral (V.length v))
process v (Send i j _ _) = do
  (a, wa) <- V.read v i
  (b, wb) <- V.read v j
  let (ab, dw) = fmap getSum . runWriter $ lub a b
  V.write v i (ab, wa + dw)
  V.write v j (ab, wb + dw)
  return $ (ab `seq` dw) `par` (v, 2 * fromIntegral dw / fromIntegral (V.length v))

mkCreate :: Int -> Double -> Double -> Action
mkCreate i t e = Create i t (t + e)

sends :: Double -> Int -> Int -> IO [Action]
sends v n i = do
  times <- ZipList . poisson 0.5 <$> newSMGen
  noise <- ZipList . samples (Normal 0 v) <$> newSMGen
  sendTo <- ZipList . samples (Uniform 0 (n-1)) <$> newSMGen
  return $ getZipList $ mkSend n i <$> sendTo <*> times <*> noise

creates :: Double -> Int -> IO [Action]
creates v i = do
  times <- ZipList . poisson 1.0 <$> newSMGen
  noise <- ZipList . samples (Normal 0 v) <$> newSMGen
  return $ getZipList $ mkCreate i <$> times <*> noise

simulate :: Msg a => a -> Double -> Int -> Double -> IO ([Double], [Double])
simulate a v n t = do
  sendActions <- mapM (sends v n) [0..n-1]
  createActions <- mapM (creates v) [0..n-1]
  let events = foldr O.merge [] (sendActions <> createActions)
  start <- V.replicate n (a, 0)
  let life = takeWhile (\x-> time x < t) events
  (counts, _) <- mapAccumM process start life
  return (time <$> life, scanl1 (+) counts)

-- we also need to account for hash collisions.
