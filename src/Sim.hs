module Sim where
import Data.Random
import Data.Random.Distribution.Exponential
import System.Random.SplitMix
import qualified Data.Vector.Mutable as V
import qualified Data.List.Ordered as O
import Control.Monad.Writer.Strict
import Control.Applicative
import Data.Word
import Msg
import Data.Traversable.Compat

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
    | i == j = Send i (n-1) t (t-e)
    | otherwise = Send i j t (t-e)

process :: Msg a b => b -> MVec (a, Word64) -> Action -> IO (MVec (a, Word64), Double)
process s v (Create i _ t) = do
  (a, w) <- V.read v i
  let (a', dw) = fmap getSum . runWriter $ lub a (atTime t s)
  V.write v i (a', w + dw)
  return (v, fromIntegral dw / fromIntegral (V.length v))
process _ v (Send i j _ _) = do
  (a, wa) <- V.read v i
  (b, wb) <- V.read v j
  let (!ab, !dw) = fmap getSum . runWriter $ lub a b
  V.write v i (ab, wa + dw)
  V.write v j (ab, wb + dw)
  return $ (v, 2 * fromIntegral dw / fromIntegral (V.length v))


mkCreate :: Int -> Double -> Double -> Action
mkCreate i t e = Create i t (t - e)

sends :: Double -> Double -> Int -> Int -> IO [Action]
sends v b n i = do
  times <- ZipList . poisson b <$> newSMGen
  noise <- ZipList . samples (Exp v) <$> newSMGen
  sendTo <- ZipList . samples (Uniform 0 (n-1)) <$> newSMGen
  return $ getZipList $ mkSend n i <$> sendTo <*> times <*> noise

creates :: Double -> Double -> Int -> IO [Action]
creates v a i = do
  times <- ZipList . poisson a <$> newSMGen
  noise <- ZipList . samples (Normal 0 v) <$> newSMGen
  return $ getZipList $ mkCreate i <$> times <*> noise

sampleSum :: Double -> Int -> [(Double, a)] -> [a]
sampleSum _ _ [] = []
sampleSum interval prev ((t, s):xs) = replicate (n - prev) s ++ sampleSum interval n xs where
  n = floor $ t / interval

simulate :: Msg a b => a -- | Initial state at each node
  -> Double -- | Scale of noise distribution
  -> Double -- | Scale of gossip Poisson process
  -> Double -- | Scale of message generation Poisson process
  -> Int    -- | Number of nodes
  -> Double -- | End time of simulation
  -> IO ([Double], [Double])
simulate st v b a n t = do
  sendActions <- mapM (sends v b n) ([0..n-1] :: [Int])
  createActions <- mapM (creates v a) ([0..n-1] :: [Int])
  let events = foldr O.merge [] (sendActions <> createActions)
  start <- V.replicate n (st, 0)
  let life = takeWhile (\x-> time x < t) events
  (_, counts) <- mapAccumM (process (v + b)) start life
  let interval = 1.0
  return ([0,interval..t], sampleSum interval (-1) $ zip (time <$> life)  (scanl1 (+) counts))

