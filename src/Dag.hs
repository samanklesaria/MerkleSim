module Dag where
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.Word
import Msg
import Util
import Control.Monad.Writer.Strict

-- A Dag maps node ids to parent ids
data Dag = Dag IntSet (IntMap IntSet) deriving (Show, Eq)

instance Semigroup Dag where
  a <> b = fst . runWriter $ lub a b

instance Monoid Dag where
  mempty = Dag [] M.empty

-- Find the chains from a head in m1 to something in m2
chase :: IntMap IntSet -> IntMap IntSet -> IntSet -> Int -> IntSet
chase m1 m2 s k
  | S.member k s = s
  | otherwise = case M.lookup k m2 of
      Nothing -> S.foldl' (chase m1 m2) (S.insert k s) (m1 M.! k)
      Just _ -> s

-- Find the chains from a head in m1 to something in m2
chase1 :: IntMap IntSet -> IntMap IntSet -> IntSet -> Int -> IntSet
chase1 m1 m2 s k = case M.lookup k m2 of
  Nothing -> S.foldl' (chase m1 m2) s (m1 M.! k)
  Just _ -> s

singleton :: Word64 -> IntSet -> Dag
singleton w s = S.singleton i where
  i = fromIntegral w


-- Seems like we should actually use ordinary Maps and Sets,
-- and use Digests instead of Word64 keys.

instance Msg Dag IntSet where
  noMsgs = mempty
  atTime t leaves = singleton (encode t) leaves
  lub (Dag h1 m1) (Dag h2 m2) = writer (res, Sum l) where
    s1 = S.foldl' (chase1 m1 m2) S.empty h1
    s2 = S.foldl' (chase1 m2 m1) s1 h2
    leaves = S.difference (h1 <> h2) s2
    l = fromIntegral $ S.size s2
    res = Dag leaves (m1 <> m2)

