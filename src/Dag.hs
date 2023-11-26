module Dag where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Map.Strict (Map)
import Data.Set (Set)
import Msg
import Util
import Control.Monad.Writer.Strict
import qualified Crypto.Hash as C

type HashSet = Set Hash
type Parents = Map Hash HashSet

-- A Dag maps node ids to parent ids
data Dag = Dag HashSet Parents deriving (Show, Eq)

instance Semigroup Dag where
  a <> b = fst . runWriter $ lub a b

instance Monoid Dag where
  mempty = Dag [] M.empty

-- Find the chains from a head in m1 to something in m2
chase :: Parents -> Parents -> HashSet -> Hash -> HashSet
chase m1 m2 s k
  | S.member k s = s
  | otherwise = case M.lookup k m2 of
      Nothing -> S.foldl' (chase m1 m2) (S.insert k s) (m1 M.! k)
      Just _ -> s

-- Find the chains from a head in m1 to something in m2
chase1 :: Parents -> Parents -> HashSet -> Hash -> HashSet
chase1 m1 m2 s k = case M.lookup k m2 of
  Nothing -> S.foldl' (chase m1 m2) s (m1 M.! k)
  Just _ -> s

singleton h l = Dag (S.singleton h) (M.singleton h l)  where

instance Msg Dag HashSet where
  noMsgs = mempty
  atTime t l = singleton h l where
    h = C.hashFinalize $ C.hashUpdates (C.hashUpdate C.hashInit (block t)) (S.toList l)
  lub (Dag h1 m1) (Dag h2 m2) = writer (res, Sum l) where
    s1 = S.foldl' (chase1 m1 m2) S.empty h1
    s2 = S.foldl' (chase1 m2 m1) s1 h2
    leaves = S.difference (h1 <> h2) s2
    l = fromIntegral $ S.size s2
    res = Dag leaves (m1 <> m2)

