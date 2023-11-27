module Dag where
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Map.Strict (Map)
import Data.Set (Set)
import Msg
import Util
import Control.Monad.Writer.Strict
import qualified Crypto.Hash as C
import GHC.Generics (Generic)
import Control.DeepSeq

type HashSet = Set Hash
type Parents = Map Hash HashSet

-- A Dag maps node ids to parent ids
data Dag = Dag HashSet Parents deriving (Show, Eq, Generic, NFData)

instance Semigroup Dag where
  a <> b = fst . runWriter $ lub a b

instance Monoid Dag where
  mempty = Dag [] M.empty

-- Find the chains from a head in m1 to something in m2
chase :: Parents -> Parents -> HashSet -> Hash -> HashSet
chase !m1 !m2 !s !k
  | S.member k s = s
  | otherwise = case M.lookup k m2 of
      Nothing -> S.foldl' (chase m1 m2) (S.insert k s) (m1 M.! k)
      Just _ -> S.insert k s

-- Find the chains from a head in m1 to something in m2, not including the first elt
chase1 :: Parents -> Parents -> HashSet -> Hash -> HashSet
chase1 m1 m2 s k = case M.lookup k m2 of
  Nothing -> S.foldl' (chase m1 m2) s (m1 M.! k)
  Just _ -> s

singleton h (Dag l m) = Dag (S.singleton h) (M.insert h l m)

instance Msg Dag where
  noMsgs = mempty
  atTime t _ d@(Dag l _) = singleton h d where
    h = C.hashFinalize $ C.hashUpdates (C.hashUpdate C.hashInit (block t)) (S.toList l)
  lub (Dag h1 m1) (Dag h2 m2) = writer (res, Sum l) where
    s1 = S.foldl' (chase1 m1 m2) S.empty h1
    s2 = S.foldl' (chase1 m2 m1) S.empty h2
    leaves = S.difference h2 s1 <> S.difference h1 s2
    l = fromIntegral $ S.size s2 + S.size s1 + S.size h1 + S.size h2
    res = Dag leaves (m1 <> m2)

