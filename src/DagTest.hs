module DagTest where
import Dag
import Data.Proxy
import Msg
import Test.QuickCheck
import Control.Monad.Writer.Strict
import Data.List (foldl')
import qualified Data.Set as S
import Util
import Data.Word
import Control.Monad.Writer.Strict


fromList :: [Double] -> Dag
fromList = foldl' (\s t-> s <> atTime t 0.3 s) mempty 

prop_basic = total . fromList

-- Sequential insertions should keep the number of heads 1
prop_seq_head xs = counterexample (show s) (S.size s <= 1) where
  (Dag s _) = fromList xs

prop_tri = S.size s === 2 where
  s0 = atTime 0 1 mempty 
  s1 = atTime 1 1 s0
  s2 = atTime 2 1 s0
  (Dag s _) = s1 <> s2

-- prop_cost_seq_cost :: [Double] -> Property
-- prop_cost_seq_cost xs = fromIntegral (length xs) === getSum (execWriter w) where
--   w = foldM (\s t-> lub s (atTime t 0.3 s)) (mempty :: Dag) xs

return []
runTests = $quickCheckAll