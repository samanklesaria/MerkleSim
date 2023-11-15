module PatChainTest where
import PatChain
import Data.Set (Set)
import qualified Data.Set as S
import Data.Proxy
import Msg
import Test.QuickCheck
import Control.Monad.Writer.Strict
import qualified PatriciaTest as PT

instance Rate () where
    rate _ = 0.3

fromList :: [Double] -> PatChain ()
fromList ds = foldMap (\t-> singleton Proxy t $ atTime t) ds

prop_const_add = (toList ab, s) === (c, Sum 1) where
  (ab, s) = runWriter (lub a b)
  a = fromList $ reverse [1..10]
  b = singleton Proxy 11 (atTime 11)
  c = reverse [1..11]

fromSet :: Set Double -> PatChain ()
fromSet = fromList . S.toDescList

prop_as_set x y =  good === mine where
  good = S.toDescList (S.union x y)
  mine = toList (fst $ runWriter $ lub (fromSet x) (fromSet y))

toList :: PatChain () -> [Double]
toList Null = []
toList (Cons _ _ p c) = PT.toList p ++ toList c


-- TODO: these tests could be shared by all the different set representations

-- Problem: Patricia is current a set of Hashes. Chain is a set of Doubles. 
-- The simulation doesn't really care about the value, only the number of elements. 
-- We could put a Set of Doubles in the value slot of a Patricia tree. 
-- Or even a Maybe Double if we're assuming hash collisions never happen. 

return []
runTests = $quickCheckAll