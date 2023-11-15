module PatChainTest where
import PatChain
import Data.Set (Set)
import qualified Data.Set as S
import Data.Proxy
import Msg
import Test.QuickCheck
import Control.Monad.Writer.Strict
import qualified PatriciaTest as PT
import Data.List (sort)
import Patricia (BitString, encode)

instance Rate () where
    rate _ = 0.3

fromList :: [Double] -> PatChain ()
fromList ds = foldMap (\t-> singleton Proxy t $ atTime t) ds

fromSet :: Set Double -> PatChain ()
fromSet = fromList . S.toDescList

prop_as_set x y = good === mine where
  good = sort $ map encode $ S.toList (S.union x y)
  mine = sort $ toList (fst $ runWriter $ lub (fromSet x) (fromSet y))

toList :: PatChain () -> [BitString]
toList Null = []
toList (Cons _ _ p c) = PT.toList p ++ toList c

-- TODO: these tests could be shared by all the different set representations

return []
runTests = $quickCheckAll