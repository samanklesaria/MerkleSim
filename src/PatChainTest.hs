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
import Util
import Data.Word

fromList :: [Double] -> PatChain
fromList = foldMap (\t-> atTime t 0.3 Null)

fromSet :: Set Double -> PatChain
fromSet = fromList . S.toDescList

prop_as_set x y = counterexample it $ good === mine where
  good = sort $ map (BinWord . encode) $ S.toList (S.union x y)
  mine = sort $ toList (fst $ runWriter $ lub (fromSet x) (fromSet y))
  it = show (fromSet x, fromSet y)

toList :: PatChain -> [BinWord]
toList Null = []
toList (Cons _ _ p c) = (PT.toList p) ++ toList c

return []
runTests = $quickCheckAll