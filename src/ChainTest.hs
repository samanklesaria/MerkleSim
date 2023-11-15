module ChainTest where
import Chain
import Msg
import Test.QuickCheck
import Control.Monad.Writer.Strict
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable (toList)
import Sim
import Test.QuickCheck.Monadic


fromList :: [Double] -> Chain'
fromList = foldr cons Null

prop_const_add = runWriter (lub a b) === (b, Sum 1) where
  a = fromList $ reverse [1..10]
  b = cons 11 a

fromSet :: Set Double -> Chain'
fromSet = fromList . S.toDescList

prop_as_set x y =  good === mine where
  good = S.toDescList (S.union x y)
  mine = toList (fst $ runWriter $ lub (fromSet x) (fromSet y))

return []
runTests = $quickCheckAll