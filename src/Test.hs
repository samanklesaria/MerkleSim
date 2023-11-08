module Test where
import Patricia
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import Test.QuickCheck
import Data.Foldable (foldMap')
import qualified Data.Foldable as F
import qualified Data.Set as S

fromFold :: Foldable t => t BitString -> Patricia
fromFold = F.foldMap singleton

toList :: Patricia -> [BitString]
toList Null = []
toList (Inner _ p True l r) = p : map (p <>) (toList l ++ toList r)
toList (Inner _ p False l r) = map (p <>) (toList l ++ toList r)

toSet = S.fromList . toList

prop_commutative x = do
  y <- shuffle x
  let a = toSet (fromFold $ map V.fromList y)
      b = toSet (fromFold $ map V.fromList x)
  return $ a === b

prop_dec_enc x = counterexample (show patricia) (result === y) where
  y = S.map V.fromList x
  patricia = fromFold y
  result = toSet patricia

prop_idemp (map V.fromList -> x) = do
  y <- shuffle x
  let both = x ++ y
      a = toSet $ fromFold both
      b = toSet $ fromFold x
  return $ a === b

return []
runTests = $quickCheckAll
