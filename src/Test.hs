module Test where
import Patricia
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Test.QuickCheck
import Data.Foldable (foldMap')
import qualified Data.Foldable as F
import qualified Data.Set as S

prop_noparent = ancestor a b === (a, b, B.empty) where
  a = B.pack [0]
  b = B.pack [1]

fromFold :: Foldable t => t ByteString -> Patricia
fromFold = F.foldr insert Null

toList :: Patricia -> [ByteString]
toList Null = []
toList (Leaf a) = [a]
toList (Inner p s) = map (p <>) $ foldMap' toList s

toSet = S.fromList . map B.unpack . toList

prop_commutative x = do
  y <- shuffle x
  let a = toSet (fromFold $ map B.pack y)
      b = toSet (fromFold $ map B.pack x)
  return $ a === b

prop_dec_enc x = counterexample (show patricia) (result === x) where
  patricia = fromFold $ S.map B.pack x
  result = toSet patricia

-- Properties to test:
-- the number of operations after n inserts scales as n log n


return []
runTests = $quickCheckAll
