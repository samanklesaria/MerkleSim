module PatriciaTest where
import Patricia
import Test.QuickCheck hiding ((.&.))
import qualified Data.Foldable as F
import qualified Data.Set as S
import Util
import Data.Word
import Data.Bits

prop_split = path r === 0b10 where
  (r, Null) = split (singleton 0b10) 1

prop_lub_single = counterexample (show p) $ conjoin [
  path p .&. mask p === 0, path l .&. mask l === 0, path r .&. mask r === 0b10] where
  p = singleton 0b10 <> singleton 0
  l = left p
  r = right p

prop_mask = mask (singleton 0b100 <> singleton 0) === 0b11

prop_lub_dub = counterexample (show p) $ conjoin [
  path p .&. mask p === 0b11, path l .&. mask l === 0b1011, path r .&. mask r === 0b0111] where
  p = singleton 0b1011 <> singleton 0b0111
  l = left p
  r = right p

prop_zero = singleton 0 <> singleton 0 === singleton 0

prop_noroot = total $ singleton 0 <> singleton 1

prop_should = total $ fromList $ map BinWord ([0b01, 0b11, 0b10] :: [Word64])

prop_manual = x === y where
  x = foldMap (fromList . map BinWord) ([[0b0,0b111],[0b11,0b0]] :: [[Word64]])
  y = foldMap (fromList . map BinWord) ([[0b11,0b0], [0b0,0b111]] :: [[Word64]])


prop_mk :: Property
prop_mk = forAll (vector 5) (\xs-> within 5000 $ total $ fromList xs)

fromList :: [BinWord] -> Patricia
fromList = F.foldMap (singleton . getWord)

toList :: Patricia -> [BinWord]
toList Null = []
toList (Inner _ p _ Null Null) = [BinWord p]
toList (Inner _ _ _ l r) = toList l <> toList r

toSet :: Patricia -> S.Set BinWord
toSet = S.fromList . toList

prop_dec_enc :: Property
prop_dec_enc = withMaxSuccess 1000 $ forAll (vector 20) $ \x-> within 3000 $ toSet (fromList x) === S.fromList x

prop_commutative :: Property
prop_commutative = withMaxSuccess 2000 $ forAll (vectorOf 4 (vector 5)) $ \x->
  forAll (shuffle x) $ \y-> within 5000 $
    let a = foldMap fromList y
        b = foldMap fromList x
    in counterexample (show y) $ a === b

prop_idemp :: Property
prop_idemp = forAll (vectorOf 5 (vector 5)) $ \x-> within 8000 $ do
  y <- shuffle x
  let both = x ++ y
      a = foldMap fromList y
      b = foldMap fromList x
  return $ a === b

return []
runTests = $quickCheckAll
