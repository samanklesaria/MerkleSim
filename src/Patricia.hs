{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Patricia where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits
import qualified Crypto.Hash.SHA256 as C

type BitString = Vector Bool

fromByteString :: ByteString -> BitString
fromByteString = mconcat . map blast . B.unpack

blast :: Word8 -> BitString
blast w = V.generate 8 (testBit w)

toByteString :: BitString -> ByteString
toByteString = undefined

data Patricia = Null | Inner {
  len_ :: Word64, hash_ :: BitString, path::BitString, val::Bool,
  left::Patricia, right::Patricia} deriving Show

hash Null = V.empty
hash p = hash_ p

len Null = 0
len p = len_ p

instance Semigroup Patricia where
  a <> Null = a
  Null <> b = b
  a <> b
    | V.length (path a) > V.length (path b) = merge (b,a) (path b) (path a) []
    | otherwise = merge (a,b) (path a) (path b) []

instance Monoid Patricia where
  mempty = Null

bxor:: BitString -> BitString -> BitString
bxor a b  = V.zipWith xor a b 

singleton :: BitString -> Patricia
singleton a = Inner 1 a a True Null Null

mk v l r s = Inner (len l + len r) h p v l r where
  p = V.fromList $ reverse s
  h = foldr1 bxor $ (if v then [p] else []) ++ [hash r, hash l]

merge :: (Patricia, Patricia) -- | A pair of tries
         -> BitString -- | Remaining shared bits of left tree
         -> BitString -- | Remaining shared bits of right tree
         -> [Bool] -- | Shared bits of both (backwards)
         -> Patricia
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Nothing) s = mk v l r s where
  v = val a || val b
  l = left a <> left b
  r = right a <> right b
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (True, _)) s = mk v l r s where
  v = val a
  l = left a
  r = right a <> b{path=y}
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (False, _)) s = mk v l r s where
  v = val a
  l = left a <> b{path=y}
  r = right a
merge (a,b) x@(V.uncons -> Just (p,ps)) y@(V.uncons -> Just (q,qs)) s
      | p == q = merge (a,b) ps qs (p : s)
      | p < q = mk False a{path=x} b{path=y} s
      | otherwise = mk False b{path=y} a{path=x} s
