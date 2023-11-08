{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Patricia where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits

type BitString = Vector Bool

fromByteString :: ByteString -> BitString
fromByteString = V.fromList . build where
   build (B.uncons -> Nothing) = []
   build (B.uncons -> Just (x, xs)) = blast x ++ build xs

blast :: Word8 -> [Bool]
blast w = map (testBit w) [0..8]

data Patricia = Null | Inner {
  path::BitString, val::Bool,
  left::Patricia, right::Patricia} deriving Show

instance Semigroup Patricia where
  a <> Null = a
  Null <> b = b
  a <> b
    | V.length (path a) > V.length (path b) = merge (b,a) (path b) (path a) []
    | otherwise = merge (a,b) (path a) (path b) []

instance Monoid Patricia where
  mempty = Null

singleton :: BitString -> Patricia
singleton a = Inner a True Null Null

merge :: (Patricia, Patricia) -- | A pair of tries
         -> BitString -- | Remaining shared bits of left tree
         -> BitString -- | Remaining shared bits of right tree
         -> [Bool] -- | Shared bits of both (backwards)
         -> Patricia
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Nothing) s =
  Inner (V.fromList $ reverse s) (val a || val b)
    (left a <> left b) (right a <> right b)
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (True, _)) s =
  Inner (V.fromList $ reverse s) (val a) (left a) (right a <> b{path=y})
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (False, _)) s =
  Inner (V.fromList $ reverse s) (val a) (left a <> b{path=y}) (right a)
merge (a,b) x@(V.uncons -> Just (p,ps)) y@(V.uncons -> Just (q,qs)) s
      | p == q = merge (a,b) ps qs (p : s)
      | p < q = Inner (V.fromList $ reverse s) False a{path=x} b{path=y}
      | otherwise = Inner (V.fromList $ reverse s) False b{path=y} a{path=x}
