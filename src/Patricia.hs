{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Patricia where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Bits
import Control.Monad.Writer

type BitString = Vector Bool

fromByteString :: ByteString -> BitString
fromByteString = mconcat . map blast . B.unpack

blast :: Word8 -> BitString
blast w = V.generate 8 (testBit w)

toByteString :: BitString -> ByteString
toByteString = undefined

data Patricia = Null | Inner {
  len_ :: Word64, hash_ :: BitString, path_::BitString, val::Bool,
  left::Patricia, right::Patricia} deriving Show

hash :: Patricia -> Vector Bool
hash Null = V.empty
hash p = hash_ p

path :: Patricia -> BitString
path Null = V.empty
path p = path_ p

setPath :: Patricia -> BitString -> Patricia
setPath Null _ = Null
setPath a b = a{path_=b}

len :: Patricia -> Word64
len Null = 0
len p = len_ p

instance Semigroup Patricia where
  a <> b = fst . runWriter $ lub a b

lub :: Patricia -> Patricia -> Counting Patricia
lub a b
    | V.length (path a) > V.length (path b) = merge (b,a) (path b) (path a) []
    | otherwise = merge (a,b) (path a) (path b) []

instance Monoid Patricia where
  mempty = Null

type Counting = Writer (Sum Word64) 

bxor:: BitString -> BitString -> BitString
bxor = V.zipWith xor

singleton :: BitString -> Patricia
singleton a = Inner 1 a a True Null Null

mk :: Monad m => Bool -> Patricia -> Patricia -> [Bool] -> m Patricia
mk v l r s = return $ Inner (len l + len r) h p v l r where
  p = V.fromList $ reverse s
  h = foldr1 bxor $ [p | v] ++ [hash r, hash l]

merge :: (Patricia, Patricia) -- | A pair of tries
         -> BitString -- | Remaining shared bits of left tree
         -> BitString -- | Remaining shared bits of right tree
         -> [Bool] -- | Shared bits of both (backwards)
         -> Counting Patricia
merge (Null, a) _ _ _ = return a
merge (a, Null) _ _ _ = return a

merge (a,b) (V.uncons -> Nothing) (V.uncons -> Nothing) s = do
  l <- left a `lub` left b
  r <- right a `lub` right b
  mk (val a || val b) l r s
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (True, _)) s = do
  r <- right a `lub` setPath b y
  mk (val a) (left a) r s
merge (a,b) (V.uncons -> Nothing) y@(V.uncons -> Just (False, _)) s = do
  l <- left a `lub` setPath b y
  mk (val a) l (right a) s 
merge (a,b) x@(V.uncons -> Just (p,ps)) y@(V.uncons -> Just (q,qs)) s
      | p == q = merge (a,b) ps qs (p : s)
      | p < q = mk False a{path_=x} (setPath b y) s
      | otherwise = mk False (setPath b y) (setPath a x) s
