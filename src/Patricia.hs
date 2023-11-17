{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Patricia where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Maybe
import Data.Word
import Data.Bits
import Control.Monad.Writer.Strict
import Msg
import qualified Crypto.Hash.MD5 as C
import Data.ByteString.Builder

type BitString = Vector Bool

encode :: Double -> BitString
encode = fromByteString . C.hashlazy . toLazyByteString . doubleBE

fromByteString :: ByteString -> BitString
fromByteString = mconcat . map blast . B.unpack

blast :: Word8 -> BitString
blast w = V.generate 8 (testBit w)

toByteString :: BitString -> ByteString
toByteString = undefined

data Patricia = Null | Inner {
  hash_ :: ByteString, path_::BitString, val::Bool,
  left::Patricia, right::Patricia} deriving Show

hash :: Patricia -> Maybe ByteString
hash Null = Nothing
hash p = Just $ hash_ p

path :: Patricia -> BitString
path Null = V.empty
path p = path_ p

setPath :: Patricia -> BitString -> Patricia
setPath Null _ = Null
setPath a b = a{path_=b}

instance Semigroup Patricia where
  a <> b = fst . runWriter $ lub a b

instance Monoid Patricia where
  mempty = Null

instance Msg Patricia where
  noMsgs = Null
  lub a b
      | hash a == hash b = return a
      | V.length (path a) > V.length (path b) = merge (b,a) (path b) (path a) []
      | otherwise = merge (a,b) (path a) (path b) []
  atTime t _ = singleton .  C.hashlazy . toLazyByteString $ doubleBE t

singleton :: ByteString -> Patricia
singleton a = Inner a (fromByteString a) True Null Null

mk :: MonadWriter (Sum Word64) m => Bool -> Patricia -> Patricia -> [Bool] -> m Patricia
mk v l r s =  writer (Inner h p v l r, Sum 1) where
  p = V.fromList $ reverse s
  h = C.finalize $ C.updates C.init $ [toByteString p | v] ++ catMaybes [hash r, hash l]

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
