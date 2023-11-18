{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Patricia where
import Data.Word
import Data.Bits
import Data.Bits.Compat
import Control.Monad.Writer.Strict
import Msg
import Util
import qualified Crypto.Hash as C
import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Exception (assert)

data Patricia = Null | Inner {
  hash :: Hash, path:: Word64, mask :: Word64,
  left::Patricia, right::Patricia} deriving (Generic, NFData)

instance Eq Patricia where
  Null == Null = True
  Null == _ = False
  _ == Null = False
  a == b = hash a == hash b && mask a == mask b && (path a .&. mask a) == (path b .&. mask b) &&
             left a == left b && right a == right b

instance Show Patricia where
  show Null = "Null"
  show p = "In {hash= " ++ show (hash p) ++ " path=" ++ show (BinWord (path p .&. mask p)) ++
           " mask=" ++ show (popCount $ mask p) ++ " l=" ++ show (left p) ++ " r=" ++
           show (right p) ++ "}"

instance Semigroup Patricia where
  a <> b = fst . runWriter $ lub a b

instance Monoid Patricia where
  mempty = Null


rightmost :: Word64 -> Word64
rightmost !a = a .&. (-a)

diffPos :: Patricia -> Patricia -> Word64
diffPos !a !b = rightmost (bitDiff .|. maskDiff) where
      bitDiff = path a `xor` path b
      maskDiff = mask a `xor` mask b

split :: Patricia -> Word64 -> (Patricia, Patricia)
split !p !pos
  | path p .&. pos == 0 = (p, Null)
  | otherwise = (Null, p)

singleton :: Word64 -> Patricia
singleton v = Inner (C.hash $ block v) v oneBits Null Null

mergeRight :: Patricia -> Patricia -> Word64 -> Counting Patricia
mergeRight !a !b !pos
  | path b .&. pos == 0 = do
      l <- lub (left a) b
      let r = right a
      writer (Inner (hash l `mergeHash` hash r) (path a) (mask a) l r, Sum 1)
  | otherwise = do
      r <- lub (right a) b
      let l = left a
      writer $ (Inner (hash l `mergeHash` hash r) (path a) (mask a) l r, Sum 1)

oneNull a b = assert (a == Null || b == Null)

instance Msg Patricia where
  noMsgs = Null
  lub Null b = return b
  lub a Null = return a
  lub a b       
      | hash a == hash b = return a
      | pos .&. mask a .&. mask b /= 0 = do -- diff in both masks
          l <- oneNull x1 x2 $ lub x1 y1
          r <- oneNull x2 y2 $ lub x2 y2
          writer (Inner (hash l `mergeHash` hash r) (path a) (pos - 1) l r, Sum 1)
      | pos .&. (mask a .|. mask b) == 0 = do -- diff not in either mask
          l <- lub (left a) (left b)
          r <- lub (right a) (right b)
          writer (Inner (hash l `mergeHash` hash r) (path a) (mask a) l r, Sum 1)
      | (pos .&. mask b /= 0) = mergeRight a b pos -- diff in b
      | (pos .&. mask a /= 0) = mergeRight b a pos -- diff in a

    where
      pos = diffPos a b
      (x1, x2) = split a pos
      (y1, y2) = split b pos
  atTime t _ = singleton (encode t)

-- There's an infinite loop somewhere.
-- 