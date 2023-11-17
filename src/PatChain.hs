{-# LANGUAGE StrictData #-}
module PatChain where
import Patricia (Patricia, hash_)
import Msg
import Control.Monad.Writer.Strict
import qualified Crypto.Hash.SHA256 as C
import Data.ByteString.Builder
import Data.ByteString (ByteString)

data PatChain = Null | Cons Int ByteString Patricia PatChain

cons :: Int -> Patricia -> PatChain -> PatChain
cons t p Null = Cons t (hash_ p) p Null
cons t p c@(Cons _ h _ _) = Cons t hash p c where
  hash = C.hashlazy . toLazyByteString $ byteString h <> byteString (hash_ p)

instance Msg PatChain where
  noMsgs = Null
  lub a Null = return a
  lub Null b = return b
  lub a@(Cons t1 b1 p1 xs) b@(Cons t2 b2 p2 ys)
      | b1 == b2 = return a
      | t1 == t2 = cons t1 <$> lub p1 p2 <*> lub xs ys
      | t1 > t2 = writer (cons t1 p1, Sum 1) <*> lub xs b
      | otherwise = writer (cons t2 p2, Sum 1) <*> lub ys a
  atTime t s = Cons c (hash_ p) p Null where
    p = atTime t s
    c = ceiling (t / s)

instance Semigroup PatChain where
  a <> b = fst . runWriter $ lub a b

instance Monoid PatChain where
  mempty = Null