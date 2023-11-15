{-# LANGUAGE StrictData #-}
module Chain where
import Msg
import Control.Monad.Writer.Strict
import qualified Crypto.Hash.MD5 as C
import Data.ByteString.Builder
import Data.ByteString (ByteString)

data Chain a = Null | Cons a ByteString (Chain a) deriving (Show, Eq, Foldable)

type Chain' = Chain Double

cons :: Double -> Chain' -> Chain'
cons a Null = atTime a
cons a b@(Cons _ h _) = Cons a hash b  where
  hash = C.hashlazy . toLazyByteString  $ (doubleBE a <> byteString h)

instance Msg Chain' where
  noMsgs = Null
  lub a Null = return a
  lub Null b = return b
  lub a@(Cons x ah xs) b@(Cons y bh ys)
      | ah == bh = return a
      | y == x = writer (cons x, Sum 1) <*>  lub xs ys
      | y > x = writer (cons y, Sum 1) <*> lub a ys
      | otherwise = writer (cons x, Sum 1) <*> lub xs b
  atTime t = Cons t (C.hashlazy . toLazyByteString  $ doubleBE t) Null
