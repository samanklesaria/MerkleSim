{-# LANGUAGE StrictData #-}
module Chain where
import Msg
import Control.Monad.Writer.Strict
import Util
import qualified Crypto.Hash as C

data Chain a = Null | Cons a Hash (Chain a) deriving (Show, Eq, Foldable)

type Chain' = Chain Double

cons :: Double -> Chain' -> Chain'
cons a Null = atTime a 1
cons a b@(Cons _ h _) = Cons a (mergeHash h $ block a) b

instance Msg Chain' Double where
  noMsgs = Null
  lub a Null = return a
  lub Null b = return b
  lub a@(Cons x ah xs) b@(Cons y bh ys)
      | ah == bh = return a
      | y == x = writer (cons x, Sum 1) <*>  lub xs ys
      | y > x = writer (cons y, Sum 1) <*> lub a ys
      | otherwise = writer (cons x, Sum 1) <*> lub xs b
  atTime t _ = Cons t (C.hash $ block t) Null
