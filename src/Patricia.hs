module Patricia where
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Set (Set)
import qualified Data.Set as S

data Patricia = Null | Leaf ByteString |
      Inner ByteString (Set Patricia)  deriving Show

path Null = B.empty
path (Leaf a) = a
path (Inner a _) = a

instance Eq Patricia where
  Leaf a == Leaf b = a == b
  Null == Null = True
  a == b = False

instance Ord Patricia where
  compare a b = compare (path a) (path b)

ancestor :: ByteString -> ByteString -> (ByteString, ByteString, ByteString)
ancestor a b = (l, r, B.pack $ reverse s) where
  (l, r, s) = worker a b []
  worker a@(B.uncons -> Nothing) b s = (a, b, s)
  worker a b@(B.uncons -> Nothing) s = (a, b, s)
  worker a@(B.uncons -> Just (x,xs)) b@(B.uncons -> Just (y,ys)) s
    | x == y = worker xs ys (x : s)
    | otherwise = (a, b, s)

insert :: ByteString -> Patricia -> Patricia
insert a Null = Leaf a
insert a (Leaf b) = Inner shared (S.fromList [Leaf l, Leaf r]) where
  (l, r, shared) = ancestor a b
insert a (Inner b v)
    | B.null r = Inner shared (S.insert (Leaf l) v)
    | otherwise = Inner shared (S.fromList [Leaf l, Inner r v])
  where
    (l, r, shared) = ancestor a b
