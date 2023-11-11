{-# LANGUAGE StrictData #-}
module TimeTrie where

data TimeTrie = Null | TimeTrie {
  from::Double, mid::Double, to::Double,
  left::TimeTrie, right::TimeTrie} deriving Show

matchTimes :: TimeTrie -> TimeTrie -> (Ordering, TimeTrie -> TimeTrie -> TimeTrie)
matchTimes = undefined

instance Semigroup TimeTrie where
  a <> Null = a
  Null <> b = b
  a <> b = case matchTimes a b of
            (EQ, f) -> f (left a <> left b) (right a <> right b)
            (LT, f) -> f (left a <> b) (right a)
            (GT, f) -> f (left a) (b <> right a)

instance Monoid TimeTrie where
    mempty = Null