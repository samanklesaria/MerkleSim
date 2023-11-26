module Msg where
import Data.Monoid
import Control.Monad.Writer.Strict
import Data.Word

type Counting = Writer (Sum Word64) 

class Msg a b | a -> b where
  noMsgs :: a
  atTime :: Double -> b -> a
  lub :: a -> a -> Counting a
