module Msg where
import Data.Monoid
import Control.Monad.Writer.Strict
import Data.Word

type Counting = Writer (Sum Word64) 

class Msg a where
  noMsgs :: Counting a
  atTime :: Double -> Counting a
  lub :: a -> a -> Counting a