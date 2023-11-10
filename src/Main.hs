module Main where
import Control.Concurrent
import Sim
import Patricia
import Data.Bits
import qualified Crypto.Hash.SHA256 as C
import qualified Data.ByteString as B
import Data.ByteString.Builder
import qualified Data.Vector as V
import Control.Monad.Writer.Strict
import qualified Control.Foldl as F
import Msg

main = do
  setNumCapabilities 7
  v <- simulate 2
  return . F.fold F.mean $ V.map (fromIntegral . getSum . execWriter)
    (v :: V.Vector (Counting Patricia))