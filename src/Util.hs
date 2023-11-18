module Util where
import Basement.Block.Mutable
import Crypto.Hash.Algorithms
import qualified Crypto.Hash as C
import Data.ByteArray.Hash
import Data.Word
import Data.ByteArray
import Basement.PrimType
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)
import Test.QuickCheck

type Hash = C.Digest MD5

encode :: Double -> Word64
encode d = h where
  FnvHash64 h = fnv1a_64Hash (block d)

block :: PrimType a => a -> Block a
block d = [d]

mergeHash :: (ByteArrayAccess a, ByteArrayAccess b) => a -> b -> Hash
mergeHash a b = C.hashFinalize $ C.hashUpdate (C.hashUpdate C.hashInit a) b

newtype BinWord = BinWord {getWord :: Word64 } deriving newtype (Eq, Ord, Arbitrary)
instance Show BinWord where
    show (BinWord w) = "0x" ++ showHex w ""
    -- show (BinWord w) = "0b" ++ showIntAtBase 2 intToDigit w ""
