module Mixcoin.Common.Util

( bsToInteger
, integerToBs
, word32ToBs
, expand256
)

where

import           Data.Bits
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as Builder
import           Data.ByteString.Lazy    (toStrict)
import           Data.List               (unfoldr)
import           Data.Ratio              (denominator, numerator)
import           Data.Word               (Word32)
import           Data.Word8

word32ToBs :: Word32 -> BS.ByteString
word32ToBs = toStrict . Builder.toLazyByteString . Builder.word32BE

integerToBs :: Integer -> BS.ByteString
integerToBs = BS.pack . reverse . unfoldr (\x -> if x == 0 then Nothing else Just ((fromIntegral x :: Word8) .&. 0xff, x `shiftR` 8))

bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' f 0 where
  f acc word = (fromIntegral word) + (acc * 256)

-- times 2^256
expand256 :: Float -> Integer
expand256 k = num * 2^(256 :: Integer) `div` denom where
  num = numerator rat
  denom = denominator rat
  rat = toRational k
