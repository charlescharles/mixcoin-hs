{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Crypto

( genWarrant
, verifyWarrant
, readPrivKey
)

where

import           Control.Concurrent.STM
import           Crypto.PubKey.HashDescr
import           Crypto.PubKey.RSA
import           Crypto.PubKey.RSA.PKCS15
import           Crypto.Random
import           Data.Aeson               (ToJSON, encode)
import           Data.ASN1.BinaryEncoding (DER (DER))
import           Data.ASN1.Encoding       (decodeASN1', encodeASN1')
import           Data.ASN1.Object         (ASN1Object, fromASN1, toASN1)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as B64 (decode, encode)
import           Data.ByteString.Char8    (pack, unpack)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Functor             ((<$>))
import           Mixcoin.Common.Types
import           System.Directory         (getHomeDirectory)
import           System.IO.Unsafe         (unsafePerformIO)

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

randGen :: TVar SystemRNG
randGen = unsafePerformIO (newTVarIO =<< cprgCreate <$> createEntropyPool)

genPrivKey :: IO MixcoinPrivKey
genPrivKey = atomically $ do
  g <- readTVar randGen
  let ((pub, priv), g') = generate g 128 3
  writeTVar randGen g'
  return priv

genWarrant :: MixcoinPrivKey -> LabeledMixRequest -> IO (Maybe String)
genWarrant pk req = atomically $ do
  g <- readTVar randGen
  let (res, g') = signSafer g hashDescrSHA256 pk (encode' req)
  writeTVar randGen g'
  return $ case res of
    Left _ -> Nothing
    Right sig -> Just $ (unpack . B64.encode) sig

-- handle broken sig
verifyWarrant :: MixcoinPubKey -> String -> LabeledMixRequest -> Bool
verifyWarrant pk sig req = verify hashDescrSHA256 pk (pack sig) (encode' req)

encode64 :: ASN1Object a => a -> BS.ByteString
encode64 a = B64.encode . encodeASN1' DER $ (toASN1 a) []

decode64 :: ASN1Object a => BS.ByteString -> Either String a
decode64 bs = do
  dec <- B64.decode bs
  asn <- case decodeASN1' DER dec of
    Left _ -> Left "couldn't decode ASN1 stream"
    Right r -> Right r
  (ret, remaining) <- fromASN1 asn
  if null remaining
     then Right ret
     else Left "invalid ASN1 stream"

readPrivKey :: IO (Either String MixcoinPrivKey)
readPrivKey = do
  home <- (++ "/.mixcoin/mixcoin-priv.der") <$> getHomeDirectory
  bs <- BS.readFile home
  return $ decode64 bs
