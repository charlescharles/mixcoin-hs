{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Crypto

( MixcoinPrivKey
, MixcoinPubKey
, genWarrant
, verifyWarrant
)

where

import           Control.Concurrent.STM
import           Crypto.PubKey.DSA
import           Crypto.PubKey.HashDescr
import           Crypto.Random
import           Data.Aeson              (ToJSON, encode)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Functor            ((<$>))
import           Mixcoin.Mix
import           System.IO.Unsafe        (unsafePerformIO)

type MixcoinPrivKey = PrivateKey

type MixcoinPubKey = PublicKey

sha256 :: HashFunction
sha256 = hashFunction hashDescrSHA256

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

randGen :: TVar SystemRNG
randGen = unsafePerformIO $ (cprgCreate <$> createEntropyPool) >>= newTVarIO

genWarrant :: MixcoinPrivKey -> SignedMixRequest -> IO String
genWarrant pk req = do
  g <- readTVarIO randGen
  let (sig, g') = sign g pk sha256 (encode' req)
  atomically $ writeTVar randGen g'
  return (show sig)

-- handle broken sig
verifyWarrant :: MixcoinPubKey -> String -> LabeledMixRequest -> Bool
verifyWarrant pk sig req = verify sha256 pk (read sig) (encode' req)
