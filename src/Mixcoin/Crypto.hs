{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Crypto

( genWarrant
, verifyWarrant
, getPrivKey

, randGen
, readTVarIO
, sha256
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
import           Mixcoin.Types
import           System.IO.Unsafe        (unsafePerformIO)

sha256 :: HashFunction
sha256 = hashFunction hashDescrSHA256

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

randGen :: TVar SystemRNG
randGen = unsafePerformIO $ (cprgCreate <$> createEntropyPool) >>= newTVarIO

getPrivKey :: IO MixcoinPrivKey
getPrivKey = return $ PrivateKey (Params 1 2 3) 4

genWarrant :: MixcoinPrivKey -> LabeledMixRequest -> IO String
genWarrant pk req = do
  g <- readTVarIO randGen
  let (sig, g') = sign g pk sha256 (encode' req)
  atomically $ writeTVar randGen g'
  return (show sig)

-- handle broken sig
verifyWarrant :: MixcoinPubKey -> String -> LabeledMixRequest -> Bool
verifyWarrant pk sig req = verify sha256 pk (read sig) (encode' req)
