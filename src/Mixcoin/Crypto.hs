{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Crypto

( genWarrant
, verifyWarrant
, genPrivKey

, randGen
, readTVarIO
)

where

import           Control.Concurrent.STM
import           Crypto.PubKey.HashDescr
import           Crypto.PubKey.RSA
import           Crypto.PubKey.RSA.PKCS15
import           Crypto.Random
import           Data.Aeson               (ToJSON, encode)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack, unpack)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Functor             ((<$>))
import           Mixcoin.Types
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
    Right sig -> Just (unpack sig)


-- handle broken sig
verifyWarrant :: MixcoinPubKey -> String -> LabeledMixRequest -> Bool
verifyWarrant pk sig req = verify hashDescrSHA256 pk (pack sig) (encode' req)

