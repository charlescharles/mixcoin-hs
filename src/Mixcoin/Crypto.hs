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
randGen = unsafePerformIO (newTVarIO =<< cprgCreate <$> createEntropyPool)

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

testParams = Params 13232376895198612407547930718267435757728527029623408872245156039757713029036368719146452186041204237350521785240337048752071462798273003935646236777459223 5421644057436475141609648488325705128047428394380474376834667300766108262613900542681289080713724597310673074119355136085795982097390670890367185141189796 857393771208094202104259627990318636601332086981

testPrivKey = do
  g <- readTVarIO randGen
  (pn, g') = generatePrivate g testParams
  atomically $ writeTVarIO randGen g'
  return PrivateKey testParams pn
