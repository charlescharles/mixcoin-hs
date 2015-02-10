{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Crypto

( MixcoinPrivKey
, MixconPubKey
, genWarrant
, verifyWarrant
)

where

import           Crypto.PubKey.DSA
import           Crypto.PubKey.HashDescr
import           Crypto.Random
import           Data.Aeson              (ToJSON, encode)
import           Data.ByteString.Lazy    (ByteString, toStrict)
import           Mixcoin.Mix

type MixcoinPrivKey = PrivateKey

type MixcoinPubKey = PublicKey

sha256 :: HashFunction
sha256 = hashFunction hashDescrSHA256

encode' :: ToJSON a => a -> ByteString
encode' = toStrict . encode

genWarrant :: Integer -> MixcoinPrivKey -> SignedMixRequest -> Maybe String
genWarrant k pk req = signWith k pk sha256 (encode' req) >>= show

verifyWarrant :: MixcoinPubKey -> String -> LabeledMixRequest -> Bool
verifyWarrant pk sig req = verify sha256 pk sig (encode' req)
