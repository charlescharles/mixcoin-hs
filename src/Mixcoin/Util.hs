module Mixcoin.Util

( btcToSatoshis )

where

import qualified 

newtype Satoshis = Satoshis BTC deriving (Eq, Read, Show)


btcToSatoshis :: Float -> Satoshis
btcToSatoshis = Satoshis
