module Mixcoin.Util

( btcToSatoshis )

where

import           Network.Bitcoin.Types

newtype Satoshis = Satoshis BTC deriving (Eq, Read, Show)

btcToSatoshis :: Floating v => v -> Satoshis
btcToSatoshis = Satoshis
