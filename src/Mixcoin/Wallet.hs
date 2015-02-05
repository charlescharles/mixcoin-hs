{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mixcoin.Wallet where
(
  Wallet (..)
, WalletHandler
, newWallet
)

import qualified Data.Text.Lazy         as T
import           Network.Haskoin.Crypto
import qualified Data.ByteString as BS
import qualified Control.Monad.State as S

data Wallet = Wallet
              { walletName   :: !T.Text
              , walletMaster :: !MasterKey
              } deriving (Eq, Show, Read)
              
newtype WalletHandler a = S.StateT Wallet IO a
                          deriving (MonadIO)

newWallet :: (MonadIO m) => WalletName -> BS.ByteString -> m Wallet
newWallet = undefined

