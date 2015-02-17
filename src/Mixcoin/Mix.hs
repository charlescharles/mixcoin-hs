{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Mixcoin.Mix

( handleMixRequest
, popMixingUtxo
, popFeeUtxo
)

where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Reader
import qualified Data.Map                  as M
import           Mixcoin.BitcoinClient
import           Mixcoin.Crypto
import           Mixcoin.Types
import           System.Random


handleMixRequest :: MixRequest -> Mixcoin SignedMixRequest
handleMixRequest r = do
  validateMixRequest r
  chunk <- addToPending r
  sign chunk

ensure :: Bool -> MixcoinError -> Mixcoin ()
ensure b = unless b . throwError

validateMixRequest :: MixRequest -> Mixcoin ()
validateMixRequest MixRequest{..} = do
  cfg <- asks config

  ensure (sendBy > 1000) $ MixcoinError "invalid sendby index"
  ensure (returnBy - sendBy > fromIntegral (minConfs cfg)) $ MixcoinError "mixing period too short"

addToPending :: MixRequest -> Mixcoin LabeledMixRequest
addToPending r = do
  escrow <- asks config >>= liftIO . getNewAddress . client
  pend <- asks pending
  let labeled = LabeledMixRequest r escrow
  liftIO $ atomically $ modifyTVar' pend (M.insert escrow labeled)
  return labeled

sign :: LabeledMixRequest -> Mixcoin SignedMixRequest
sign req = do
  pk <- privKey <$> asks config
  w <- liftIO $ genWarrant pk req
  maybe (throwError $ MixcoinError "failed signature") (return . SignedMixRequest req) w

-- randomly pop an element from the mixing TVar
popMixingUtxo :: Mixcoin UTXO
popMixingUtxo = do
  g <- liftIO getStdGen
  mixPool <- asks mixing
  (addr, g') <- liftIO $ atomically $ do
        addrs <- readTVar mixPool
        let n = length addrs
            (i, g') = randomR (0, n-1) g
        modifyTVar' mixPool (removeAt i)
        return (addrs !! i, g')
  liftIO $ setStdGen g'
  return addr

popFeeUtxo :: Mixcoin UTXO
popFeeUtxo = undefined

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (succ i) xs
