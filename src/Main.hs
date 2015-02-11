{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Crypto.Hash.SHA256        as SHA256 (hash)
import           Data.Configurator
import           Data.Functor              ((<$>))
import qualified Data.Map                  as M
import qualified Data.Text.Lazy            as T
import           Mixcoin.BitcoinClient
import           Mixcoin.Crypto
import           Mixcoin.Mix
import           Mixcoin.Types
import           Network.Haskoin.Constants (switchToTestnet3)
import           Network.Haskoin.Crypto
import           Network.HTTP.Types        (badRequest400)
import           System.IO                 (Handle, stderr)
import           Web.Scotty


testConfig :: IO MixcoinConfig
testConfig = do
  c <- getClient' "http://127.0.0.1:9001" "cguo" "Thereis1"
  pk <- getPrivKey
  return $ MixcoinConfig { chunkSize = 0.02
                         , minerFee = 0.01
                         , feeProbability = 0.002
                         , minConfs = 1
                         , client = c
                         , privKey = pk }

getConfig :: IO MixcoinConfig
getConfig = do
  cfg <- load [Required "~/.mixcoin/server.cfg"]
  chunkSize' <- require cfg "chunk-size" :: IO BTC
  minerFee' <- require cfg "miner-fee" :: IO BTC
  feeProb <- require cfg "fee-percentage"
  minConfs' <- require cfg "min-confirmations"
  btcHost <- require cfg "bitcoind-host"
  btcUser <- require cfg "bitcoind-user"
  btcPass <- require cfg "bitcoind-pass"
  client' <- getClient' btcHost btcUser btcPass
  pk <- getPrivKey
  return $ MixcoinConfig { chunkSize = chunkSize'
              		 , minerFee = minerFee'
                         , feeProbability = feeProb
             		 , minConfs = minConfs'
              		 , client = client'
                         , privKey = pk }

main :: IO ()
main = do
  switchToTestnet3
  mixState <- testConfig >>= newState
  _ <- forkIO $ execMixcoin mixState watchForTxs
  scotty 9000 (server mixState)

server :: MixcoinState -> ScottyM ()
server mstate = do
  post "/" (mixRequest mstate)

mixRequest :: MixcoinState -> ActionM ()
mixRequest mstate = do
  c <- jsonData
  (res, _) <- liftIO $ runMixcoin mstate (handleMixRequest c)
  case res of
   Left err -> do
     status badRequest400
     text $ T.pack (show err)
   Right signed -> json signed

watchForTxs :: Mixcoin ()
watchForTxs = forever $ receiveTxs >> waitMinutes 10

receiveTxs :: Mixcoin ()
receiveTxs = do
  tell ["scanning for transactions"]
  addrs <- asks pending >>= liftIO . fmap M.keys . readTVarIO
  c <- client <$> asks config
  confs <- minConfs <$> asks config
  received <- liftIO $ getReceivedForAddresses c addrs confs
  forM_ received receiveChunk

-- move chunk from pending to mixing; start mix
receiveChunk :: UTXO -> Mixcoin ()
receiveChunk u = do
  pend <- asks pending >>= liftIO . readTVarIO
  -- TODO error handling here?
  let addr = destAddr u
      Just info = M.lookup addr pend
  feeProb <- feeProbability <$> asks config
  if isFee feeProb u info
     then addToRetained addr u
     else addToMixing addr u >> mix info

addToMixing :: Address -> UTXO -> Mixcoin ()
addToMixing addr u = do
  mixPool <- asks mixing
  moveToPool mixPool addr u

addToRetained :: Address -> UTXO -> Mixcoin ()
addToRetained addr u = do
  retain <- asks retained
  moveToPool retain addr u

-- delete addr from pending, cons UTXO onto dest pool
moveToPool :: TVar [UTXO] -> Address -> UTXO -> Mixcoin ()
moveToPool dest addr ut = do
  pend <- asks pending
  liftIO . atomically $ do
    modifyTVar' pend (M.delete addr)
    modifyTVar' dest (ut:)

-- hash nonce || blockhash
isFee :: Float -> UTXO -> LabeledMixRequest -> Bool
isFee prob ut req = False where
  non = nonce (mixReq req)
  hash = blockHash ut

-- generate delay, wait, send chunk
mix :: LabeledMixRequest -> Mixcoin ()
mix c = do
  delay <- generateDelay c
  mstate <- ask
  let out = outAddr (mixReq c)
      send = waitSend delay out
  _ <- liftIO $ forkIO $ execMixcoin mstate send
  return ()

-- generate delay in minutes
generateDelay :: LabeledMixRequest -> Mixcoin Int
generateDelay c = return 3

waitSend :: Int -> Address -> Mixcoin ()
waitSend d dest = do
  liftIO (waitMinutes d)
  utxo <- popMixingUtxo
  feeUtxo <- popFeeUtxo
  cfg <- asks config
  let c = client cfg
      destAmt = chunkSize cfg
      feeAmt = minerFee cfg
  liftIO $ sendChunkWithFee c utxo feeUtxo dest destAmt feeAmt

waitMinutes :: MonadIO m => Int -> m ()
waitMinutes = liftIO . threadDelay . minutes

-- milliseconds in a minute
minutes :: Int -> Int
minutes = (* (truncate (6e7 :: Double)))
