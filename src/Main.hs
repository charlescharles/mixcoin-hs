{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (forkIO)
import           Control.Monad.Reader
import           Data.Configurator
import qualified Data.Text.Lazy            as T
import           Mixcoin.BitcoinClient
import           Mixcoin.Crypto
import           Mixcoin.Mix
import           Mixcoin.Types
import           Network.Haskoin.Constants (switchToTestnet3)
import           Network.HTTP.Types        (badRequest400)
import           Web.Scotty

testConfig :: IO MixcoinConfig
testConfig = do
  c <- getClient' "http://127.0.0.1:9001" "cguo" "Thereis1"
  pkRes <- readPrivKey
  let pk = case pkRes of
   	Left e -> error e
    	Right res -> res
  return $ MixcoinConfig { chunkSize = 0.02
                         , minerFee = 0.01
                         , feeProbability = 0.002
                         , feeAccount = "mixcoin-fees"
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
  feeAcct <- require cfg "fee-account"
  btcHost <- require cfg "bitcoind-host"
  btcUser <- require cfg "bitcoind-user"
  btcPass <- require cfg "bitcoind-pass"
  client' <- getClient' btcHost btcUser btcPass
  pkRes <- readPrivKey
  let pk = case pkRes of
    	Left e -> error e
    	Right res -> res
  return $ MixcoinConfig { chunkSize = chunkSize'
              		 , minerFee = minerFee'
                         , feeProbability = feeProb
             		 , minConfs = minConfs'
                         , feeAccount = feeAcct
              		 , client = client'
                         , privKey = pk }

startLogger :: IO ()
startLogger = do
  s <- openlog "Mixcoin.Server" [] USER INFO
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "Mixcoin.Server" (setLevel INFO)

main :: IO ()
main = do
  switchToTestnet3
  startLogger
  liftIO $ infoM "Mixcoin.Server" "starting server"
  mixState <- testConfig >>= newState
  _ <- forkIO $ execMixcoin mixState startMix
  scotty 9000 (server mixState)

server :: MixcoinState -> ScottyM ()
server mstate = do
  post "/" (mixRequest mstate)

mixRequest :: MixcoinState -> ActionM ()
mixRequest mstate = do
  c <- jsonData
  res <- liftIO $ runMixcoin mstate (handleMixRequest c)
  case res of
   Left err -> do
     status badRequest400
     text $ T.pack (show err)
   Right signed -> json signed
