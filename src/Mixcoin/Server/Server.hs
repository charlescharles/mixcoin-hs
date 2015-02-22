{-# LANGUAGE OverloadedStrings #-}

module Mixcoin.Server.Server

(runServer)

where

import           Control.Concurrent        (forkIO)
import           Control.Monad.Reader
import           Data.Configurator
import qualified Data.Text.Lazy            as T
import           Mixcoin.Common.BitcoinClient
import           Mixcoin.Common.Crypto
import           Mixcoin.Server.Mix
import           Mixcoin.Server.Types
import           Network.Haskoin.Constants (switchToTestnet3)
import           Network.HTTP.Types        (badRequest400)
import           Web.Scotty

testConfig :: IO ServerConfig
testConfig = do
  c <- getClient' "http://127.0.0.1:9001" "cguo" "Thereis1"
  pkRes <- readPrivKey
  let pk = case pkRes of
        Left e -> error e
        Right res -> res
  let mxConfig = MixcoinConfig { chunkSize = 0.02
                                , minerFee = 0.01
				, feeProbability = 0.002
                         	, feeAccount = "mixcoin-fees"
                         	, minConfs = 1
                         	, client = c
                         	, privKey = pk }
  return $ ServerConfig { port = 9001, mixcoinConfig = mxConfig }
  


getConfig :: IO ServerConfig
getConfig = do
  cfg <- load [Required "~/.mixcoin/server.cfg"]
  chunkSize' <- require cfg "chunk-size" :: IO BTC
  minerFee' <- require cfg "miner-fee" :: IO BTC
  feeProb <- require cfg "fee-percentage"
  minConfs' <- require cfg "min-confirmations"
  feeAcct <- require cfg "fee-account"
  port <- require cfg "mixcoin-port"
  btcHost <- require cfg "bitcoind-host"
  btcUser <- require cfg "bitcoind-user"
  btcPass <- require cfg "bitcoind-pass"
  client' <- getClient' btcHost btcUser btcPass
  pkRes <- readPrivKey
  let pk = case pkRes of
      Left e -> error e
      Right res -> res
  let mxCfg = MixcoinConfig { chunkSize = chunkSize'
                   		, minerFee = minerFee'
                         	, feeProbability = feeProb
                 		, minConfs = minConfs'
                        	, feeAccount = feeAcct
                   		, client = client'
                         	, privKey = pk }
  return $ ServerConfig { port = port', mixcoinConfig = mxCfg }

startLogger :: IO ()
startLogger = do
  s <- openlog "Mixcoin.Server" [] USER INFO
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger "Mixcoin.Server" (setLevel INFO)

runServer :: IO ()
runServer = do
  switchToTestnet3
  startLogger
  liftIO $ infoM "Mixcoin.Server" "starting server"
  cfg <- getConfig
  let mstate = newState (mixcoinConfig cfg)
  _ <- forkIO $ execMixcoin mstate startMix
  scotty (port cfg) (server mstate)

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
