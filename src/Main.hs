{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Functor              ((<$>))
import qualified Data.Map                  as M
import qualified Data.Text.Lazy            as T
import           Mixcoin.BitcoinClient
import           Mixcoin.Mix
import           Network.Haskoin.Constants (switchToTestnet3)
import           Network.Haskoin.Crypto
import           Network.HTTP.Types        (badRequest400)
import           Web.Scotty

testConfig :: IO Config
testConfig = (Config 2 0.002 1) <$> (getClient' "http://127.0.0.1:9001" "cguo" "Thereis1")

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
receiveChunk :: ReceivedChunk -> Mixcoin ()
receiveChunk c = do
  pendV <- asks pending
  mixV <- asks mixing
  pend <- liftIO $ readTVarIO pendV
  -- TODO error handling here?
  let addr = destAddr c
      Just info = M.lookup addr pend
  liftIO $ atomically $ do
    modifyTVar' pendV (M.delete addr)
    modifyTVar' mixV (addr:)

  mix info

-- generate delay, wait, send chunk
mix :: Chunk -> Mixcoin ()
mix c = do
  delay <- generateDelay c
  mstate <- ask
  let out = outAddr (mixReq c)
      send = waitSend delay out
  _ <- liftIO $ forkIO $ execMixcoin mstate send
  return ()

-- generate delay in minutes
generateDelay :: Chunk -> Mixcoin Int
generateDelay c = return 3

waitSend :: Int -> Address -> Mixcoin ()
waitSend d to = do
  liftIO (waitMinutes d)
  from <- popMixChunk
  c <- client <$> asks config
  liftIO $ sendChunk c from to

waitMinutes :: MonadIO m => Int -> m ()
waitMinutes = liftIO . threadDelay . minutes

minutes :: Int -> Int
minutes = (* (truncate 6e7))
