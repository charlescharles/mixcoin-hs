{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map               as M
import qualified Data.Text.Lazy         as T
import           Mixcoin.BitcoinClient
import           Mixcoin.Mix
import           Mixcoin.Mix
import           Network.Bitcoin.Wallet (Client, getClient)
import qualified Network.Bitcoin.Wallet as W
import           Network.HTTP.Types     (badRequest400)
import           Web.Scotty

main :: IO ()
main = do
  mixState <- newState testConfig
  _ <- forkIO $ watchForTxs mixState
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

watchForTxs :: MixcoinState -> IO ()
watchForTxs mstate = forever $ do
  _ <- forkIO $ runMixcoin mstate receiveTxs >> return ()
  threadDelay $ minutes 10

scanTxs :: Mixcoin [Tx]
scanTxs = return []

receiveTxs :: Mixcoin ()
receiveTxs = do
  tell ["scanning for transactions"]
  txs <- scanTxs
  forM_ txs receiveChunk

receiveChunk :: PubKey -> Mixcoin ()
receiveChunk pk = do
  pendV <- asks pending
  mixV <- asks mixing
  pend <- liftIO $ readTVarIO pendV
  -- TODO error handling here?
  let Just c = M.lookup pk pend

  liftIO $ atomically $ do
    modifyTVar' pendV (M.delete pk)
    modifyTVar' mixV (pk:)

  mix c

mix :: Chunk -> Mixcoin ()
mix c = do
  delay <- generateDelay c
  _ <- liftIO $ forkIO $ waitSend delay (outAddr $ chunkRequest c)
  return ()

generateDelay :: Chunk -> Mixcoin Int
generateDelay c = return (minutes 3)

waitSend :: Int -> PubKey -> IO ()
waitSend d pk = undefined

minutes :: Int -> Int
minutes = (* (truncate 6e7))
