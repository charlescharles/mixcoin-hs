{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Mixcoin.Mix

( handleMixRequest
, startMix
)

where

import           Control.Applicative
import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import qualified Crypto.Hash.SHA256     as SHA256 (hash)
import qualified Data.Bits              as Bits (xor)
import qualified Data.ByteString        as BS
import           Data.List              (find)
import qualified Data.Map               as M
import           Mixcoin.BitcoinClient
import           Mixcoin.Common.Util
import           Mixcoin.Crypto
import           Mixcoin.Types
import           System.Random

startMix :: Mixcoin ()
startMix = populateFeeReserves >> watchForTxs

populateFeeReserves :: Mixcoin ()
populateFeeReserves = do
  feeAcct <- feeAccount <$> asks config
  cl <- client <$> asks config
  utxos <- liftIO $ getUtxosForAccount cl feeAcct
  retain <- asks retained
  liftIO $ atomically $ modifyTVar' retain (++ utxos)

handleMixRequest :: MixRequest -> Mixcoin SignedMixRequest
handleMixRequest r = do
  liftIO $ infoM "Mixcoin.Server.Mix" $ "saw a mix request: " ++ show r
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
  liftIO $ infoM "Mixcoin.Server.Mix" "signing mix request"
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

-- pop the first fee UTXO with sufficient funds
popFeeUtxo :: Mixcoin UTXO
popFeeUtxo = do
  retain <- asks retained
  feeAmt <- minerFee <$> asks config
  maybeUtxo <- liftIO $ atomically $ do
    utxos <- readTVar retain
    case find (\(i, u) -> amount u > feeAmt) (zip [1..] utxos) of
     Just (i, u) -> do
       modifyTVar' retain (removeAt i)
       return (Just u)
     Nothing -> return Nothing
  maybe (throwError $ MixcoinError "insufficient fee UTXOs") return maybeUtxo

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (succ i) xs

watchForTxs :: Mixcoin ()
watchForTxs = forever $ receiveTxs >> waitMinutes 10

receiveTxs :: Mixcoin ()
receiveTxs = do
  addrs <- asks pending >>= liftIO . fmap M.keys . readTVarIO
  liftIO $ infoM "Mixcoin.Server.Main" $ "fetching txs for addresses: " ++ show addrs
  c <- client <$> asks config
  confs <- minConfs <$> asks config
  received <- liftIO $ getReceivedForAddresses c addrs confs
  forM_ received receiveChunk

-- move chunk from pending to mixing; start mix
receiveChunk :: UTXO -> Mixcoin ()
receiveChunk u = do
  liftIO $ infoM "Mixcoin.Server.Main" $ "received chunk: " ++ show u
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
isFee prob ut req = h < p where
  nonceBs = word32ToBs $ nonce (mixReq req)
  hashBs = integerToBs $ getBigWordInteger (blockHash ut)
  xored = BS.pack $ BS.zipWith Bits.xor nonceBs hashBs
  hashed = SHA256.hash xored  -- should have length 256 bits = 32 bytes
  h = bsToInteger hashed
  p = expand256 prob

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
  liftIO $ infoM "Mixcoin.Server" $ "deferring send to " ++ show dest ++ " by " ++ show d ++ " minutes"
  liftIO (waitMinutes d)
  utxo <- popMixingUtxo
  feeUtxo <- popFeeUtxo
  cfg <- asks config
  let c		 = client cfg
      destAmt	 = chunkSize cfg
      feeAmt	 = minerFee cfg
  liftIO $ sendChunkWithFee c utxo feeUtxo dest destAmt feeAmt

  -- put the fee utxo back in with a decreased amount
  let remaining = (amount feeUtxo) - feeAmt
  retain <- asks retained
  liftIO $ atomically $ modifyTVar' retain (++ [feeUtxo {amount = remaining}])


waitMinutes :: MonadIO m => Int -> m ()
waitMinutes = liftIO . threadDelay . minutes

-- milliseconds in a minute
minutes :: Int -> Int
minutes = (* (truncate (6e7 :: Double)))
