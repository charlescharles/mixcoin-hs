{-# LANGUAGE RecordWildCards #-}

module Mixcoin.Common.BitcoinClient

( UTXO (..)
, Client
, BTC
, getReceivedForAddresses
, getClient'
, sendChunkWithFee
, getNewAddress
, getUtxosForAccount
, getChainHeight
)

where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8     as C8
import           Data.Functor              ((<$>))
import           Data.Maybe                (catMaybes, fromJust)
import qualified Data.Text                 as T
import           Data.Vector               (fromList, toList)
import           Mixcoin.Common.Types
import qualified Network.Bitcoin           as B
import qualified Network.Haskoin.Crypto    as H

acctName :: T.Text
acctName = T.pack "mixcoin"

getClient' :: String -> String -> String -> IO Client
getClient' h u p = B.getClient h (C8.pack u) (C8.pack p)

getNewAddress :: Client -> IO H.Address
getNewAddress c = (fromJust . convertAddress) <$> B.getNewAddress c (Just acctName)

getReceivedForAddresses :: Client -> [H.Address] -> Int -> IO [UTXO]
getReceivedForAddresses c as minConf = catMaybes . asked <$> (unspent >>= toUtxos) where
  unspent = toList <$> B.listUnspent c (Just minConf) Nothing (fromList addrs)
  -- asked filters out the `Maybe UTXO`s that we didn't ask for
  asked = filter (maybe False (\ut -> (destAddr ut) `elem` as))
  toUtxos = mapM (processReceived c)
  addrs = map convertAddress' as

-- turn UnspentTransaction into UTXO, fetching additional info along the way
processReceived :: Client -> B.UnspentTransaction -> IO (Maybe UTXO)
processReceived c ut@(B.UnspentTransaction txid outidx addr _ _ amt _) = runMaybeT $ do
  addr' <- MaybeT . return $ convertAddress addr
  let outidx' = fromIntegral outidx
  rawTx <- liftIO $ B.getRawTransactionInfo c txid
  let hash = B.rawTxBlockHash rawTx
  hash' <- MaybeT . return $ (H.decodeBlockHashLE . T.unpack) hash
  blk <- liftIO $ B.getBlock c hash
  let height = fromIntegral (B.blkHeight blk)
  return $ UTXO { unspentTx = ut, destAddr = addr', blockHash = hash', blockHeight = height, outIndex = outidx', amount = amt}

convertAddress :: B.Address -> Maybe H.Address
convertAddress = H.base58ToAddr . T.unpack

convertAddress' :: H.Address -> B.Address
convertAddress' = T.pack . H.addrToBase58

sendChunkWithFee :: Client -> UTXO -> UTXO -> H.Address -> BTC -> BTC -> IO ()
sendChunkWithFee c ut feeUt dest destAmt feeAmt = do
  let dest' = convertAddress' dest
      feeDest = convertAddress' (destAddr feeUt)
      utxos = [unspentTx ut, unspentTx feeUt]
      outs = [(dest', destAmt), (feeDest, feeAmt)]
  raw <- B.createRawTransaction c (fromList utxos) (fromList outs)
  signed <- B.signRawTransaction c raw Nothing Nothing Nothing
  _ <- B.sendRawTransaction c (B.rawSigned signed)
  return ()

getUtxosForAccount :: Client -> String -> IO [UTXO]
getUtxosForAccount c acct = do
  addrsVec <- B.getAddressesByAccount c (T.pack acct)
  let addrs = (catMaybes . map convertAddress . toList) addrsVec
  getReceivedForAddresses c addrs 1

getChainHeight :: Client -> IO BlockHeight
getChainHeight = fmap fromIntegral . B.getBlockCount
