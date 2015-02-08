{-# LANGUAGE RecordWildCards #-}

module Mixcoin.BitcoinClient

( ReceivedChunk(..)
, Client
, getReceivedForAddresses
, getClient'
, sendChunk
, getNewAddress
)

where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8          as C8
import           Data.Functor                   ((<$>))
import           Data.Maybe                     (catMaybes)
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T
import           Data.Vector                    (fromList, toList)
import           Data.Word
import qualified Network.Bitcoin.BlockChain     as BB
import qualified Network.Bitcoin.RawTransaction as BR
import qualified Network.Bitcoin.Types          as BT
import           Network.Bitcoin.Wallet         (Client)
import qualified Network.Bitcoin.Wallet         as BW
import qualified Network.Haskoin.Crypto         as HC

data ReceivedChunk = ReceivedChunk
                     { txId        :: !HC.TxHash
                     , destAddr    :: !HC.Address
                     , blockHash   :: !HC.BlockHash
                     , blockHeight :: !Word32
                     , outIndex    :: !Word32
                     } deriving (Eq, Show, Read)

acctName :: BT.Account
acctName = T.pack "mixcoin"

getClient' :: String -> String -> String -> IO Client
getClient' h u p = BR.getClient h (C8.pack u) (C8.pack p)

getNewAddress :: Client -> IO HC.Address
getNewAddress c = (fromJust . convertAddress) <$> BW.getNewAddress c (Just acctName)

getReceivedForAddresses :: Client -> [HC.Address] -> Int -> IO [ReceivedChunk]
getReceivedForAddresses c as minConf = catMaybes <$> (unspent >>= mapM (processReceived c)) where
  unspent = toList <$> BR.listUnspent c (Just minConf) Nothing (fromList addrs)
  addrs = map convertAddress' as

processReceived :: Client -> BR.UnspentTransaction -> IO (Maybe ReceivedChunk)
processReceived c (BR.UnspentTransaction txid outidx addr _ _ _ _) = runMaybeT $ do
  txid' <- MaybeT . return $ convertTxHash txid
  addr' <- MaybeT . return $ convertAddress addr
  let outidx' = fromIntegral outidx
  rawTx <- liftIO $ BR.getRawTransactionInfo c txid
  let hash = BR.rawTxBlockHash rawTx
  hash' <- MaybeT . return $ (HC.decodeBlockHashLE . T.unpack) hash
  blk <- liftIO $ BB.getBlock c hash
  let height = fromIntegral (BB.blkHeight blk)
  return $ ReceivedChunk { txId = txid', destAddr = addr', blockHash = hash', blockHeight = height, outIndex = outidx'}

convertAddress :: BT.Address -> Maybe HC.Address
convertAddress = HC.base58ToAddr . T.unpack

-- not sure if Network.Bitcoin will take base-58 encoded
convertAddress' :: HC.Address -> BT.Address
convertAddress' = T.pack . HC.addrToBase58

convertTxHash :: BT.TransactionID -> Maybe HC.TxHash
convertTxHash = HC.decodeTxHashLE . T.unpack

sendChunk :: Client -> HC.Address -> HC.Address -> IO ()
sendChunk c from to = undefined
