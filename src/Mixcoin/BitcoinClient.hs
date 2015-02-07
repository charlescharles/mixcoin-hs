{-# LANGUAGE RecordWildCards #-}

module Mixcoin.BitcoinClient

( ReceivedChunk(..)
, getReceivedForAddresses
)

where

import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString.Char8          as C8
import           Data.Functor
import           Data.Maybe                     (fromJust)
import qualified Data.Text                      as T
import           Data.Vector                    (fromList, toList)
import           Data.Word
import qualified Network.Bitcoin.BlockChain     as BB
import qualified Network.Bitcoin.RawTransaction as BR
import qualified Network.Bitcoin.Types          as BT
import           Network.Bitcoin.Wallet         (Client)
import qualified Network.Haskoin.Crypto         as HC

data ReceivedChunk = ReceivedChunk
                     { txId        :: !HC.TxHash
                     , toAddr      :: !HC.Address
                     , blockHash   :: !HC.BlockHash
                     , blockHeight :: !Word32
                     , outIndex    :: !Word32
                     } deriving (Eq, Show, Read)

getReceivedForAddresses :: Client -> [HC.Address] -> Int -> IO [ReceivedChunk]
getReceivedForAddresses c as minConf = mapM (processReceived c) =<< toList <$> unspent where
  unspent = BR.listUnspent c (Just minConf) Nothing (fromList addrs)
  addrs = map convertAddress' as

processReceived :: Client -> BR.UnspentTransaction -> IO ReceivedChunk
processReceived c (BR.UnspentTransaction txid outidx addr _ _ _ _) = do
  let txid' = convertTxHash txid
      outidx' = fromIntegral outidx
      addr' = convertAddress addr
  rawTx <- BR.getRawTransactionInfo c txid
  let hash = BR.rawTxBlockHash rawTx
      hash' = (fromJust . HC.decodeBlockHashLE . T.unpack) hash -- TODO handle Nothing
  blk <- BB.getBlock c hash
  let height = fromIntegral (BB.blkHeight blk)
  return $ ReceivedChunk { txId = txid', toAddr = addr', blockHash = hash', blockHeight = height, outIndex = outidx'}

convertAddress :: BT.Address -> HC.Address
convertAddress = fromJust . HC.base58ToAddr . C8.unpack . HC.encodeBase58 . fst . B16.decode . C8.pack . T.unpack

-- not sure if Network.Bitcoin will take base-58 encoded
convertAddress' :: HC.Address -> BT.Address
convertAddress' = T.pack . HC.addrToBase58

-- handle Nothing case
convertTxHash :: BT.TransactionID -> HC.TxHash
convertTxHash = fromJust . HC.decodeTxHashLE . T.unpack
