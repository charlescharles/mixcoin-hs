{-# LANGUAGE RecordWildCards #-}

module Mixcoin.BitcoinClient where

import qualified Network.Bitcoin.Wallet as BW
import Network.Bitcoin.Wallet (Client, getClient)
import qualified Network.Bitcoin.RawTransaction as BT
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Transaction as HT
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8


getReceivedForAddresses :: Client -> [HC.Address] -> Int -> IO [HT.Tx]
getReceivedForAddresses c as minConf = 
                        

convertTx :: BT.UnspentTransaction -> HT.Tx
convertTx UnspentTransaction{..} = Tx ver ins outs lock where
  ins = [TxIn outpt scr inSeq]
  outpt = OutPoint 

convertTxHash :: BT.TransactionID -> Maybe HT.TxHash
convertTxHash = HC.decodeTxHashLE . T.unpack
