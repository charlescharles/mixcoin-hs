{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Mixcoin.Mix

( MixcoinState (..)
, MixcoinError (..)
, Config (..)
, Chunk (..)
, MixRequest (..)
, SignedMixRequest (..)
, Log
, Mixcoin
, runMixcoin
, execMixcoin
, newState
, handleMixRequest
, popMixChunk
)

where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error.Class  (MonadError, throwError)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON, (.:),
                                             (.=))
import qualified Data.Map                   as M
import           Data.Word                  (Word32)
import           Mixcoin.BitcoinClient
import           Network.Haskoin.Crypto
import           System.Random


type BlockHeight = Word32

data Config = Config
              { chunkSize :: Int
              , fee       :: Float
              , minConfs  :: Int
              , client    :: Client
              }

data MixRequest = MixRequest
                    { sendBy   :: BlockHeight
                    , returnBy :: BlockHeight
                    , nonce    :: Int
                    , outAddr  :: Address
                    } deriving (Eq, Show)

instance FromJSON MixRequest where
  parseJSON (Object v) = MixRequest <$>
         v .: "sendBy" <*>
                         v .: "returnBy" <*>
                         v .: "nonce" <*>
                         v .: "outAddr"
  parseJSON _ = mzero

data Chunk = Chunk
             { mixReq     :: !MixRequest
             , escrowAddr :: !Address
             } deriving (Eq, Show)

data SignedMixRequest = SignedMixRequest
                          { chunk   :: !Chunk
                          , warrant :: !String
                          } deriving (Eq, Show)

instance ToJSON SignedMixRequest where
  toJSON SignedMixRequest{..} = object [ "sendBy" .= (sendBy . mixReq) chunk
                                         , "returnBy" .= (returnBy . mixReq) chunk
                                         , "nonce" .= (nonce . mixReq) chunk
                                         , "outAddr" .= (outAddr . mixReq) chunk
                                         , "escrowAddr" .= escrowAddr chunk
                                         , "warrant" .= warrant ]

data MixcoinState = MixcoinState
                    { config   :: Config
                    , pending  :: TVar (M.Map Address Chunk)
                    , mixing   :: TVar [Address]
                    , retained :: TVar [Address]
                    }

newtype MixcoinError = MixcoinError String deriving (Eq, Show)

type Log = String

newtype Mixcoin a = Mixcoin
                    { runM :: EitherT MixcoinError (WriterT [Log] (ReaderT MixcoinState IO)) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader MixcoinState,
                              MonadIO, MonadWriter [Log],
                              MonadError MixcoinError)

runMixcoin :: MixcoinState -> Mixcoin a -> IO (Either MixcoinError a, [Log])
runMixcoin s = flip runReaderT s . (runWriterT . runEitherT . runM)

execMixcoin :: MixcoinState -> Mixcoin a -> IO ()
execMixcoin s m = runMixcoin s m >> return ()

newState :: Config -> IO MixcoinState
newState cfg = do
  pend <- newTVarIO M.empty
  mix <- newTVarIO []
  retained <- newTVarIO []
  return $ MixcoinState cfg pend mix retained

handleMixRequest :: MixRequest -> Mixcoin SignedMixRequest
handleMixRequest r = do
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

addToPending :: MixRequest -> Mixcoin Chunk
addToPending r = do
  escrow <- asks config >>= liftIO . getNewAddress . client
  pend <- asks pending
  let chunk = Chunk r escrow
  liftIO $ atomically $ modifyTVar' pend (M.insert escrow chunk)
  return chunk

sign :: Chunk -> Mixcoin SignedMixRequest
sign c = do
  return $ SignedMixRequest c "fake warrant"

-- randomly pop an element from the mixing TVar
popMixChunk :: Mixcoin Address
popMixChunk = do
  g <- liftIO getStdGen
  mixPool <- asks mixing
  (addr, g') <- liftIO $ atomically $ do
        addrs <- readTVar mixPool
        let n = length addrs
            (i, next) = randomR (0, n-1) g
        modifyTVar' mixPool (removeAt i)
        return (addrs !! i, next) :: STM (Address, StdGen)
  liftIO $ setStdGen g'
  return addr

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (succ i) xs
