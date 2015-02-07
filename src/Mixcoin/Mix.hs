{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Mixcoin.Mix

( MixcoinState (..)
, MixcoinError (..)
, Chunk (..)
, MixRequest (..)
, SignedMixRequest (..)
, PubKey
, Log
, Mixcoin
, runMixcoin
, newState
, handleMixRequest
, testConfig
, Tx
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

type Tx = String

type PubKey = String

type BlockHeight = Word32

data Config = Config
              { chunkSize :: Int
              , fee       :: Float
              , minConfs  :: Int
              } deriving (Eq, Show)

testConfig :: Config
testConfig = Config 100 0.02 1

data MixRequest = MixRequest
                    { sendBy   :: BlockHeight
                    , returnBy :: BlockHeight
                    , nonce    :: Int
                    , outAddr  :: PubKey
                    } deriving (Eq, Show)

instance FromJSON MixRequest where
  parseJSON (Object v) = MixRequest <$>
         v .: "sendBy" <*>
                         v .: "returnBy" <*>
                         v .: "nonce" <*>
                         v .: "outAddr"
  parseJSON _ = mzero

data Chunk = Chunk
             { mixRequest :: !MixRequest
             , escrowAddr :: !PubKey
             } deriving (Eq, Show)

data SignedMixRequest = SignedMixRequest
                          { chunk   :: !Chunk
                          , warrant :: !PubKey
                          } deriving (Eq, Show)

instance ToJSON SignedMixRequest where
  toJSON SignedMixRequest{..} = object [ "sendBy" .= (sendBy . mixRequest) chunk
                                         , "returnBy" .= (returnBy . mixRequest) chunk
                                         , "nonce" .= (nonce . mixRequest) chunk
                                         , "outAddr" .= (outAddr . mixRequest) chunk
                                         , "escrowAddr" .= escrowAddr chunk
                                         , "warrant" .= warrant ]

data MixcoinState = MixcoinState
                    { config   :: Config
                    , pending  :: TVar (M.Map PubKey Chunk)
                    , mixing   :: TVar [PubKey]
                    , retained :: TVar [PubKey]
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

newState :: Config -> IO MixcoinState
newState cfg = do
  pend <- newTVarIO M.empty
  mix <- newTVarIO []
  retained <- atomically $ newTVar []
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
  escrow <- generatePubKey
  pend <- asks pending
  let chunk = Chunk r escrow
  liftIO $ atomically $ modifyTVar' pend (M.insert escrow chunk)
  return chunk

generatePubKey :: Mixcoin PubKey
generatePubKey = return "fake pubkey"

sign :: Chunk -> Mixcoin SignedMixRequest
sign c = do
  return $ SignedMixRequest c "fake warrant"
