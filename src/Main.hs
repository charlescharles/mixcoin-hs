{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error.Class  (MonadError, catchError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.Map                   as M
--import           Network.Haskoin

main :: IO ()
main = undefined

type PubKey = String

data Config = Config
              { chunkSize :: Int
              , fee       :: Float
              , minConfs  :: Int
              } deriving (Eq, Show)

data ChunkRequest = ChunkRequest
                    { sendBy   :: Int
                    , returnBy :: Int
                    , nonce    :: Int
                    , outAddr  :: PubKey
                    } deriving (Eq, Show)

instance FromJSON ChunkRequest where
  parseJSON (Object v) = ChunkRequest <$>
  			 v .: "sendBy" <*>
                         v .: "returnBy" <*>
                         v .: "nonce" <*>
                         v .: "outAddr"
  parseJSON _ = mzero

data Chunk = Chunk
             { request    :: !ChunkRequest
             , escrowAddr :: !PubKey
             } deriving (Eq, Show)

data SignedChunkRequest = SignedChunkRequest
                          { chunk   :: !Chunk
                          , warrant :: !String
                          } deriving (Eq, Show)

instance ToJSON SignedChunkRequest where
  toJSON SignedChunkRequest{..} = object [ "sendBy" .= (sendBy . request) chunk
                                         , "returnBy" .= (returnBy . request) chunk
                                         , "nonce" .= (nonce . request) chunk
                                         , "outAddr" .= (outAddr . request) chunk
                                         , "escrowAddr" .= escrowAddr chunk
                                         , "warrant" .= warrant ]

data MixcoinState = MixcoinState
                    { config   :: Config
                    , pending  :: TVar (M.Map PubKey Chunk)
                    , mixing   :: TVar [PubKey]
                    , retained :: TVar [PubKey]
                    }

newtype MixcoinError = MixcoinError String

type Log = String

newtype Mixcoin a = Mixcoin
                    { runM :: EitherT MixcoinError (WriterT [Log] (ReaderT MixcoinState IO)) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader MixcoinState,
                              MonadIO, MonadWriter [Log],
                              MonadError MixcoinError, Alternative)

runMixcoin :: MixcoinState -> Mixcoin a -> IO (Either MixcoinError a, [Log])
runMixcoin s = flip runReaderT s . (runWriterT . runEitherT . runM)

newState :: Config -> IO MixcoinState
newState cfg = do
  pend <- newTVarIO M.empty
  mix <- newTVarIO []
  retained <- atomically $ newTVar []
  return $ MixcoinState cfg pend mix retained

handleMixRequest :: ChunkRequest -> Mixcoin SignedChunkRequest
handleMixRequest r = do
  validateMixRequest r
  chunk <- addToPending r
  sign chunk

ensure :: Bool -> MixcoinError -> Mixcoin ()
ensure b e = if b then return () else throwError e

validateMixRequest :: ChunkRequest -> Mixcoin ()
validateMixRequest ChunkRequest{..} = do
  cfg <- asks config

  ensure (sendBy > 1000) $ MixcoinError "invalid sendby index"
  ensure (returnBy - sendBy > minConfs cfg) $ MixcoinError "mixing period too short"

addToPending :: ChunkRequest -> Mixcoin Chunk
addToPending r = do
  escrow <- generatePubKey
  pend <- asks pending
  let chunk = Chunk r escrow
  liftIO $ atomically $ modifyTVar' pend (M.insert escrow chunk)
  return chunk

generatePubKey :: Mixcoin PubKey
generatePubKey = return "fake pubkey"

sign :: Chunk -> Mixcoin SignedChunkRequest
sign c = do
  return SignedChunkRequest c "fake warrant"
