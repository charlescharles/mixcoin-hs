{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Control.Applicative
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error.Class  (MonadError, catchError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Either
import           Control.Monad.Writer
import           Data.Aeson                 (FromJSON, ToJSON, Value (..),
                                             object, parseJSON, toJSON, (.:),
                                             (.=))
import qualified Data.Map                   as M
import qualified Data.Text.Lazy             as T
import           Mixcoin.Wallet
import           Network.Haskoin.Crypto     hiding (PubKey)
import           Network.HTTP.Types         (badRequest400)
import           Web.Scotty

main :: IO ()
main = do
  mixState <- newState testConfig
  forkIO $ watchForTxs mixState
  scotty 9000 (server mixState)

server :: MixcoinState -> ScottyM ()
server mstate = do
  post "/" (mixRequest mstate)

mixRequest :: MixcoinState -> ActionM ()
mixRequest mstate = do
  c <- jsonData
  (res, log) <- liftIO $ runMixcoin mstate (handleMixRequest c)
  case res of
   Left err -> do
     status badRequest400
     text $ T.pack (show err)
   Right signed -> json signed

watchForTxs :: MixcoinState -> IO ()
watchForTxs mstate = forever $ do
  forkIO $ runMixcoin mstate receiveTxs >> return ()
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
  liftIO $ forkIO $ waitSend delay (outAddr $ chunkRequest c)
  return ()

generateDelay :: Chunk -> Mixcoin Int
generateDelay c = return (minutes 3)

waitSend :: Int -> PubKey -> IO ()
waitSend d pk = undefined



minutes :: Int -> Int
minutes = (* (truncate 6e7))

type Tx = String

type PubKey = String

data Config = Config
              { chunkSize :: Int
              , fee       :: Float
              , minConfs  :: Int
              } deriving (Eq, Show)

testConfig :: Config
testConfig = Config 100 0.02 1

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
             { chunkRequest :: !ChunkRequest
             , escrowAddr   :: !PubKey
             } deriving (Eq, Show)

data SignedChunkRequest = SignedChunkRequest
                          { chunk   :: !Chunk
                          , warrant :: !PubKey
                          } deriving (Eq, Show)

instance ToJSON SignedChunkRequest where
  toJSON SignedChunkRequest{..} = object [ "sendBy" .= (sendBy . chunkRequest) chunk
                                         , "returnBy" .= (returnBy . chunkRequest) chunk
                                         , "nonce" .= (nonce . chunkRequest) chunk
                                         , "outAddr" .= (outAddr . chunkRequest) chunk
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

handleMixRequest :: ChunkRequest -> Mixcoin SignedChunkRequest
handleMixRequest r = do
  validateMixRequest r
  chunk <- addToPending r
  sign chunk

ensure :: Bool -> MixcoinError -> Mixcoin ()
ensure b = unless b . throwError

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
  return $ SignedChunkRequest c "fake warrant"

