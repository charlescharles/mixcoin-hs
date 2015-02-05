{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
                    { send    :: Int
                    , return  :: Int
                    , nonce   :: Int
                    , outAddr :: PubKey
                    } deriving (Eq, Show)

data Chunk = Chunk
             { request    :: ChunkRequest
             , escrowAddr :: PubKey
             } deriving (Eq, Show)

data MixcoinState = MixcoinState
                    { config   :: Config
                    , pending  :: TVar (M.Map PubKey Chunk)
                    , mixing   :: TVar [PubKey]
                    , retained :: TVar [PubKey]
                    }

type MixcoinError = String

type Log = String

newtype Mixcoin a = Mixcoin
                    { runM :: EitherT MixcoinError (WriterT [Log] (ReaderT MixcoinState IO)) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader MixcoinState,
                              MonadIO, MonadWriter [String], MonadPlus,
                              MonadError MixcoinError, Alternative)

runMixcoin :: MixcoinState -> Mixcoin a -> IO (Either MixcoinError a, [Log])
runMixcoin s = flip runReaderT s . (runWriterT . runEitherT . runM)

handleMixRequest :: ChunkRequest -> Mixcoin ()
handleMixRequest r = do
  liftIO $ putStrLn "hi"

