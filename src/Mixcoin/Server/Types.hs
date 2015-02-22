{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mixcoin.Server.Types

( Address
, MixcoinConfig (..)
, ServerConfig (..)
, MixRequest (..)
, LabeledMixRequest (..)
, SignedMixRequest (..)
, MixcoinState (..)
, MixcoinError (..)
, Mixcoin
, MixcoinPrivKey
, MixcoinPubKey
, UTXO (..)
, BlockHeight
, BTC
, runMixcoin
, execMixcoin
, newState
, throwError

-- logging
, Facility (..)
, Priority (..)
, openlog
, addHandler
, infoM, debugM, warningM, errorM
, rootLoggerName
, setHandlers
, setLevel
, updateGlobalLogger
, getBigWordInteger
)

where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error.Class  (MonadError, throwError)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Data.Map                   as M
import           Mixcoin.Common.Types

data ServerConfig = ServerConfig
			{ mixcoinCfg :: MixcoinConfig
                        , port       :: Int
                        } deriving (Eq, Show)

data MixcoinConfig = MixcoinConfig
              { chunkSize      :: BTC
              , minerFee       :: BTC
              , feeProbability :: Float
              , minConfs       :: Int
              , client         :: Client
              , privKey        :: MixcoinPrivKey
              , feeAccount     :: String -- ^ the account containing fee chunks
              }

data MixcoinState = MixcoinState
                    { config   :: MixcoinConfig
                    , pending  :: TVar (M.Map Address LabeledMixRequest)
                    , mixing   :: TVar [UTXO]
                    , retained :: TVar [UTXO]
                    }

newtype Mixcoin a = Mixcoin
                    { runM :: EitherT MixcoinError (ReaderT MixcoinState IO) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader MixcoinState,
                              MonadIO, MonadError MixcoinError)

runMixcoin :: MixcoinState -> Mixcoin a -> IO (Either MixcoinError a)
runMixcoin s = flip runReaderT s . (runEitherT . runM)

execMixcoin :: MixcoinState -> Mixcoin a -> IO ()
execMixcoin s m = runMixcoin s m >> return ()

newState :: MixcoinConfig -> IO MixcoinState
newState cfg = do
  pend <- newTVarIO M.empty
  mix <- newTVarIO []
  retained <- newTVarIO []
  return $ MixcoinState cfg pend mix retained
