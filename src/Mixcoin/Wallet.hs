-- a super simple wallet library

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mixcoin.Wallet

( Wallet
, WalletHandler
, runWallet
, signAll
, newAddress
, newWallet )

where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State.Lazy hiding (gets, modify)
import qualified Data.Map                       as M
import           Network.Haskoin.Crypto
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction




data Wallet = Wallet
              { keys :: !(M.Map Address PrvKey)
              } deriving (Eq, Show, Read)

insertKeyPair :: Address -> PrvKey -> Wallet -> Wallet
insertKeyPair a p w = Wallet $ M.insert a p (keys w)

newtype WalletHandler a = WalletHandler
                          { runW :: EitherT String (StateT Wallet IO) a }
                          deriving (Functor, Applicative, Monad,
                          	MonadIO, MonadState Wallet,
                                MonadError String)

newWallet :: Wallet
newWallet = Wallet M.empty

runWallet :: WalletHandler a -> Wallet -> IO (Either String a)
runWallet m w = evalStateT ((runEitherT . runW) m) w

newAddress :: WalletHandler Address
newAddress = do
  priv <- liftIO $ withSource devRandom genPrvKey
  let addr = (pubKeyAddr . derivePubKey) priv
  modify $ insertKeyPair addr priv
  return addr

signAll :: Tx -> WalletHandler Tx
signAll tx@(Tx _ txIns _ _) = do
  sigIns <- WalletHandler $ hoistEither $ mapM constructSigInput txIns
  pks <- fetchPrivKeys txIns
  (signed, complete) <- WalletHandler $ hoistEither $ detSignTx tx sigIns pks
  unless complete (throwError "incomplete signature")
  return signed

constructSigInput :: TxIn -> Either String SigInput
constructSigInput (TxIn prevOut script _) = SigInput <$>
                                            decodeOutputBS script <*>
                                            pure prevOut <*>
                                            pure (SigAll False) <*>
                                            pure Nothing

fetchPrivKeys :: [TxIn] -> WalletHandler [PrvKey]
fetchPrivKeys txIns = do
  let txInToAddr = addrForOutput <=< decodeOutputBS . scriptInput
  scriptOuts <- WalletHandler $ hoistEither $ mapM txInToAddr txIns
  ks <- gets keys
  let pksM = mapM (flip M.lookup ks) scriptOuts
  maybe (throwError "addr not found in wallet") return pksM

addrForOutput :: ScriptOutput -> Either String Address
addrForOutput (PayPKHash a) = Right a
addrForOutput _ = Left "output is not P2PK"
