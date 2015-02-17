module Mixcoin.Common.Types

( MixRequest (..)
, LabeledMixRequest (..)
, SignedMixRequest (..)
, MixcoinError (..)
, Log
, MixcoinPrivKey
, MixcoinPubKey
, )


newtype MixcoinError = MixcoinError String deriving (Eq, Show)

type Log = String

type MixcoinPrivKey = PrivateKey

type MixcoinPubKey = PublicKey

data MixRequest = MixRequest
                    { sendBy   :: BlockHeight
                    , returnBy :: BlockHeight
                    , nonce    :: Int
                    , outAddr  :: Address
                    } deriving (Eq, Show)

instance ToJSON MixRequest where
  toJSON MixRequest{..} = object $ [ "sendBy" .= sendBy
                                   , "returnBy" .= returnBy
                                   , "nonce" .= nonce
                                   , "outAddr" .= outAddr ]


instance FromJSON MixRequest where
  parseJSON (Object v) = MixRequest <$>
         v .: "sendBy" <*>
                         v .: "returnBy" <*>
                         v .: "nonce" <*>
                         v .: "outAddr"
  parseJSON _ = mzero

data LabeledMixRequest = LabeledMixRequest
             { mixReq     :: !MixRequest
             , escrowAddr :: !Address
             } deriving (Eq, Show)

instance ToJSON LabeledMixRequest where
  toJSON LabeledMixRequest{..} = Array . V.fromList $
                                 [ object [ "sendBy" .=  sendBy mixReq ]
                                 , object [ "returnBy" .= returnBy mixReq ]
                                 , object [ "nonce" .= nonce mixReq ]
                                 , object [ "outAddr" .= outAddr mixReq ]
                                 , object [ "escrowAddr" .= escrowAddr ] ]

instance FromJSON LabeledMixRequest where
  parseJSON j = do
    [sendby', returnby', nonce', out', escrow'] <- parseJSON j
    mixreq <- MixRequest <$> sendby' .: "sendBy"
          <*> returnby' .: "returnBy"
                  <*> nonce' .: "nonce"
                  <*> out' .: "outAddr"
    LabeledMixRequest mixreq <$> escrow' .: "escrowAddr"

  parseJSON _ = mzero

data SignedMixRequest = SignedMixRequest
                          { labeledMixReq :: !LabeledMixRequest
                          , warrant       :: !String
                          } deriving (Eq, Show)

-- accessors for SignedMixRequest
sendBySigned, returnBySigned :: SignedMixRequest -> BlockHeight
sendBySigned = sendBy . mixReq . labeledMixReq
returnBySigned = returnBy . mixReq . labeledMixReq

nonceSigned :: SignedMixRequest -> Int
nonceSigned = nonce . mixReq . labeledMixReq

outAddrSigned :: SignedMixRequest -> Address
outAddrSigned = outAddr . mixReq . labeledMixReq

escrowAddrSigned :: SignedMixRequest -> Address
escrowAddrSigned = escrowAddr . labeledMixReq

-- key order: sendBy, returnBy, nonce, outAddr, escrowAddr, warrant
instance ToJSON SignedMixRequest where
  toJSON s = Array . V.fromList $ [ object [ "sendBy" .= sendBySigned s ]
                                , object [ "returnBy" .= returnBySigned s ]
                                , object [ "nonce" .= nonceSigned s ]
                                , object [ "outAddr" .= outAddrSigned s ]
                                , object [ "escrowAddr" .= escrowAddrSigned s ]
                                , object [ "warrant" .= warrant s ] ]

instance FromJSON SignedMixRequest where
  parseJSON j = do
    [sendby', returnby', nonce', out', escrow', warrant'] <- parseJSON j
    mixreq <- MixRequest <$> sendby' .: "sendBy"
          <*> returnby' .: "returnBy"
                  <*> nonce' .: "nonce"
                  <*> out' .: "outAddr"
    labeled <- LabeledMixRequest mixreq <$> escrow' .: "escrowAddr"
    SignedMixRequest labeled <$> warrant' .: "warrant"

  parseJSON _ = mzero
