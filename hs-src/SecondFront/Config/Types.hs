{-# LANGUAGE TemplateHaskell #-}

module SecondFront.Config.Types(
    -- | Notice that most record members here have non-underscore lenses
    --   associated with them. 

    -- == Records
    OwnedEndpoint(..),
    TargetEndpoint(..),
    OutsideConfig(..),
    PairIPPort,
    InsideConfig(..),
    VirtualHost(..),
    TranslationConfig(..),
    ServerCredentials(..),

    -- == Lenses
    virtualHosts_IC,
    name_VH,
    backendService_VH,
    serverEndpoint_OC,
    serverCredentials_OC,
    certificatePath_SC,
    certificateKeyPath_SC,
    finishOnRequest_TC
    ) where 


import qualified Data.ByteString as B
import qualified Control.Lens    as L
import Control.Concurrent.MVar   (MVar)
import qualified SecondTransfer  as S


-- | Information needed for the outward-facing interface
data OutsideConfig = OutsideConfig {
    -- | Where to listen for connections
    _serverEndpoint_OC           :: OwnedEndpoint
    -- | Server credentials
    ,_serverCredentials_OC       :: ServerCredentials
    }

-- | Credentials needed for correct SSL encryption. This same certificate should
--   support, with altNames, all the domains in the virtual hosts served by this 
--   library.
data ServerCredentials = ServerCredentials {
    -- | Path to in-file certificate in PEM format.
    _certificatePath_SC          :: FilePath,
    -- | Path to the private key of the certificate.
    _certificateKeyPath_SC       :: FilePath
    }


-- | Information to configure a listening network socket
data OwnedEndpoint = LocalInterface_OE PairIPPort

-- | Information to configure a target endpoint: for example a 
-- proxied service
data TargetEndpoint = NetworkIPv4_TE PairIPPort
                   | LocalSocket_TE FilePath

-- | Information to identify a network interface. Two members at this
--   pair: an interface network name (an IPv4 IP at this moment, like 
--   "127.0.0.1"...) and a  port.
type PairIPPort = (String,Int)

data InsideConfig = InsideConfig {
    _virtualHosts_IC :: [VirtualHost]
    }

data VirtualHost = VirtualHost {
    _name_VH           :: B.ByteString,
    _backendService_VH :: S.CoherentWorker
    }

-- Nothing here yet
data TranslationConfig = TranslationConfig {
    _finishOnRequest_TC :: Maybe (MVar S.FinishRequest)
    }

L.makeLenses ''InsideConfig 
L.makeLenses ''VirtualHost
L.makeLenses ''OutsideConfig
L.makeLenses ''ServerCredentials
L.makeLenses ''OwnedEndpoint
L.makeLenses ''TranslationConfig