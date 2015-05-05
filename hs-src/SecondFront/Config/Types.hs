{-# LANGUAGE TemplateHaskell #-}

module SecondFront.Config.Types where 


import qualified Data.ByteString as B
import qualified Control.Lens    as L
import qualified SecondTransfer  as S


data OwnedEndpoint = LocalInterface_OE PairIPPort
                   | LocalSocket_OE FilePath

data TargetEndpoint = NetworkIPv4_TE PairIPPort
                   | LocalSocket_TE FilePath

type PairIPPort = (String,Int)


data OutsideConfig = OutsideConfig {
    _serverEndpoint:: OwnedEndpoint
    }

data InsideConfig = InsideConfig {
    _virtualHosts :: [VirtualHost]
    }

data VirtualHost = VirtualHost {
    _vhName           :: B.ByteString,
    _vhBackendService :: S.CoherentWorker
    }

-- Nothing here yet
data TranslationConfig = TranslationConfig {
    
    }

L.makeLenses ''InsideConfig 
L.makeLenses ''VirtualHost
L.makeLenses ''OutsideConfig