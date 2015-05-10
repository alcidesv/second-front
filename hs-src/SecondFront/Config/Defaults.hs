{-# LANGUAGE OverloadedStrings #-}
module SecondFront.Config.Defaults(
    defaultOutsideConfig,
    defaultInsideConfig,
    defaultTranslationConfig,
    defaultVirtualHosts,
    defaultServerCredentials
    ) where 

import SecondFront.Config.Types
import SecondFront.Workers.FileServer 

-- | Default outside config. Listens at 127.0.0.1:8080 and uses
--  the `defaultServerCredentials`
defaultOutsideConfig :: OutsideConfig
defaultOutsideConfig = OutsideConfig {
    _serverEndpoint_OC = LocalInterface_OE ("127.0.0.1", 4043),
    _serverCredentials_OC = defaultServerCredentials
  }


-- | Default inside config with the `defaultVirtualHosts` list
defaultInsideConfig :: InsideConfig 
defaultInsideConfig = InsideConfig {
    _virtualHosts_IC = defaultVirtualHosts  
  }

-- | Default virtual host list: contains "www.httpdos.com:8080" and a 
--   file-server able to serve the current directory.  Get your own.
defaultVirtualHosts :: [VirtualHost]
defaultVirtualHosts = [
    VirtualHost {
        _name_VH           = "www.httpdos.com:8080",
        _backendService_VH = fileServer defaultFileServerConfig
      }
  ]

-- | Default translation config. Pretty empty for now
defaultTranslationConfig :: TranslationConfig 
defaultTranslationConfig = TranslationConfig {
  _finishOnRequest_TC = Nothing  
  }

-- | Default server credentials. This assumes there is a couple of files
--   in .pem format with the certificate and the certificate key, in a relative
--   directory "_priv/cert.pem" and "_priv/privkey.pem". That's as insecure as
--   it gets, even if the default file server won't enter that path....
defaultServerCredentials :: ServerCredentials
defaultServerCredentials = 
  ServerCredentials {
        _certificatePath_SC    = "_priv/cert.pem",
        _certificateKeyPath_SC = "_priv/privkey.pem"
      }