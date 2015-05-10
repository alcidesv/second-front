{-# LANGUAGE OverloadedStrings #-}
module Main where 


import Control.Lens (set)

import SecondFront
import SecondFront.Workers
import SecondFront.Config
import qualified SecondFront.Logging as L(enableConsoleLogging)

import SecondTransfer (enableConsoleLogging)


main = do 
    -- Enable verbose logging in the second-transfer library, which is 
    -- handling the low-level details of http/2
    L.enableConsoleLogging
    -- Enable verbose logging also in this server, with things like requested
    -- files and the like
    enableConsoleLogging
    -- And then start serving resources. The server needs three things: configuration
    -- for the outward-facing network port (outsideConfig), data-fetching mechanisms (insideConfig)
    -- and the other settings related to how generally how data is exchanged 
    -- between the two sides (translationConfg)
    proxyServe 
        outsideConfig
        insideConfig 
        defaultTranslationConfig

-- The parameters fed to the server are just modified variations of the default parameters.

insideConfig :: InsideConfig
insideConfig = 
    -- The code below says "take the defaultInsideConfig, and set a new list of virtualHosts there,
    -- don't change anything else"
    set  
        virtualHosts_IC     -- What to set
        [
            VirtualHost
                "www.httptwo.com"
                (fileServer
                    (set
                        basePath_FSC
                        "website/_site"
                        defaultFileServerConfig
                    )
                )
        ]                   -- To what
        defaultInsideConfig -- Where: in the default config


outsideConfig :: OutsideConfig
outsideConfig = let
    -- The code below says "take the defaultOutsideConfig, and change the place where the 
    -- credentials are going to be found...
    edit = set 
        serverCredentials_OC
        (ServerCredentials "_priv/site_cert.pem" "_priv/site_privkey.pem")
        defaultOutsideConfig
    -- Now let's change the bind interface so that this server listens on all interfaces, 
    -- by default it only listens in 127.0.0.1
    edit' = set 
        serverEndpoint_OC
        (LocalInterface_OE ("0.0.0.0",4043))
        edit
  in 
    edit'

