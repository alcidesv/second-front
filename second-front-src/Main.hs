{-# LANGUAGE OverloadedStrings #-}
module Main where 


import Control.Lens (set)

import SecondFront
import SecondFront.Workers
import SecondFront.Config
import qualified SecondFront.Logging as L(enableConsoleLogging)

import SecondTransfer (enableConsoleLogging)


-- The "InsideConfig" contains, among other things, the definitions 
-- of virtual hosts.
insideConfig :: InsideConfig
insideConfig = 
    set  
        virtualHosts_IC     -- What to set
        [
            VirtualHost
                "www.httpdos.com:8080"
                (fileServer
                    (set
                        basePath_FSC
                        "sample-website/_site"
                        defaultFileServerConfig
                    )
                )
        ]                   -- To what
        defaultInsideConfig -- Where: in the default config

main = do 
    L.enableConsoleLogging
    enableConsoleLogging
    proxyServe 
    	defaultOutsideConfig 
    	insideConfig 
    	defaultTranslationConfig
