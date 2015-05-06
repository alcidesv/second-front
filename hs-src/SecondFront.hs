{-# LANGUAGE TemplateHaskell #-}

module SecondFront(
    proxyServe,

    -- * Top-level configuration
    module SecondFront.Config.Types
    ) where 


import SecondFront.Config.Types
import SecondFront.VirtualHosts.Route(proxyServe)

