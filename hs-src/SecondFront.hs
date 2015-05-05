{-# LANGUAGE TemplateHaskell #-}

module SecondFront(
    proxyServe,

    -- * Top-level configuration
    OutsideConfig,
    InsideConfig,
    TranslationConfig
    ) where 


import SecondFront.Config.Types
import SecondFront.VirtualHosts.Route(proxyServe)

