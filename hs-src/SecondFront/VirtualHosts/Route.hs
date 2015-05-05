{-# LANGUAGE OverloadedStrings #-}

module SecondFront.VirtualHosts.Route where


import Data.Foldable   (find)
import Control.Lens

import SecondTransfer
import SecondFront.Config.Types
import SecondFront.Workers.W404


proxyServe :: 
    OutsideConfig 
    -> InsideConfig 
    -> TranslationConfig
    -> IO ()
proxyServe = error "Not implemented"


route :: [VirtualHost] -> Request -> CoherentWorker 
route vh_list (headers, _) = let 
    -- Get the requested host, if any
    requested_host = case lookup ":authority" headers  of 
        Nothing -> "*"
        Just host -> host 
    use_coherent_worker = case find (\vh -> (vh ^. vhName) == requested_host ) vh_list  of 
        Nothing -> w404
        Just vh -> vh ^. vhBackendService
  in 
    use_coherent_worker