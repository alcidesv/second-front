{-# LANGUAGE OverloadedStrings #-}

module SecondFront.VirtualHosts.Route where


import System.Log.Logger
import Control.Lens
import Control.Concurrent.MVar
-- import Control.Exception as E
-- import Control.Concurrent

import Data.Foldable   (find)
import Data.ByteString.Char8 (unpack)

import SecondTransfer
import SecondTransfer.Http2
import SecondFront.Config.Types
import SecondFront.Workers.W404

import System.Posix.Signals


proxyServe :: 
    OutsideConfig 
    -> InsideConfig 
    -> TranslationConfig
    -> IO ()
proxyServe outside_config inside_config translation_config = do

    let 
        virtual_hosts = inside_config ^. virtualHosts_IC
        coherent_worker = route virtual_hosts
        credentials = outside_config ^. serverCredentials_OC
        server_endpoint = outside_config ^. serverEndpoint_OC
        LocalInterface_OE pair_ip_port = server_endpoint
        maybe_finish_request_a = translation_config ^. finishOnRequest_TC

    finish_request <- case maybe_finish_request_a of 
        Nothing -> do 
            x <- newEmptyMVar 
            installHandler keyboardSignal (Catch (do 
                putMVar x FinishRequest
                )) Nothing
            return x 


        Just x  -> return x

    sessions_context <- makeSessionsContext defaultSessionsConfig
    let attendant = http2Attendant sessions_context coherent_worker

    tlsServeWithALPNAndFinishOnRequest
        (credentials ^. certificatePath_SC)
        (credentials ^. certificateKeyPath_SC)
        (pair_ip_port ^. _1 )
        -- TODO: We can add plain HTTPS below, someday
        [
            ("h2", attendant),
            ("h2-15", attendant),
            ("h2-14", attendant)
        ]
        (pair_ip_port ^. _2 )
        finish_request


route :: [VirtualHost] -> Request -> IO PrincipalStream 
route vh_list req@(headers, _) = let 
    -- Get the requested host, if any
    requested_host = case lookup ":authority" headers  of 
        Nothing -> "*"
        Just host -> host 
    use_coherent_worker = case find (\vh -> (vh ^. name_VH) == requested_host ) vh_list  of 
        Nothing -> w404
        Just vh -> vh ^. backendService_VH
  in do 
    infoM "2ndF" $ "Authority: " ++ (unpack requested_host)
    use_coherent_worker req