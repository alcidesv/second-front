{-# LANGUAGE OverloadedStrings #-}
module SecondFront.Workers.Data where 


import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)

import SecondTransfer(Headers)



bad404ResponseData :: B.ByteString 
bad404ResponseData = "404: SecondFront: Didn't find that"

bad501ResponseData :: B.ByteString
bad501ResponseData = "505: You are seeing this instead of some non-implemented, more specific slap"


-- TODO: It could be a really bad idea to give the server indication always....
-- We may need a way to control these ancilliary headers somewhere...
serverPair :: (B.ByteString, B.ByteString)
serverPair = ("server", "second-front--0.1")

standardErrorHeaders :: Headers 
standardErrorHeaders = [
    ("content-type", "text/plain")
    ,serverPair
    ]

bad404ResponseHeaders ::  Headers
bad404ResponseHeaders =   [
     (":status", "404")
    ,serverPair
    ,("content-length",  (  pack . show $ B.length bad404ResponseData)) 
    ]++ standardErrorHeaders

bad501ResponseHeaders :: Headers
bad501ResponseHeaders = [
     (":status", "501")
    ,serverPair
    ,("content-length", (  pack . show $ B.length bad501ResponseData )) 
    ]++ standardErrorHeaders

good200ResponseHeaders :: B.ByteString -> Int -> Headers
good200ResponseHeaders mime_type content_length = [
     (":status", "200")
    ,("content-length", (  pack . show $ content_length ) ) 
    ,serverPair
    ,("content-type", mime_type)
    ]