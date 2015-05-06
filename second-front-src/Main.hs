
module Main where 

import SecondFront
import SecondFront.Workers
import SecondFront.Config
import qualified SecondFront.Logging as L(enableConsoleLogging)

import SecondTransfer (enableConsoleLogging)

main = do 
    L.enableConsoleLogging
    enableConsoleLogging
    proxyServe defaultOutsideConfig defaultInsideConfig defaultTranslationConfig
