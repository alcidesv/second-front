module SecondFront.Logging(
    enableConsoleLogging
    ) where 

import           System.IO                 (stderr)

-- Logging utilities
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter, LogHandler)
import           System.Log.Handler.Simple
-- import           System.Log.Handler.Syslog (Facility (..), Option (..), openlog)
import           System.Log.Logger


-- | Activates logging to terminal
enableConsoleLogging :: IO ()
enableConsoleLogging = configureLoggingToConsole


configureLoggingToConsole :: IO ()
configureLoggingToConsole = do 
    s <- streamHandler stderr DEBUG  >>= 
        \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    setLoggerLevels s


-- configureLoggingToSyslog :: IO ()
-- configureLoggingToSyslog = do 
--     s <- openlog "RehMimic" [PID] DAEMON INFO >>= 
--         \lh -> return $ setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
--     setLoggerLevels s


setLoggerLevels :: (LogHandler s) => s -> IO () 
setLoggerLevels s = do
    updateGlobalLogger rootLoggerName removeHandler
    updateGlobalLogger "2ndF" (
        setHandlers [s] .  
        setLevel INFO  
        )