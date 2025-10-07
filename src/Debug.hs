{-# LANGUAGE OverloadedStrings #-}

module Debug
    ( debugLog
    , debugBreakpoint
    , debugTrace
    , debugError
    , debugWarn
    , debugInfo
    , DebugConfig(..)
    , defaultDebugConfig
    , withDebugConfig
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (hFlush, stdout, hIsTerminalDevice, stdin)
import System.Environment (lookupEnv)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Control.Monad (when)

-- Debug configuration
data DebugConfig = DebugConfig
    { dcEnabled :: Bool
    , dcLogLevel :: Int  -- 0 = off, 1 = error, 2 = warn, 3 = info, 4 = debug
    , dcShowTime :: Bool
    , dcShowLocation :: Bool
    } deriving (Show, Eq)

-- Default debug configuration
defaultDebugConfig :: DebugConfig
defaultDebugConfig = DebugConfig
    { dcEnabled = True
    , dcLogLevel = 3
    , dcShowTime = True
    , dcShowLocation = True
    }

-- Run action with debug configuration
withDebugConfig :: DebugConfig -> IO a -> IO a
withDebugConfig _ action = do
    -- Store config in environment or just pass it along
    action

-- Debug logging with location information
debugLog :: MonadIO m => String -> String -> m ()
debugLog location message =
    debugLogWith defaultDebugConfig location message

debugLogWith :: MonadIO m => DebugConfig -> String -> String -> m ()
debugLogWith config location message
    | not (dcEnabled config) = return ()
    | dcLogLevel config < 3 = return ()
    | otherwise = liftIO $ do
        timestamp <- if dcShowTime config
            then do
                now <- getCurrentTime
                return $ formatTime defaultTimeLocale "[%H:%M:%S] " now
            else return ""

        let locationStr = if dcShowLocation config
                then "[" ++ location ++ "] "
                else ""

        putStrLn $ timestamp ++ locationStr ++ message
        hFlush stdout

-- Debug breakpoint - pauses execution and shows debug info
debugBreakpoint :: MonadIO m => String -> String -> m ()
debugBreakpoint location message =
    debugBreakpointWith defaultDebugConfig location message

debugBreakpointWith :: MonadIO m => DebugConfig -> String -> String -> m ()
debugBreakpointWith config location message
    | not (dcEnabled config) = return ()
    | otherwise = liftIO $ do
        debugLogWith config location $ "=== BREAKPOINT ==="
        debugLogWith config location message
        -- Only wait for input if explicitly enabled via env var
        -- to keep CI and test runs non-interactive by default.
        mWait <- lookupEnv "TYPUS_DEBUG_WAIT"
        case mWait of
          Just v | v == "1" || v == "true" || v == "TRUE" -> do
            isInteractive <- hIsTerminalDevice stdin
            when isInteractive $ do
              putStrLn $ location ++ ": Press Enter to continue..."
              _ <- getLine
              return ()
          _ -> return ()
        return ()

-- Debug trace - shows the flow of execution
debugTrace :: MonadIO m => String -> String -> m ()
debugTrace location message =
    debugTraceWith defaultDebugConfig location message

debugTraceWith :: MonadIO m => DebugConfig -> String -> String -> m ()
debugTraceWith config location message
    | not (dcEnabled config) = return ()
    | dcLogLevel config < 4 = return ()
    | otherwise = debugLogWith config location $ "TRACE: " ++ message

-- Utility functions for different log levels
debugError :: MonadIO m => String -> String -> m ()
debugError location message =
    debugLogWithLevel defaultDebugConfig 1 location message

debugWarn :: MonadIO m => String -> String -> m ()
debugWarn location message =
    debugLogWithLevel defaultDebugConfig 2 location message

debugInfo :: MonadIO m => String -> String -> m ()
debugInfo location message =
    debugLogWithLevel defaultDebugConfig 3 location message

debugLogWithLevel :: MonadIO m => DebugConfig -> Int -> String -> String -> m ()
debugLogWithLevel config level location message
    | not (dcEnabled config) = return ()
    | dcLogLevel config < level = return ()
    | otherwise = debugLogWith config location message