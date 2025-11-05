{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)

-- Example function demonstrating debugging capabilities
exampleFunction :: Int -> Int -> IO Int
exampleFunction x y = do
    -- Log function entry with location
    debugLog "exampleFunction" $ "Called with args: " ++ show x ++ ", " ++ show y

    -- Add a breakpoint at function start
    debugBreakpoint "exampleFunction" "Starting computation"

    let result = x + y

    -- Trace intermediate computation
    debugTrace "exampleFunction" $ "Intermediate result: " ++ show result

    -- Log function exit
    debugLog "exampleFunction" $ "Returning result: " ++ show result

    return result

-- Example with custom debug configuration
exampleWithCustomConfig :: IO ()
exampleWithCustomConfig = do
    let customConfig = DebugConfig
            { dcEnabled = True
            , dcLogLevel = 4  -- Debug level
            , dcShowTime = True
            , dcShowLocation = True
            }

    withDebugConfig customConfig $ do
        debugLog "customConfig" "This uses custom debug configuration"
        debugTrace "customConfig" "This trace message will be shown"
        debugBreakpoint "customConfig" "Breakpoint with custom config"

main :: IO ()
main = do
    putStrLn "=== Debug Module Example ==="

    -- Basic debug logging
    debugLog "main" "Starting debug example"

    -- Run example function
    result <- exampleFunction 5 10
    putStrLn $ "Function result: " ++ show result

    -- Custom configuration example
    putStrLn "\n=== Custom Configuration Example ==="
    exampleWithCustomConfig

    -- Demonstrate different log levels
    putStrLn "\n=== Log Level Demonstration ==="

    let infoConfig = DebugConfig { dcEnabled = True, dcLogLevel = 3, dcShowTime = False, dcShowLocation = False }
    let debugConfig = DebugConfig { dcEnabled = True, dcLogLevel = 4, dcShowTime = False, dcShowLocation = False }

    putStrLn "Info level (3):"
    withDebugConfig infoConfig $ debugLog "main" "This info message should appear"

    putStrLn "Debug level (4):"
    withDebugConfig debugConfig $ debugTrace "main" "This debug trace should appear with debug config"

    putStrLn "\n=== Debug Example Complete ==="