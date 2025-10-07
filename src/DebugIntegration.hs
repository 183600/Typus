{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures -Wno-overlapping-patterns #-}


module DebugIntegration
    ( withDebugging
    , debugParseStep
    , debugCompileStep
    , debugOwnershipStep
    , createDebugBreakpoints
    , setupCompilerDebugging
    ) where

import Data.IORef

import Debug
import CommandLineDebug

-- Debug integration for compiler phases
withDebugging :: CommandLineDebugConfig -> String -> IO a -> IO a
withDebugging _config phase action = do
    debugLog phase $ "Starting " ++ phase ++ " phase"
    result <- action
    debugLog phase $ "Completed " ++ phase ++ " phase"
    return result

-- Debug parse step with breakpoints
debugParseStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugParseStep config filename action = do
    let location = "parse:" ++ filename
    debugInfo location $ "Starting parsing of: " ++ filename
    runWithCLIDebug config location action

-- Debug compile step with breakpoints
debugCompileStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugCompileStep config filename action = do
    let location = "compile:" ++ filename
    debugInfo location $ "Starting compilation of: " ++ filename
    runWithCLIDebug config location action

-- Debug ownership step with breakpoints
debugOwnershipStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugOwnershipStep config filename action = do
    let location = "ownership:" ++ filename
    debugInfo location $ "Starting ownership analysis of: " ++ filename
    runWithCLIDebug config location action

-- Create standard breakpoints for compiler debugging
createDebugBreakpoints :: CommandLineDebugConfig -> IO ()
createDebugBreakpoints config = do
    -- Set common breakpoints
    setBreakpoint config "parse:main"
    setBreakpoint config "compile:main"
    setBreakpoint config "ownership:main"
    setBreakpoint config "typecheck:main"
    setBreakpoint config "generate:main"

    -- Set phase entry breakpoints
    setBreakpoint config "parse:start"
    setBreakpoint config "compile:start"
    setBreakpoint config "ownership:start"
    setBreakpoint config "typecheck:start"
    setBreakpoint config "generate:start"

-- Setup compiler debugging with recommended configuration
setupCompilerDebugging :: IO CommandLineDebugConfig
setupCompilerDebugging = do
    config <- defaultCLIDebugConfig

    -- Configure for compiler debugging
    writeIORef (cldEnabled config) True
    writeIORef (cldLogLevel config) 4  -- Show debug level
    writeIORef (cldInteractive config) True

    -- Set common breakpoints
    createDebugBreakpoints config

    -- Log initialization
    debugInfo "debug:setup" "Compiler debugging initialized"
    debugInfo "debug:setup" "Available breakpoints: parse, compile, ownership, typecheck, generate"
    debugInfo "debug:setup" "Use debug-cli to manage breakpoints and debug settings"

    return config

-- Utility functions for compiler debugging
{-# WARNING debugCompilerStart "This function is part of the debug API and may be used in future" #-}
debugCompilerStart :: CommandLineDebugConfig -> String -> IO ()
debugCompilerStart config filename = do
    debugInfo "compiler:start" $ "Starting compilation of: " ++ filename
    runWithCLIDebug config "compiler:start" $ return ()
    debugInfo "compiler:start" $ "Starting compilation of: " ++ filename
    runWithCLIDebug config "compiler:start" $ return ()

{-# WARNING debugCompilerEnd "This function is part of the debug API and may be used in future" #-}
debugCompilerEnd config filename = do
    debugInfo "compiler:end" $ "Finished compilation of: " ++ filename
    runWithCLIDebug config "compiler:end" $ return ()
debugCompilerEnd config filename = do
    debugInfo "compiler:end" $ "Finished compilation of: " ++ filename
    runWithCLIDebug config "compiler:end" $ return ()

{-# WARNING debugErrorReport "This function is part of the debug API and may be used in future" #-}
debugErrorReport config location errorMsg = do
    debugError location $ "Error: " ++ errorMsg
    runWithCLIDebug config ("error:" ++ location) $ return ()
debugErrorReport config location errorMsg = do
    debugError location $ "Error: " ++ errorMsg
    runWithCLIDebug config ("error:" ++ location) $ return ()

{-# WARNING debugWarningReport "This function is part of the debug API and may be used in future" #-}
debugWarningReport _config location warning = do
    debugWarn location $ "Warning: " ++ warning
debugWarningReport _config location warning = do
    debugWarn location $ "Warning: " ++ warning

{-# WARNING debugPerformance "This function is part of the debug API and may be used in future" #-}
debugPerformance _config metric value = do
    debugTrace "performance" $ metric ++ ": " ++ value
debugPerformance _config metric value = do
    debugTrace "performance" $ metric ++ ": " ++ value

-- Integration example functions
{-# WARNING exampleDebugIntegration "This function is part of the debug API and may be used in future" #-}
exampleDebugIntegration :: IO ()
exampleDebugIntegration = do
    putStrLn "=== Debug Integration Example ==="
    config <- setupCompilerDebugging

    -- Simulate compiler phases
    debugCompilerStart config "example.typus"

    -- Parse phase
    debugParseStep config "example.typus" $ do
        putStrLn "Parsing example.typus..."
        -- Simulate parsing
        return ()

    -- Compile phase
    debugCompileStep config "example.typus" $ do
        putStrLn "Compiling example.typus..."
        -- Simulate compilation
        return ()

    -- Ownership phase
    debugOwnershipStep config "example.typus" $ do
        putStrLn "Ownership analysis for example.typus..."
        -- Simulate ownership analysis
        return ()

    debugCompilerEnd config "example.typus"
    putStrLn "=== Debug Integration Example Complete ==="

-- Command line debugger utility functions
{-# WARNING showCurrentBreakpoints "This function is part of the debug API and may be used in future" #-}
showCurrentBreakpoints = listBreakpoints

{-# WARNING addCustomBreakpoint "This function is part of the debug API and may be used in future" #-}
addCustomBreakpoint = setBreakpoint

{-# WARNING removeAllBreakpoints "This function is part of the debug API and may be used in future" #-}
removeAllBreakpoints = clearBreakpoints

{-# WARNING enableInteractiveMode "This function is part of the debug API and may be used in future" #-}
enableInteractiveMode config = do
    writeIORef (cldInteractive config) True
enableInteractiveMode config = do
    writeIORef (cldInteractive config) True
    debugInfo "debug:mode" "Interactive mode enabled"

{-# WARNING disableInteractiveMode "This function is part of the debug API and may be used in future" #-}
disableInteractiveMode config = do
    writeIORef (cldInteractive config) False
disableInteractiveMode config = do
    writeIORef (cldInteractive config) False
    debugInfo "debug:mode" "Interactive mode disabled"