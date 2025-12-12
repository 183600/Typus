{-# LANGUAGE OverloadedStrings #-}


module DebugIntegration
    ( withDebugging
    , debugParseStep
    , debugCompileStep
    , debugOwnershipStep
    , createDebugBreakpoints
    , setupCompilerDebugging
    , debugCompilerStart
    , debugCompilerEnd
    , debugErrorReport
    , debugWarningReport
    , debugPerformance
    , exampleDebugIntegration
    , showCurrentBreakpoints
    , addCustomBreakpoint
    , removeAllBreakpoints
    , enableInteractiveMode
    , disableInteractiveMode
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
debugCompilerStart :: CommandLineDebugConfig -> String -> IO ()
debugCompilerStart config filename = do
    debugInfo "compiler:start" $ "Starting compilation of: " ++ filename
    runWithCLIDebug config "compiler:start" $ return ()
    debugInfo "compiler:start" $ "Starting compilation of: " ++ filename
    runWithCLIDebug config "compiler:start" $ return ()

debugCompilerEnd :: CommandLineDebugConfig -> String -> IO ()
debugCompilerEnd config filename = do
    debugInfo "compiler:end" $ "Finished compilation of: " ++ filename
    runWithCLIDebug config "compiler:end" $ return ()

debugErrorReport :: CommandLineDebugConfig -> String -> String -> IO ()
debugErrorReport config location errorMsg = do
    debugError location $ "Error: " ++ errorMsg
    runWithCLIDebug config ("error:" ++ location) $ return ()

debugWarningReport :: CommandLineDebugConfig -> String -> String -> IO ()
debugWarningReport _config location warning = do
    debugWarn location $ "Warning: " ++ warning

debugPerformance :: CommandLineDebugConfig -> String -> String -> IO ()
debugPerformance _config metric value = do
    debugTrace "performance" $ metric ++ ": " ++ value

-- Integration example functions
exampleDebugIntegration :: IO ()
exampleDebugIntegration = do
    putStrLn "=== Debug Integration Example ==="
    config <- setupCompilerDebugging

    -- Simulate compiler phases
    let exampleFile :: FilePath
        exampleFile = "fixtures/reference/example.typus"
    debugCompilerStart config exampleFile

    -- Parse phase
    debugParseStep config exampleFile $ do
        putStrLn $ "Parsing " ++ exampleFile ++ "..."
        -- Simulate parsing
        return ()

    -- Compile phase
    debugCompileStep config exampleFile $ do
        putStrLn $ "Compiling " ++ exampleFile ++ "..."
        -- Simulate compilation
        return ()

    -- Ownership phase
    debugOwnershipStep config exampleFile $ do
        putStrLn $ "Ownership analysis for " ++ exampleFile ++ "..."
        -- Simulate ownership analysis
        return ()

    debugCompilerEnd config exampleFile
    putStrLn "=== Debug Integration Example Complete ==="

-- Command line debugger utility functions
showCurrentBreakpoints :: CommandLineDebugConfig -> IO ()
showCurrentBreakpoints = listBreakpoints

addCustomBreakpoint :: CommandLineDebugConfig -> String -> IO ()
addCustomBreakpoint = setBreakpoint

removeAllBreakpoints :: CommandLineDebugConfig -> IO ()
removeAllBreakpoints = clearBreakpoints

enableInteractiveMode :: CommandLineDebugConfig -> IO ()
enableInteractiveMode config = do
    writeIORef (cldInteractive config) True
    debugInfo "debug:mode" "Interactive mode enabled"

disableInteractiveMode :: CommandLineDebugConfig -> IO ()
disableInteractiveMode config = do
    writeIORef (cldInteractive config) False
    debugInfo "debug:mode" "Interactive mode disabled"