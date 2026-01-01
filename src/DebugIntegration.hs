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

import Data.IORef (writeIORef)

import Debug (debugLog, debugError, debugInfo, debugWarn, debugTrace)
import CommandLineDebug (CommandLineDebugConfig, pushCallStack, popCallStack, listBreakpoints, printBreakpoints, setBreakpoint, clearBreakpoints, addWatchVariable, runWithCLIDebug, defaultCLIDebugConfig, getCallStack, listWatchVariables, cldEnabled, cldLogLevel, cldInteractive)

-- Debug integration for compiler phases
withDebugging :: CommandLineDebugConfig -> String -> IO a -> IO a
withDebugging config phase action = do
    pushCallStack config phase
    debugLog phase $ "Starting " ++ phase ++ " phase"
    result <- action
    debugLog phase $ "Completed " ++ phase ++ " phase"
    popCallStack config
    return result

-- Debug parse step with breakpoints
debugParseStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugParseStep config filename action = do
    let location = "parse:" ++ filename
    pushCallStack config location
    addWatchVariable config "currentFile" filename
    debugInfo location $ "Starting parsing of: " ++ filename
    result <- runWithCLIDebug config location action
    popCallStack config
    return result

-- Debug compile step with breakpoints
debugCompileStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugCompileStep config filename action = do
    let location = "compile:" ++ filename
    pushCallStack config location
    addWatchVariable config "currentFile" filename
    debugInfo location $ "Starting compilation of: " ++ filename
    result <- runWithCLIDebug config location action
    popCallStack config
    return result

-- Debug ownership step with breakpoints
debugOwnershipStep :: CommandLineDebugConfig -> String -> IO a -> IO a
debugOwnershipStep config filename action = do
    let location = "ownership:" ++ filename
    pushCallStack config location
    addWatchVariable config "currentFile" filename
    debugInfo location $ "Starting ownership analysis of: " ++ filename
    result <- runWithCLIDebug config location action
    popCallStack config
    return result

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
showCurrentBreakpoints = printBreakpoints

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

-- Enhanced debugging functions for compiler

-- Debug type checking step
_debugTypeCheckStep :: CommandLineDebugConfig -> String -> IO a -> IO a
_debugTypeCheckStep config filename action = do
    let location = "typecheck:" ++ filename
    pushCallStack config location
    addWatchVariable config "currentFile" filename
    debugInfo location $ "Starting type checking of: " ++ filename
    result <- runWithCLIDebug config location action
    popCallStack config
    return result

-- Debug code generation step
_debugCodeGenStep :: CommandLineDebugConfig -> String -> IO a -> IO a
_debugCodeGenStep config filename action = do
    let location = "codegen:" ++ filename
    pushCallStack config location
    addWatchVariable config "currentFile" filename
    debugInfo location $ "Starting code generation for: " ++ filename
    result <- runWithCLIDebug config location action
    popCallStack config
    return result

-- Debug error with context
_debugErrorWithContext :: CommandLineDebugConfig -> String -> String -> [(String, String)] -> IO ()
_debugErrorWithContext config location errorMsg context = do
    debugError location $ "Error: " ++ errorMsg
    mapM_ (\(key, value) -> addWatchVariable config key value) context
    runWithCLIDebug config ("error:" ++ location) $ return ()

-- Debug warning with context
_debugWarningWithContext :: CommandLineDebugConfig -> String -> String -> [(String, String)] -> IO ()
_debugWarningWithContext config location warningMsg context = do
    debugWarn location $ "Warning: " ++ warningMsg
    mapM_ (\(key, value) -> addWatchVariable config key value) context

-- Show full debug state
_showFullDebugState :: CommandLineDebugConfig -> IO ()
_showFullDebugState config = do
    putStrLn "\n=== DEBUG STATE ==="
    
    -- Show call stack
    callStack <- getCallStack config
    putStrLn "\nCall Stack:"
    if null callStack
        then putStrLn "  (empty)"
        else mapM_ (\frame -> putStrLn $ "  " ++ frame) callStack
    let _ = callStack -- Suppress unused variable warning
    
    -- Show watch variables
    putStrLn "\nWatch Variables:"
    listWatchVariables config
    
    -- Show breakpoints
    putStrLn "\nBreakpoints:"
    listBreakpoints config
    
    putStrLn "==================\n"

-- Debug function entry
_debugFunctionEntry :: CommandLineDebugConfig -> String -> String -> [(String, String)] -> IO ()
_debugFunctionEntry config funcName args context = do
    let location = "func:" ++ funcName
    pushCallStack config location
    addWatchVariable config "function" funcName
    addWatchVariable config "arguments" args
    mapM_ (\(key, value) -> addWatchVariable config key value) context
    debugInfo location $ "Entering function: " ++ funcName ++ " with args: " ++ args

-- Debug function exit
_debugFunctionExit :: CommandLineDebugConfig -> String -> String -> IO ()
_debugFunctionExit config funcName result = do
    let location = "func:" ++ funcName
    addWatchVariable config "result" result
    debugInfo location $ "Exiting function: " ++ funcName ++ " with result: " ++ result
    popCallStack config