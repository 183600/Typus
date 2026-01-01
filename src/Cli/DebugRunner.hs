{-# LANGUAGE OverloadedStrings #-}

module Cli.DebugRunner
    ( runDebugMode
    , processDebugArgs
    , DebugCommand(..)
    ) where

import CommandLineDebug (CommandLineDebugConfig, defaultCLIDebugConfig, setBreakpoint, listBreakpoints, printBreakpoints, clearBreakpoints)
import EnhancedDebug (EnhancedDebugConfig, defaultEnhancedDebugConfig, createBreakpoint, LogLevel(Debug, Info, Warning, Error), setLogLevel, getDebugStats)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map
import Text.Printf (printf)

-- Debug command types
data DebugCommand
    = SetBreakpoint String
    | SetConditionalBreakpoint String (String -> Bool)
    | ListBreakpoints
    | ClearBreakpoints
    | SetLogLevel LogLevel
    | ShowDebugStats
    | RunWithDebug String
    | EnableTracing
    | DisableTracing
    | Help
    | ExitDebug

instance Show DebugCommand where
    show (SetBreakpoint loc) = "SetBreakpoint " ++ loc
    show (SetConditionalBreakpoint loc _) = "SetConditionalBreakpoint " ++ loc ++ " <function>"
    show ListBreakpoints = "ListBreakpoints"
    show ClearBreakpoints = "ClearBreakpoints"
    show (SetLogLevel level) = "SetLogLevel " ++ show level
    show ShowDebugStats = "ShowDebugStats"
    show (RunWithDebug file) = "RunWithDebug " ++ file
    show EnableTracing = "EnableTracing"
    show DisableTracing = "DisableTracing"
    show Help = "Help"
    show ExitDebug = "ExitDebug"

instance Eq DebugCommand where
    (SetBreakpoint a) == (SetBreakpoint b) = a == b
    (SetConditionalBreakpoint a _) == (SetConditionalBreakpoint b _) = a == b
    ListBreakpoints == ListBreakpoints = True
    ClearBreakpoints == ClearBreakpoints = True
    (SetLogLevel a) == (SetLogLevel b) = a == b
    ShowDebugStats == ShowDebugStats = True
    (RunWithDebug a) == (RunWithDebug b) = a == b
    EnableTracing == EnableTracing = True
    DisableTracing == DisableTracing = True
    Help == Help = True
    ExitDebug == ExitDebug = True
    _ == _ = False

-- Run debug mode
runDebugMode :: IO ()
runDebugMode = do
    putStrLn "=== Typus Debug Mode ==="
    putStrLn "Type 'help' for available commands"
    
    -- Initialize debug configurations
    cliConfig <- defaultCLIDebugConfig
    enhancedConfig <- defaultEnhancedDebugConfig
    
    debugLoop cliConfig enhancedConfig

-- Main debug loop
debugLoop :: CommandLineDebugConfig -> EnhancedDebugConfig -> IO ()
debugLoop cliConfig enhancedConfig = do
    putStr "debug> "
    hFlush stdout
    line <- getLine
    let args = words line
    
    if null args
        then debugLoop cliConfig enhancedConfig
        else do
            command <- parseDebugCommand args
            case command of
                Help -> do
                    showDebugHelp
                    debugLoop cliConfig enhancedConfig
                SetBreakpoint location -> do
                    setBreakpoint cliConfig location
                    createBreakpoint enhancedConfig location
                    debugLoop cliConfig enhancedConfig
                ListBreakpoints -> do
                    listBreakpoints cliConfig
                    debugLoop cliConfig enhancedConfig
                ClearBreakpoints -> do
                    clearBreakpoints cliConfig
                    debugLoop cliConfig enhancedConfig
                SetLogLevel level -> do
                    setLogLevel enhancedConfig level
                    debugLoop cliConfig enhancedConfig
                ShowDebugStats -> do
                    showStats enhancedConfig
                    debugLoop cliConfig enhancedConfig
                EnableTracing -> do
                    setLogLevel enhancedConfig Debug
                    putStrLn "Tracing enabled"
                    debugLoop cliConfig enhancedConfig
                DisableTracing -> do
                    setLogLevel enhancedConfig Info
                    putStrLn "Tracing disabled"
                    debugLoop cliConfig enhancedConfig
                RunWithDebug filename -> do
                    runFileWithDebug cliConfig enhancedConfig filename
                    debugLoop cliConfig enhancedConfig
                ExitDebug -> return ()
                _ -> do
                    putStrLn "Unknown command. Type 'help' for available commands."
                    debugLoop cliConfig enhancedConfig

-- Parse debug command
parseDebugCommand :: [String] -> IO DebugCommand
parseDebugCommand args = do
    case args of
        ["help", "h"] -> return Help
        ["help"] -> return Help
        ["h"] -> return Help
        ["breakpoint", "set", location] -> return $ SetBreakpoint location
        ["bp", "set", location] -> return $ SetBreakpoint location
        ["breakpoint", "list"] -> return ListBreakpoints
        ["bp", "list"] -> return ListBreakpoints
        ["breakpoint", "clear"] -> return ClearBreakpoints
        ["bp", "clear"] -> return ClearBreakpoints
        ["log", "level", "debug"] -> return $ SetLogLevel Debug
        ["log", "level", "info"] -> return $ SetLogLevel Info
        ["log", "level", "warning"] -> return $ SetLogLevel Warning
        ["log", "level", "error"] -> return $ SetLogLevel Error
        ["log", "debug"] -> return EnableTracing
        ["log", "info"] -> return DisableTracing
        ["stats"] -> return ShowDebugStats
        ["run", filename] -> return $ RunWithDebug filename
        ["trace", "on"] -> return EnableTracing
        ["trace", "off"] -> return DisableTracing
        ["exit"] -> return ExitDebug
        ["quit"] -> return ExitDebug
        ["q"] -> return ExitDebug
        _ -> return Help

-- Show debug help
showDebugHelp :: IO ()
showDebugHelp = do
    putStrLn "\n=== Debug Commands ==="
    putStrLn "Breakpoint commands:"
    putStrLn "  breakpoint set <location>    - Set breakpoint at location"
    putStrLn "  bp set <location>            - Set breakpoint at location (short)"
    putStrLn "  breakpoint list              - List all breakpoints"
    putStrLn "  bp list                      - List all breakpoints (short)"
    putStrLn "  breakpoint clear             - Clear all breakpoints"
    putStrLn "  bp clear                     - Clear all breakpoints (short)"
    putStrLn ""
    putStrLn "Logging commands:"
    putStrLn "  log level <level>            - Set log level (debug/info/warning/error)"
    putStrLn "  log debug                    - Enable debug tracing"
    putStrLn "  log info                     - Set info level (disable debug tracing)"
    putStrLn "  trace on                     - Enable tracing"
    putStrLn "  trace off                    - Disable tracing"
    putStrLn ""
    putStrLn "Other commands:"
    putStrLn "  stats                        - Show debug statistics"
    putStrLn "  run <filename>               - Run file with debugging"
    putStrLn "  help                         - Show this help"
    putStrLn "  h                            - Show this help (short)"
    putStrLn "  exit, quit, q                - Exit debug mode"
    putStrLn ""
    putStrLn "Example usage:"
    putStrLn "  breakpoint set Parser.parseTypus"
    putStrLn "  log level debug"
    putStrLn "  run example.typus"

-- Show debug statistics
showStats :: EnhancedDebugConfig -> IO ()
showStats config = do
    putStrLn "\n=== Debug Statistics ==="
    (execCounts, timings, logCounts) <- getDebugStats config
    
    putStrLn "\nExecution Counts:"
    if null execCounts
        then putStrLn "  (no executions tracked)"
        else mapM_ (\(loc, count) -> 
            putStrLn $ "  " ++ loc ++ ": " ++ show count) (Map.toList execCounts)
    
    putStrLn "\nTimings:"
    if null timings
        then putStrLn "  (no timings recorded)"
        else mapM_ (\(loc, time) -> 
            putStrLn $ "  " ++ loc ++ ": " ++ printf "%.3f" time ++ "s") (Map.toList timings)
    
    putStrLn "\nLog Counts:"
    if null logCounts
        then putStrLn "  (no logs recorded)"
        else mapM_ (\(level, count) -> 
            putStrLn $ "  " ++ show level ++ ": " ++ show count) (Map.toList logCounts)

-- Run file with debugging
runFileWithDebug :: CommandLineDebugConfig -> EnhancedDebugConfig -> String -> IO ()
runFileWithDebug _ enhancedConfig filename = do
    exists <- doesFileExist filename
    if not exists
        then do
            putStrLn $ "Error: File not found: " ++ filename
            return ()
        else do
            putStrLn $ "Running file with debugging: " ++ filename
            
            -- Set breakpoints at common locations
            createBreakpoint enhancedConfig "Parser.parseTypus"
            createBreakpoint enhancedConfig "Compiler.compile"
            createBreakpoint enhancedConfig "Ownership.analyze"
            
            -- Enable debug logging
            setLogLevel enhancedConfig Debug
            
            -- Here you would typically call the actual compilation function
            -- with debugging enabled. For now, we'll simulate it.
            putStrLn "Debug mode enabled. Compilation would run here."
            putStrLn "Breakpoints set at key locations."
            putStrLn "Use 'breakpoint list' to see all breakpoints."

-- Process debug arguments from command line
processDebugArgs :: [String] -> IO ()
processDebugArgs args = do
    case args of
        [] -> runDebugMode
        ["help"] -> showDebugHelp
        ["breakpoint", "set", location] -> do
            config <- defaultCLIDebugConfig
            setBreakpoint config location
            putStrLn $ "Breakpoint set at: " ++ location
        ["breakpoint", "list"] -> do
            config <- defaultCLIDebugConfig
            printBreakpoints config
        ["log", "level", levelStr] -> do
            config <- defaultEnhancedDebugConfig
            case parseLogLevel levelStr of
                Just level -> do
                    setLogLevel config level
                    putStrLn $ "Log level set to: " ++ show level
                Nothing -> putStrLn $ "Invalid log level: " ++ levelStr
        _ -> do
            putStrLn "Invalid debug arguments. Use 'debug help' for usage."

-- Parse log level from string
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel str = case str of
    "debug" -> Just Debug
    "info" -> Just Info
    "warning" -> Just Warning
    "error" -> Just Error
    _ -> Nothing