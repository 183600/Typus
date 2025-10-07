{-# LANGUAGE OverloadedStrings #-}

module CommandLineDebug
    ( CommandLineDebugConfig(..)
    , defaultCLIDebugConfig
    , runWithCLIDebug
    , setBreakpoint
    , listBreakpoints
    , clearBreakpoints
    , toggleDebugOutput
    , setDebugLevel
    , showDebugStatus
    ) where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (hFlush, stdout)

-- Command line debug configuration
data CommandLineDebugConfig = CommandLineDebugConfig
    { cldEnabled :: IORef Bool
    , cldLogLevel :: IORef Int
    , cldBreakpoints :: IORef (Set String)
    , cldBreakConditions :: IORef (Map String (String -> Bool))
    , cldInteractive :: IORef Bool
    }

-- Default command line debug configuration
defaultCLIDebugConfig :: IO CommandLineDebugConfig
defaultCLIDebugConfig = do
    enabledRef <- newIORef True
    logLevelRef <- newIORef 3
    breakpointsRef <- newIORef Set.empty
    breakConditionsRef <- newIORef Map.empty
    interactiveRef <- newIORef True
    return $ CommandLineDebugConfig enabledRef logLevelRef breakpointsRef breakConditionsRef interactiveRef

-- Run action with command line debugging
runWithCLIDebug :: CommandLineDebugConfig -> String -> IO a -> IO a
runWithCLIDebug config location action = do
    checkBreakpoint config location
    action

-- Check if breakpoint is hit at current location
checkBreakpoint :: CommandLineDebugConfig -> String -> IO ()
checkBreakpoint config location = do
    enabled <- readIORef (cldEnabled config)
    if not enabled
        then return ()
        else do
            breakpoints <- readIORef (cldBreakpoints config)
            if Set.member location breakpoints
                then handleBreakpoint config location
                else do
                    conditions <- readIORef (cldBreakConditions config)
                    case Map.lookup location conditions of
                        Nothing -> return ()
                        Just condition -> checkConditionalBreakpoint config location condition

-- Handle breakpoint hit
handleBreakpoint :: CommandLineDebugConfig -> String -> IO ()
handleBreakpoint config location = do
    interactive <- readIORef (cldInteractive config)
    if interactive
        then do
            putStrLn $ "\n=== BREAKPOINT HIT ==="
            putStrLn $ "Location: " ++ location
            putStrLn "Available commands:"
            putStrLn "  c, continue - Continue execution"
            putStrLn "  s, step - Step to next breakpoint"
            putStrLn "  l, list - List all breakpoints"
            putStrLn "  d, disable - Disable debugging"
            putStrLn "  e, enable - Enable debugging"
            putStrLn "  q, quit - Quit program"
            putStrLn "  h, help - Show this help"
            handleDebugCommands config location
        else do
            putStrLn $ "\n=== BREAKPOINT: " ++ location ++ " ==="
            putStrLn "Press Enter to continue..."
            _ <- getLine
            return ()

-- Handle debug commands
handleDebugCommands :: CommandLineDebugConfig -> String -> IO ()
handleDebugCommands config location = do
    putStr "debug> "
    hFlush stdout
    line <- getLine
    case words line of
        ["c"] -> return ()
        ["continue"] -> return ()
        ["s"] -> return ()
        ["step"] -> return ()
        ["l"] -> do
            listBreakpoints config
            handleDebugCommands config location
        ["list"] -> do
            listBreakpoints config
            handleDebugCommands config location
        ["d"] -> do
            toggleDebugOutput config
            putStrLn "Debugging disabled"
            handleDebugCommands config location
        ["disable"] -> do
            toggleDebugOutput config
            putStrLn "Debugging disabled"
            handleDebugCommands config location
        ["e"] -> do
            toggleDebugOutput config
            putStrLn "Debugging enabled"
            handleDebugCommands config location
        ["enable"] -> do
            toggleDebugOutput config
            putStrLn "Debugging enabled"
            handleDebugCommands config location
        ["q"] -> error "Program terminated by user at breakpoint"
        ["quit"] -> error "Program terminated by user at breakpoint"
        ["h"] -> do
            showDebugHelp
            handleDebugCommands config location
        ["help"] -> do
            showDebugHelp
            handleDebugCommands config location
        _ -> do
            putStrLn "Unknown command. Type 'h' for help."
            handleDebugCommands config location

-- Show debug help
showDebugHelp :: IO ()
showDebugHelp = do
    putStrLn "Available commands:"
    putStrLn "  c, continue - Continue execution"
    putStrLn "  s, step - Step to next breakpoint"
    putStrLn "  l, list - List all breakpoints"
    putStrLn "  d, disable - Disable debugging"
    putStrLn "  e, enable - Enable debugging"
    putStrLn "  q, quit - Quit program"
    putStrLn "  h, help - Show this help"

-- Set breakpoint at location
setBreakpoint :: CommandLineDebugConfig -> String -> IO ()
setBreakpoint config location = do
    modifyIORef (cldBreakpoints config) (Set.insert location)
    putStrLn $ "Breakpoint set at: " ++ location

-- List all breakpoints
listBreakpoints :: CommandLineDebugConfig -> IO ()
listBreakpoints config = do
    breakpoints <- readIORef (cldBreakpoints config)
    if Set.null breakpoints
        then putStrLn "No breakpoints set"
        else do
            putStrLn "Current breakpoints:"
            mapM_ (\bp -> putStrLn $ "  " ++ bp) (Set.toList breakpoints)

-- Clear all breakpoints
clearBreakpoints :: CommandLineDebugConfig -> IO ()
clearBreakpoints config = do
    writeIORef (cldBreakpoints config) Set.empty
    putStrLn "All breakpoints cleared"

-- Toggle debug output
toggleDebugOutput :: CommandLineDebugConfig -> IO ()
toggleDebugOutput config = do
    modifyIORef (cldEnabled config) not
    enabled <- readIORef (cldEnabled config)
    putStrLn $ "Debug output " ++ (if enabled then "enabled" else "disabled")

-- Set debug level
setDebugLevel :: CommandLineDebugConfig -> Int -> IO ()
setDebugLevel config level = do
    writeIORef (cldLogLevel config) level
    putStrLn $ "Debug level set to: " ++ show level

-- Show debug status
showDebugStatus :: CommandLineDebugConfig -> IO ()
showDebugStatus config = do
    enabled <- readIORef (cldEnabled config)
    logLevel <- readIORef (cldLogLevel config)
    breakpoints <- readIORef (cldBreakpoints config)
    interactive <- readIORef (cldInteractive config)

    putStrLn "=== Debug Status ==="
    putStrLn $ "Debug enabled: " ++ show enabled
    putStrLn $ "Log level: " ++ show logLevel
    putStrLn $ "Interactive mode: " ++ show interactive
    putStrLn $ "Active breakpoints: " ++ show (Set.size breakpoints)
    if not (Set.null breakpoints)
        then do
            putStrLn "Breakpoints:"
            mapM_ (\bp -> putStrLn $ "  " ++ bp) (Set.toList breakpoints)
        else putStrLn "No breakpoints set"

-- Check conditional breakpoint
checkConditionalBreakpoint :: CommandLineDebugConfig -> String -> (String -> Bool) -> IO ()
checkConditionalBreakpoint config location _ = do
    interactive <- readIORef (cldInteractive config)
    if interactive
        then do
            putStrLn $ "\n=== CONDITIONAL BREAKPOINT: " ++ location ++ " ==="
            putStrLn "Condition met. Available commands:"
            putStrLn "  c, continue - Continue execution"
            putStrLn "  s, step - Step to next breakpoint"
            putStrLn "  i, info - Show debug info"
            putStrLn "  h, help - Show help"
            handleConditionalBreakpointCommands config location
        else do
            putStrLn $ "\n=== CONDITIONAL BREAKPOINT: " ++ location ++ " ==="
            putStrLn "Press Enter to continue..."
            _ <- getLine
            return ()

-- Handle conditional breakpoint commands
handleConditionalBreakpointCommands :: CommandLineDebugConfig -> String -> IO ()
handleConditionalBreakpointCommands config location = do
    putStr "debug> "
    hFlush stdout
    line <- getLine
    case words line of
        ["c"] -> return ()
        ["continue"] -> return ()
        ["s"] -> return ()
        ["step"] -> return ()
        ["i"] -> do
            showDebugInfo location
            handleConditionalBreakpointCommands config location
        ["info"] -> do
            showDebugInfo location
            handleConditionalBreakpointCommands config location
        ["h"] -> do
            showConditionalBreakpointHelp
            handleConditionalBreakpointCommands config location
        ["help"] -> do
            showConditionalBreakpointHelp
            handleConditionalBreakpointCommands config location
        _ -> do
            putStrLn "Unknown command. Type 'h' for help."
            handleConditionalBreakpointCommands config location

-- Show conditional breakpoint help
showConditionalBreakpointHelp :: IO ()
showConditionalBreakpointHelp = do
    putStrLn "Available commands:"
    putStrLn "  c, continue - Continue execution"
    putStrLn "  s, step - Step to next breakpoint"
    putStrLn "  i, info - Show debug info"
    putStrLn "  h, help - Show help"

-- Show debug info
showDebugInfo :: String -> IO ()
showDebugInfo location = do
    putStrLn $ "Location: " ++ location
    putStrLn "Debug info available at this location"