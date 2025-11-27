{-# LANGUAGE OverloadedStrings #-}

module CommandLineDebug
    ( CommandLineDebugConfig(..)
    , defaultCLIDebugConfig
    , enableDebugConsoleOutput
    , disableDebugConsoleOutput
    , runWithCLIDebug
    , checkBreakpoint
    , setBreakpoint
    , listBreakpoints
    , clearBreakpoints
    , toggleDebugOutput
    , DebugCommandResult(..)
    , processDebugCommand
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
    , cldOutput :: IORef (Bool -> String -> IO ())
    }

-- Default command line debug configuration
defaultCLIDebugConfig :: IO CommandLineDebugConfig
defaultCLIDebugConfig = do
    enabledRef <- newIORef True
    logLevelRef <- newIORef 3
    breakpointsRef <- newIORef Set.empty
    breakConditionsRef <- newIORef Map.empty
    interactiveRef <- newIORef True
    outputRef <- newIORef silentOutput
    return $ CommandLineDebugConfig enabledRef logLevelRef breakpointsRef breakConditionsRef interactiveRef outputRef

type OutputWriter = Bool -> String -> IO ()

silentOutput :: OutputWriter
silentOutput _ _ = pure ()

consoleOutput :: OutputWriter
consoleOutput newline message =
    if newline
        then putStrLn message
        else do
            putStr message
            hFlush stdout

setDebugOutputChannel :: CommandLineDebugConfig -> OutputWriter -> IO ()
setDebugOutputChannel config writer = writeIORef (cldOutput config) writer

enableDebugConsoleOutput :: CommandLineDebugConfig -> IO ()
enableDebugConsoleOutput config = setDebugOutputChannel config consoleOutput

disableDebugConsoleOutput :: CommandLineDebugConfig -> IO ()
disableDebugConsoleOutput config = setDebugOutputChannel config silentOutput

logWith :: CommandLineDebugConfig -> Bool -> String -> IO ()
logWith config newline message = do
    writer <- readIORef (cldOutput config)
    writer newline message

logLine :: CommandLineDebugConfig -> String -> IO ()
logLine config = logWith config True

logPrompt :: CommandLineDebugConfig -> String -> IO ()
logPrompt config = logWith config False

logLines :: CommandLineDebugConfig -> [String] -> IO ()
logLines config = mapM_ (logLine config)

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
            logLines config
                [ "\n=== BREAKPOINT HIT ==="
                , "Location: " ++ location
                , "Available commands:"
                , "  c, continue - Continue execution"
                , "  s, step - Step to next breakpoint"
                , "  l, list - List all breakpoints"
                , "  d, disable - Disable debugging"
                , "  e, enable - Enable debugging"
                , "  q, quit - Quit program"
                , "  h, help - Show this help"
                ]
            handleDebugCommands config location
        else do
            logLines config
                [ "\n=== BREAKPOINT: " ++ location ++ " ==="
                , "Press Enter to continue..."
                ]
            _ <- getLine
            return ()

-- Handle debug commands
handleDebugCommands :: CommandLineDebugConfig -> String -> IO ()
handleDebugCommands config location = do
    logPrompt config "debug> "
    line <- getLine
    result <- processDebugCommand config location (words line)
    case result of
        ResumeExecution -> return ()
        AwaitMoreInput -> handleDebugCommands config location

-- Result of handling a debug command
data DebugCommandResult
    = ResumeExecution
    | AwaitMoreInput
    deriving (Eq, Show)

processDebugCommand :: CommandLineDebugConfig -> String -> [String] -> IO DebugCommandResult
processDebugCommand config _ tokens =
    case tokens of
        ["c"] -> return ResumeExecution
        ["continue"] -> return ResumeExecution
        ["s"] -> return ResumeExecution
        ["step"] -> return ResumeExecution
        ["l"] -> do
            listBreakpoints config
            return AwaitMoreInput
        ["list"] -> do
            listBreakpoints config
            return AwaitMoreInput
        ["d"] -> disableDebugging >> return AwaitMoreInput
        ["disable"] -> disableDebugging >> return AwaitMoreInput
        ["e"] -> enableDebugging >> return AwaitMoreInput
        ["enable"] -> enableDebugging >> return AwaitMoreInput
        ["q"] -> error "Program terminated by user at breakpoint"
        ["quit"] -> error "Program terminated by user at breakpoint"
        ["h"] -> do
            showDebugHelp config
            return AwaitMoreInput
        ["help"] -> do
            showDebugHelp config
            return AwaitMoreInput
        _ -> do
            logLine config "Unknown command. Type 'h' for help."
            return AwaitMoreInput
  where
    disableDebugging = applyState False "Debugging disabled"
    enableDebugging = applyState True "Debugging enabled"

    applyState newState statusMessage = do
        writeIORef (cldEnabled config) newState
        logLine config $ "Debug output " ++ if newState then "enabled" else "disabled"
        logLine config statusMessage

-- Show debug help
showDebugHelp :: CommandLineDebugConfig -> IO ()
showDebugHelp config =
    logLines config
        [ "Available commands:"
        , "  c, continue - Continue execution"
        , "  s, step - Step to next breakpoint"
        , "  l, list - List all breakpoints"
        , "  d, disable - Disable debugging"
        , "  e, enable - Enable debugging"
        , "  q, quit - Quit program"
        , "  h, help - Show this help"
        ]

-- Set breakpoint at location
setBreakpoint :: CommandLineDebugConfig -> String -> IO ()
setBreakpoint config location = do
    modifyIORef (cldBreakpoints config) (Set.insert location)
    logLine config $ "Breakpoint set at: " ++ location

-- List all breakpoints
listBreakpoints :: CommandLineDebugConfig -> IO ()
listBreakpoints config = do
    breakpoints <- readIORef (cldBreakpoints config)
    if Set.null breakpoints
        then logLine config "No breakpoints set"
        else do
            logLine config "Current breakpoints:"
            mapM_ (logLine config . ("  " ++)) (Set.toList breakpoints)

-- Clear all breakpoints
clearBreakpoints :: CommandLineDebugConfig -> IO ()
clearBreakpoints config = do
    writeIORef (cldBreakpoints config) Set.empty
    logLine config "All breakpoints cleared"

-- Toggle debug output
toggleDebugOutput :: CommandLineDebugConfig -> IO ()
toggleDebugOutput config = do
    modifyIORef (cldEnabled config) not
    enabled <- readIORef (cldEnabled config)
    logLine config $ "Debug output " ++ (if enabled then "enabled" else "disabled")

-- Set debug level
setDebugLevel :: CommandLineDebugConfig -> Int -> IO ()
setDebugLevel config level = do
    writeIORef (cldLogLevel config) level
    logLine config $ "Debug level set to: " ++ show level

-- Show debug status
showDebugStatus :: CommandLineDebugConfig -> IO ()
showDebugStatus config = do
    enabled <- readIORef (cldEnabled config)
    logLevel <- readIORef (cldLogLevel config)
    breakpoints <- readIORef (cldBreakpoints config)
    interactive <- readIORef (cldInteractive config)

    logLines config
        [ "=== Debug Status ==="
        , "Debug enabled: " ++ show enabled
        , "Log level: " ++ show logLevel
        , "Interactive mode: " ++ show interactive
        , "Active breakpoints: " ++ show (Set.size breakpoints)
        ]
    if not (Set.null breakpoints)
        then do
            logLine config "Breakpoints:"
            mapM_ (logLine config . ("  " ++)) (Set.toList breakpoints)
        else logLine config "No breakpoints set"

-- Check conditional breakpoint
checkConditionalBreakpoint :: CommandLineDebugConfig -> String -> (String -> Bool) -> IO ()
checkConditionalBreakpoint config location condition = do
    let locationState = location
    if not (condition locationState)
        then pure ()
        else do
            interactive <- readIORef (cldInteractive config)
            if interactive
                then do
                    logLines config
                        [ "\n=== CONDITIONAL BREAKPOINT: " ++ location ++ " ==="
                        , "Condition met. Available commands:"
                        , "  c, continue - Continue execution"
                        , "  s, step - Step to next breakpoint"
                        , "  i, info - Show debug info"
                        , "  h, help - Show help"
                        ]
                    handleConditionalBreakpointCommands config location
                else do
                    logLines config
                        [ "\n=== CONDITIONAL BREAKPOINT: " ++ location ++ " ==="
                        , "Press Enter to continue..."
                        ]
                    _ <- getLine
                    return ()

-- Handle conditional breakpoint commands
handleConditionalBreakpointCommands :: CommandLineDebugConfig -> String -> IO ()
handleConditionalBreakpointCommands config location = do
    logPrompt config "debug> "
    line <- getLine
    case words line of
        ["c"] -> return ()
        ["continue"] -> return ()
        ["s"] -> return ()
        ["step"] -> return ()
        ["i"] -> do
            showDebugInfo config location
            handleConditionalBreakpointCommands config location
        ["info"] -> do
            showDebugInfo config location
            handleConditionalBreakpointCommands config location
        ["h"] -> do
            showConditionalBreakpointHelp config
            handleConditionalBreakpointCommands config location
        ["help"] -> do
            showConditionalBreakpointHelp config
            handleConditionalBreakpointCommands config location
        _ -> do
            logLine config "Unknown command. Type 'h' for help."
            handleConditionalBreakpointCommands config location

-- Show conditional breakpoint help
showConditionalBreakpointHelp :: CommandLineDebugConfig -> IO ()
showConditionalBreakpointHelp config =
    logLines config
        [ "Available commands:"
        , "  c, continue - Continue execution"
        , "  s, step - Step to next breakpoint"
        , "  i, info - Show debug info"
        , "  h, help - Show help"
        ]

-- Show debug info
showDebugInfo :: CommandLineDebugConfig -> String -> IO ()
showDebugInfo location = do
    putStrLn $ "Location: " ++ location
    putStrLn "Debug info available at this location"