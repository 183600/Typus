{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module CommandLineDebug
    ( CommandLineDebugConfig(..)
    , defaultCLIDebugConfig
    , runWithCLIDebug
    , checkBreakpoint
    , setBreakpoint
    , setConditionalBreakpoint
    , listBreakpoints
    , printBreakpoints
    , clearBreakpoints
    , toggleDebugOutput
    , DebugCommandResult(..)
    , processDebugCommand
    , processDebugCommandWithOutput
    , setDebugLevel
    , showDebugStatus
    , printDebugStatus
    , addWatchVariable
    , removeWatchVariable
    , listWatchVariables
    , getCallStack
    , pushCallStack
    , popCallStack
    , evaluateExpression
    , stepInto
    , stepOver
    , stepOut
    , continue
    , runToCursor
    ) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
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
    , cldCallStack :: IORef [String]
    , cldWatchVariables :: IORef (Map String String)
    , cldStepMode :: IORef Bool
    , cldCurrentLocation :: IORef String
    }

-- Default command line debug configuration
defaultCLIDebugConfig :: IO CommandLineDebugConfig
defaultCLIDebugConfig = do
    enabledRef <- newIORef True
    logLevelRef <- newIORef 3
    breakpointsRef <- newIORef Set.empty
    breakConditionsRef <- newIORef Map.empty
    interactiveRef <- newIORef True
    callStackRef <- newIORef []
    watchVarsRef <- newIORef Map.empty
    stepModeRef <- newIORef False
    currentLocationRef <- newIORef ""
    return $ CommandLineDebugConfig enabledRef logLevelRef breakpointsRef breakConditionsRef interactiveRef callStackRef watchVarsRef stepModeRef currentLocationRef

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
    result <- processDebugCommand config line
    case result of
        ResumeExecution -> return ()
        AwaitMoreInput -> handleDebugCommands config location

-- Result of handling a debug command
data DebugCommandResult
    = ResumeExecution
    | AwaitMoreInput
    deriving stock (Eq, Show)

processDebugCommand :: CommandLineDebugConfig -> String -> IO DebugCommandResult
processDebugCommand config commandStr = processDebugCommandWithOutput True config "" (words commandStr)

_processDebugCommandWithArgs :: CommandLineDebugConfig -> String -> [String] -> IO DebugCommandResult
_processDebugCommandWithArgs = processDebugCommandWithOutput True

-- Internal version with output control
processDebugCommandWithOutput :: Bool -> CommandLineDebugConfig -> String -> [String] -> IO DebugCommandResult
processDebugCommandWithOutput enableOutput config _ tokens =
    case tokens of
        ["c"] -> return ResumeExecution
        ["continue"] -> return ResumeExecution
        ["s"] -> return ResumeExecution
        ["step"] -> return ResumeExecution
        ["l"] -> do
            when enableOutput $ printBreakpoints config
            return AwaitMoreInput
        ["list"] -> do
            when enableOutput $ printBreakpoints config
            return AwaitMoreInput
        ["d"] -> disableDebugging >> return AwaitMoreInput
        ["disable"] -> disableDebugging >> return AwaitMoreInput
        ["e"] -> enableDebugging >> return AwaitMoreInput
        ["enable"] -> enableDebugging >> return AwaitMoreInput
        ["q"] -> error "Program terminated by user at breakpoint"
        ["quit"] -> error "Program terminated by user at breakpoint"
        ["h"] -> do
            when enableOutput $ showDebugHelp
            return AwaitMoreInput
        ["help"] -> do
            when enableOutput $ showDebugHelp
            return AwaitMoreInput
        _ -> do
            when enableOutput $ putStrLn "Unknown command. Type 'h' for help."
            return AwaitMoreInput
  where
    disableDebugging = applyState False "Debugging disabled"
    enableDebugging = applyState True "Debugging enabled"

    applyState newState statusMessage = do
        writeIORef (cldEnabled config) newState
        when enableOutput $ do
            putStrLn $ "Debug output " ++ if newState then "enabled" else "disabled"
            putStrLn statusMessage

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
    modifyIORef' (cldBreakpoints config) (Set.insert location)
    putStrLn $ "Breakpoint set at: " ++ location

-- List all breakpoints
listBreakpoints :: CommandLineDebugConfig -> IO [String]
listBreakpoints config = do
    breakpoints <- readIORef (cldBreakpoints config)
    return (Set.toList breakpoints)

-- Print all breakpoints
printBreakpoints :: CommandLineDebugConfig -> IO ()
printBreakpoints config = do
    breakpoints <- listBreakpoints config
    if null breakpoints
        then putStrLn "No breakpoints set"
        else do
            putStrLn "Current breakpoints:"
            mapM_ (\bp -> putStrLn $ "  " ++ bp) breakpoints

-- Clear all breakpoints
clearBreakpoints :: CommandLineDebugConfig -> IO ()
clearBreakpoints config = do
    writeIORef (cldBreakpoints config) Set.empty
    putStrLn "All breakpoints cleared"

-- Toggle debug output
toggleDebugOutput :: CommandLineDebugConfig -> IO ()
toggleDebugOutput config = do
    modifyIORef' (cldEnabled config) not
    enabled <- readIORef (cldEnabled config)
    putStrLn $ "Debug output " ++ (if enabled then "enabled" else "disabled")

-- Set debug level
setDebugLevel :: CommandLineDebugConfig -> Int -> IO ()
setDebugLevel config level = do
    writeIORef (cldLogLevel config) level
    putStrLn $ "Debug level set to: " ++ show level

-- Show debug status
showDebugStatus :: CommandLineDebugConfig -> IO String
showDebugStatus config = do
    enabled <- readIORef (cldEnabled config)
    logLevel <- readIORef (cldLogLevel config)
    breakpoints <- readIORef (cldBreakpoints config)
    interactive <- readIORef (cldInteractive config)

    let statusLines = 
            [ "=== Debug Status ==="
            , "Debug enabled: " ++ show enabled
            , "Log level: " ++ show logLevel
            , "Interactive mode: " ++ show interactive
            , "Active breakpoints: " ++ show (Set.size breakpoints)
            ] ++ 
            (if not (Set.null breakpoints)
                then "Breakpoints:" : map ("  " ++) (Set.toList breakpoints)
                else ["No breakpoints set"])
    return (unlines statusLines)

-- Print debug status
printDebugStatus :: CommandLineDebugConfig -> IO ()
printDebugStatus config = do
    status <- showDebugStatus config
    putStrLn status

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

-- Enhanced debugging functions

-- Set conditional breakpoint
setConditionalBreakpoint :: CommandLineDebugConfig -> String -> (String -> Bool) -> IO ()
setConditionalBreakpoint config location condition = do
    modifyIORef' (cldBreakConditions config) (Map.insert location condition)
    putStrLn $ "Conditional breakpoint set at: " ++ location

-- Watch variable
addWatchVariable :: CommandLineDebugConfig -> String -> String -> IO ()
addWatchVariable config varName value = do
    modifyIORef' (cldWatchVariables config) (Map.insert varName value)
    putStrLn $ "Watching variable: " ++ varName ++ " = " ++ value

-- Remove watch variable
removeWatchVariable :: CommandLineDebugConfig -> String -> IO ()
removeWatchVariable config varName = do
    modifyIORef' (cldWatchVariables config) (Map.delete varName)
    putStrLn $ "Stopped watching variable: " ++ varName

-- List watch variables
listWatchVariables :: CommandLineDebugConfig -> IO [(String, String)]
listWatchVariables config = do
    watchVars <- readIORef (cldWatchVariables config)
    return (Map.toList watchVars)

-- Print watch variables
_printWatchVariables :: CommandLineDebugConfig -> IO ()
_printWatchVariables config = do
    watchVars <- listWatchVariables config
    if null watchVars
        then putStrLn "No watch variables set"
        else do
            putStrLn "Watch variables:"
            mapM_ (\(name, value) -> putStrLn $ "  " ++ name ++ " = " ++ value) watchVars

-- Get call stack
getCallStack :: CommandLineDebugConfig -> IO [String]
getCallStack config = readIORef (cldCallStack config)

-- Push to call stack
pushCallStack :: CommandLineDebugConfig -> String -> IO ()
pushCallStack config location = do
    modifyIORef' (cldCallStack config) (location :)
    writeIORef (cldCurrentLocation config) location

-- Pop from call stack
popCallStack :: CommandLineDebugConfig -> IO ()
popCallStack config = do
    stack <- readIORef (cldCallStack config)
    case stack of
        [] -> return ()
        (_:rest) -> do
            writeIORef (cldCallStack config) rest
            case rest of
                [] -> writeIORef (cldCurrentLocation config) ""
                (loc:_) -> writeIORef (cldCurrentLocation config) loc

-- Evaluate expression (simplified)
evaluateExpression :: CommandLineDebugConfig -> String -> IO String
evaluateExpression _config expr = do
    return $ "Expression evaluated: " ++ expr ++ " = <result>"

-- Step debugging functions
stepInto :: CommandLineDebugConfig -> IO ()
stepInto config = do
    writeIORef (cldStepMode config) True
    putStrLn "Step into mode enabled"

stepOver :: CommandLineDebugConfig -> IO ()
stepOver config = do
    writeIORef (cldStepMode config) True
    putStrLn "Step over mode enabled"

stepOut :: CommandLineDebugConfig -> IO ()
stepOut config = do
    writeIORef (cldStepMode config) False
    putStrLn "Step out - continuing execution"

continue :: CommandLineDebugConfig -> IO ()
continue config = do
    writeIORef (cldStepMode config) False
    putStrLn "Continuing execution"

-- Run to cursor
runToCursor :: CommandLineDebugConfig -> String -> IO ()
runToCursor config location = do
    setBreakpoint config location
    continue config