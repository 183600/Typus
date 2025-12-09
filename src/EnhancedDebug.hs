{-# LANGUAGE OverloadedStrings #-}

module EnhancedDebug
    ( LogLevel(..)
    , DebugLogger(..)
    , EnhancedDebugConfig(..)
    , defaultEnhancedDebugConfig
    , withEnhancedDebug
    , logDebug
    , logInfo
    , logWarning
    , logError
    , logWithLevel
    , setLogLevel
    , addLogOutput
    , createBreakpoint
    , createConditionalBreakpoint
    , checkAndHandleBreakpoint
    , debugPrint
    , debugPrintLn
    , debugTrace
    , debugEnterFunction
    , debugExitFunction
    , debugMeasureTime
    , getDebugStats
    , resetDebugStats
    ) where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- Log levels
data LogLevel = Debug | Info | Warning | Error
    deriving (Eq, Ord, Show, Enum)

-- Debug logger configuration
data DebugLogger = DebugLogger
    { dlLogLevel :: IORef LogLevel
    , dlOutputs :: IORef [LogLevel -> String -> IO ()]
    , dlLogCount :: IORef (Map LogLevel Int)
    }

-- Enhanced debug configuration with breakpoints
data EnhancedDebugConfig = EnhancedDebugConfig
    { edcLogger :: DebugLogger
    , edcBreakpoints :: IORef (Set String)
    , edcConditionalBreakpoints :: IORef (Map String (String -> Bool))
    , edcFunctionStack :: IORef [String]
    , edcExecutionCounts :: IORef (Map String Int)
    , edcTimings :: IORef (Map String Double)
    , edcBreakpointHitCount :: IORef (Map String Int)
    }

-- Default enhanced debug configuration
defaultEnhancedDebugConfig :: IO EnhancedDebugConfig
defaultEnhancedDebugConfig = do
    logLevelRef <- newIORef Info
    outputsRef <- newIORef [defaultLogOutput]
    logCountRef <- newIORef Map.empty
    
    let logger = DebugLogger logLevelRef outputsRef logCountRef
    
    breakpointsRef <- newIORef Set.empty
    conditionalBreakpointsRef <- newIORef Map.empty
    functionStackRef <- newIORef []
    executionCountsRef <- newIORef Map.empty
    timingsRef <- newIORef Map.empty
    breakpointHitCountRef <- newIORef Map.empty
    
    return $ EnhancedDebugConfig 
        logger 
        breakpointsRef 
        conditionalBreakpointsRef 
        functionStackRef 
        executionCountsRef 
        timingsRef 
        breakpointHitCountRef

-- Default log output function
defaultLogOutput :: LogLevel -> String -> IO ()
defaultLogOutput level msg = do
    let prefix :: String
        prefix = case level of
            Debug -> "[DEBUG]"
            Info -> "[INFO]"
            Warning -> "[WARN]"
            Error -> "[ERROR]"
    putStrLn $ prefix ++ " " ++ msg

-- Run action with enhanced debugging
withEnhancedDebug :: EnhancedDebugConfig -> String -> IO a -> IO a
withEnhancedDebug config location action = do
    debugEnterFunction config location
    checkAndHandleBreakpoint config location
    result <- action
    debugExitFunction config location
    return result

-- Log messages at different levels
logDebug :: EnhancedDebugConfig -> String -> IO ()
logDebug = logWithLevel Debug

logInfo :: EnhancedDebugConfig -> String -> IO ()
logInfo = logWithLevel Info

logWarning :: EnhancedDebugConfig -> String -> IO ()
logWarning = logWithLevel Warning

logError :: EnhancedDebugConfig -> String -> IO ()
logError = logWithLevel Error

-- Log with specific level
logWithLevel :: LogLevel -> EnhancedDebugConfig -> String -> IO ()
logWithLevel level config message = do
    currentLevel <- readIORef (dlLogLevel (edcLogger config))
    when (level >= currentLevel) $ do
        outputs <- readIORef (dlOutputs (edcLogger config))
        mapM_ (\output -> output level message) outputs
        
        -- Update log count
        modifyIORef (dlLogCount (edcLogger config)) 
            (Map.insertWith (+) level 1)

-- Set log level
setLogLevel :: EnhancedDebugConfig -> LogLevel -> IO ()
setLogLevel config level = do
    writeIORef (dlLogLevel (edcLogger config)) level
    logInfo config $ "Log level set to: " ++ show level

-- Add log output
addLogOutput :: EnhancedDebugConfig -> (LogLevel -> String -> IO ()) -> IO ()
addLogOutput config output = do
    modifyIORef (dlOutputs (edcLogger config)) (output :)

-- Create breakpoint
createBreakpoint :: EnhancedDebugConfig -> String -> IO ()
createBreakpoint config location = do
    modifyIORef (edcBreakpoints config) (Set.insert location)
    logInfo config $ "Breakpoint set at: " ++ location

-- Create conditional breakpoint
createConditionalBreakpoint :: EnhancedDebugConfig -> String -> (String -> Bool) -> IO ()
createConditionalBreakpoint config location condition = do
    modifyIORef (edcConditionalBreakpoints config) (Map.insert location condition)
    logInfo config $ "Conditional breakpoint set at: " ++ location

-- Check and handle breakpoint
checkAndHandleBreakpoint :: EnhancedDebugConfig -> String -> IO ()
checkAndHandleBreakpoint config location = do
    -- Update execution count
    modifyIORef (edcExecutionCounts config) 
        (Map.insertWith (+) location 1)
    
    -- Check regular breakpoints
    breakpoints <- readIORef (edcBreakpoints config)
    let isBreakpoint = Set.member location breakpoints
    
    -- Check conditional breakpoints
    conditionalBreakpoints <- readIORef (edcConditionalBreakpoints config)
    conditionalResults <- mapM (\condition -> evaluate (condition location)) 
                              (Map.elems conditionalBreakpoints)
    let hasConditionalBreakpoint = Map.member location conditionalBreakpoints
        conditionMet = hasConditionalBreakpoint && 
                      (case Map.lookup location conditionalBreakpoints of
                          Just _ -> or conditionalResults
                          Nothing -> False)
    
    when (isBreakpoint || conditionMet) $ do
        -- Update breakpoint hit count
        modifyIORef (edcBreakpointHitCount config) 
            (Map.insertWith (+) location 1)
        
        handleBreakpoint config location isBreakpoint conditionMet

-- Handle breakpoint hit
handleBreakpoint :: EnhancedDebugConfig -> String -> Bool -> Bool -> IO ()
handleBreakpoint config location isRegular isConditional = do
    let breakpointType :: String
        breakpointType = if isRegular && isConditional 
                            then "REGULAR + CONDITIONAL"
                         else if isRegular 
                            then "REGULAR"
                         else "CONDITIONAL"
    
    logInfo config $ "\n=== " ++ breakpointType ++ " BREAKPOINT ==="
    logInfo config $ "Location: " ++ location
    
    -- Show function stack
    stack <- readIORef (edcFunctionStack config)
    logInfo config "Function stack:"
    mapM_ (\fn -> logInfo config $ "  " ++ fn) (reverse stack)
    
    -- Show execution count
    counts <- readIORef (edcExecutionCounts config)
    case Map.lookup location counts of
        Just count -> logInfo config $ "Execution count: " ++ show count
        Nothing -> return ()
    
    putStrLn "\nBreakpoint commands:"
    putStrLn "  c, continue - Continue execution"
    putStrLn "  s, stack - Show function stack"
    putStrLn "  i, info - Show debug info"
    putStrLn "  t, trace - Enable/disable tracing"
    putStrLn "  h, help - Show help"
    putStrLn "  q, quit - Quit program"
    
    handleBreakpointCommands config location

-- Handle breakpoint commands
handleBreakpointCommands :: EnhancedDebugConfig -> String -> IO ()
handleBreakpointCommands config location = do
    putStr "debug> "
    hFlush stdout
    line <- getLine
    case words line of
        ["c"] -> return ()
        ["continue"] -> return ()
        ["s"] -> do
            showFunctionStack config
            handleBreakpointCommands config location
        ["stack"] -> do
            showFunctionStack config
            handleBreakpointCommands config location
        ["i"] -> do
            showDebugInfo config location
            handleBreakpointCommands config location
        ["info"] -> do
            showDebugInfo config location
            handleBreakpointCommands config location
        ["t"] -> do
            toggleTracing config
            handleBreakpointCommands config location
        ["trace"] -> do
            toggleTracing config
            handleBreakpointCommands config location
        ["h"] -> do
            showBreakpointHelp
            handleBreakpointCommands config location
        ["help"] -> do
            showBreakpointHelp
            handleBreakpointCommands config location
        ["q"] -> error "Program terminated by user at breakpoint"
        ["quit"] -> error "Program terminated by user at breakpoint"
        _ -> do
            putStrLn "Unknown command. Type 'h' for help."
            handleBreakpointCommands config location

-- Show function stack
showFunctionStack :: EnhancedDebugConfig -> IO ()
showFunctionStack config = do
    stack <- readIORef (edcFunctionStack config)
    putStrLn "\nFunction Stack:"
    if null stack
        then putStrLn "  (empty)"
        else mapM_ (\fn -> putStrLn $ "  " ++ fn) (reverse stack)

-- Show debug info
showDebugInfo :: EnhancedDebugConfig -> String -> IO ()
showDebugInfo config location = do
    putStrLn "\n=== Debug Info ==="
    putStrLn $ "Current location: " ++ location
    
    counts <- readIORef (edcExecutionCounts config)
    putStrLn $ "Execution counts: " ++ show (Map.size counts) ++ " locations tracked"
    
    hitCounts <- readIORef (edcBreakpointHitCount config)
    putStrLn $ "Breakpoint hits: " ++ show (Map.size hitCounts) ++ " breakpoints hit"
    
    logCounts <- readIORef (dlLogCount (edcLogger config))
    putStrLn $ "Log messages: " ++ show (Map.size logCounts) ++ " types logged"
    mapM_ (\(level, count) -> 
        putStrLn $ "  " ++ show level ++ ": " ++ show count) (Map.toList logCounts)

-- Toggle tracing
toggleTracing :: EnhancedDebugConfig -> IO ()
toggleTracing config = do
    currentLevel <- readIORef (dlLogLevel (edcLogger config))
    let newLevel = if currentLevel == Debug then Info else Debug
    setLogLevel config newLevel
    putStrLn $ "Tracing " ++ (if newLevel == Debug then "enabled" else "disabled")

-- Show breakpoint help
showBreakpointHelp :: IO ()
showBreakpointHelp = do
    putStrLn "\nBreakpoint Commands:"
    putStrLn "  c, continue - Continue execution"
    putStrLn "  s, stack - Show function stack"
    putStrLn "  i, info - Show debug info"
    putStrLn "  t, trace - Enable/disable tracing"
    putStrLn "  h, help - Show this help"
    putStrLn "  q, quit - Quit program"

-- Debug print functions
debugPrint :: EnhancedDebugConfig -> String -> IO ()
debugPrint _ msg = putStr msg >> hFlush stdout

debugPrintLn :: EnhancedDebugConfig -> String -> IO ()
debugPrintLn _ msg = putStrLn msg

-- Debug trace
debugTrace :: EnhancedDebugConfig -> String -> IO a -> IO a
debugTrace config label action = do
    logDebug config $ "TRACE: Entering " ++ label
    result <- action
    logDebug config $ "TRACE: Exiting " ++ label
    return result

-- Enter function
debugEnterFunction :: EnhancedDebugConfig -> String -> IO ()
debugEnterFunction config functionName = do
    modifyIORef (edcFunctionStack config) (functionName :)
    logDebug config $ "ENTER: " ++ functionName

-- Exit function
debugExitFunction :: EnhancedDebugConfig -> String -> IO ()
debugExitFunction config functionName = do
    stack <- readIORef (edcFunctionStack config)
    case stack of
        [] -> logWarning config $ "EXIT: " ++ functionName ++ " (empty stack)"
        (top:rest) -> 
            if top == functionName
                then do
                    writeIORef (edcFunctionStack config) rest
                    logDebug config $ "EXIT: " ++ functionName
                else logWarning config $ "EXIT: " ++ functionName ++ " (expected " ++ top ++ ")"

-- Measure execution time
debugMeasureTime :: EnhancedDebugConfig -> String -> IO a -> IO a
debugMeasureTime config label action = do
    startTime <- getCurrentTime
    result <- action
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime :: Double
    modifyIORef (edcTimings config) (Map.insertWith (+) label duration)
    logDebug config $ "TIME: " ++ label ++ " took " ++ printf "%.3f" duration ++ "s"
    return result

-- Get debug statistics
getDebugStats :: EnhancedDebugConfig -> IO (Map String Int, Map String Double, Map LogLevel Int)
getDebugStats config = do
    executionCounts <- readIORef (edcExecutionCounts config)
    timings <- readIORef (edcTimings config)
    logCounts <- readIORef (dlLogCount (edcLogger config))
    return (executionCounts, timings, logCounts)

-- Reset debug statistics
resetDebugStats :: EnhancedDebugConfig -> IO ()
resetDebugStats config = do
    writeIORef (edcExecutionCounts config) Map.empty
    writeIORef (edcTimings config) Map.empty
    writeIORef (dlLogCount (edcLogger config)) Map.empty
    writeIORef (edcBreakpointHitCount config) Map.empty
    logInfo config "Debug statistics reset"