#!/usr/bin/env stack
-- stack --resolver lts-20.0 runghc --package base,containers,mtl

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, forever)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)

import Debug
import CommandLineDebug

main :: IO ()
main = do
    args <- getArgs
    config <- defaultCLIDebugConfig

    putStrLn "=== Typus Debug CLI ==="
    putStrLn "Type 'help' for available commands or 'quit' to exit"

    case args of
        [] -> runInteractiveDebug config
        ["--help"] -> showHelp
        ["-h"] -> showHelp
        _ -> putStrLn "Usage: debug-cli [--help]"

    return ()

runInteractiveDebug :: CommandLineDebugConfig -> IO ()
runInteractiveDebug config = forever $ do
    putStr "debug> "
    hFlush stdout
    line <- getLine
    case words line of
        [] -> return ()
        ["help"] -> showHelp
        ["h"] -> showHelp
        ["quit"] -> do
            putStrLn "Exiting debug CLI..."
            return ()
        ["q"] -> do
            putStrLn "Exiting debug CLI..."
            return ()
        ["status"] -> showDebugStatus config
        ["s"] -> showDebugStatus config
        ["set", "breakpoint", location] -> setBreakpoint config location
        ["break", location] -> setBreakpoint config location
        ["b", location] -> setBreakpoint config location
        ["list"] -> listBreakpoints config
        ["l"] -> listBreakpoints config
        ["clear"] -> clearBreakpoints config
        ["c"] -> clearBreakpoints config
        ["toggle"] -> toggleDebugOutput config
        ["t"] -> toggleDebugOutput config
        ["level", levelStr] ->
            case readMaybe levelStr of
                Just level -> setDebugLevel config level
                Nothing -> putStrLn $ "Invalid level: " ++ levelStr
        ["enable"] -> do
            writeIORef (cldEnabled config) True
            putStrLn "Debug enabled"
        ["disable"] -> do
            writeIORef (cldEnabled config) False
            putStrLn "Debug disabled"
        ["test", location] -> do
            putStrLn $ "Testing breakpoint at: " ++ location
            checkBreakpoint config location
        ["log", location, message] -> do
            debugLog location message
        ["info", location] -> do
            debugInfo location $ "Info at " ++ location
        ["warn", location, message] -> do
            debugWarn location message
        ["error", location, message] -> do
            debugError location message
        ["trace", location, message] -> do
            debugTrace location message
        _ -> do
            let firstWord = takeWhile (/= ' ') line
            putStrLn $ "Unknown command: " ++ firstWord
            putStrLn "Type 'help' for available commands"

showHelp :: IO ()
showHelp = do
    putStrLn "=== Typus Debug CLI Help ==="
    putStrLn ""
    putStrLn "Basic Commands:"
    putStrLn "  help, h             - Show this help"
    putStrLn "  quit, q             - Exit debug CLI"
    putStrLn "  status, s           - Show debug status"
    putStrLn ""
    putStrLn "Breakpoint Commands:"
    putStrLn "  set breakpoint LOC   - Set breakpoint at location"
    putStrLn "  break LOC           - Set breakpoint at location"
    putStrLn "  b LOC               - Set breakpoint at location"
    putStrLn "  list, l             - List all breakpoints"
    putStrLn "  clear, c            - Clear all breakpoints"
    putStrLn ""
    putStrLn "Debug Control:"
    putStrLn "  toggle, t           - Toggle debug output"
    putStrLn "  enable              - Enable debugging"
    putStrLn "  disable             - Disable debugging"
    putStrLn "  level N             - Set debug level (0-4)"
    putStrLn ""
    putStrLn "Log Commands:"
    putStrLn "  log LOC MSG         - Log message at location"
    putStrLn "  info LOC            - Log info at location"
    putStrLn "  warn LOC MSG        - Log warning at location"
    putStrLn "  error LOC MSG       - Log error at location"
    putStrLn "  trace LOC MSG       - Log trace at location"
    putStrLn ""
    putStrLn "Test Commands:"
    putStrLn "  test LOC            - Test breakpoint at location"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  set breakpoint main  - Set breakpoint at main"
    putStrLn "  break parse         - Set breakpoint at parse"
    putStrLn "  log compile \"Starting compilation\""
    putStrLn "  test main           - Test breakpoint at main"
    putStrLn "  level 4             - Set debug level to 4"

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads