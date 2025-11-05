#!/usr/bin/env stack
{- stack script
   --resolver lts-22.0
   --package process
   --package directory
   --package filepath
-}

-- Command line debugging tool for Typus compiler
-- Usage: ./debug-cmd.hs [command] [args]

import System.Environment (getArgs)
import System.Process (readProcessWithExitCode, system)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeFileName, takeExtension)
import System.IO (hPutStrLn, stderr)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (toLower)
import Control.Exception (try, IOException)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> printHelp
        (cmd:rest) -> handleCommand cmd rest

printHelp :: IO ()
printHelp = do
    putStrLn "Typus Debug Tool"
    putStrLn "================="
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  build        - Build the project with debug flags"
    putStrLn "  test         - Run tests with debug output"
    putStrLn "  break        - Set breakpoint in file at line"
    putStrLn "  log          - Add debug logging to file"
    putStrLn "  trace        - Enable execution tracing"
    putStrLn "  profile      - Run performance profiling"
    putStrLn "  clean        - Clean build artifacts"
    putStrLn "  help         - Show this help message"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  ./debug-cmd.hs build"
    putStrLn "  ./debug-cmd.hs break src/Parser.hs 42"
    putStrLn "  ./debug-cmd.hs log src/Compiler.hs"
    putStrLn "  ./debug-cmd.hs trace"

handleCommand :: String -> [String] -> IO ()
handleCommand cmd args = case map toLower cmd of
    "build"   -> buildProject
    "test"    -> runTests
    "break"   -> setBreakpoint args
    "log"     -> addLogging args
    "trace"   -> enableTracing
    "profile" -> runProfiling
    "clean"   -> cleanProject
    "help"    -> printHelp
    _          -> putStrLn $ "Unknown command: " ++ cmd

buildProject :: IO ()
buildProject = do
    putStrLn "Building project with debug flags..."
    let cmd = "stack build --ghc-options='-g -Wall -Werror' --haddock --no-haddock-deps"
    (code, out, err) <- readProcessWithExitCode "bash" ["-c", cmd] ""
    if code == ExitSuccess
        then do
            putStrLn "Build successful!"
            putStrLn $ "Output: " ++ take 200 out
        else do
            hPutStrLn stderr "Build failed!"
            hPutStrLn stderr $ "Error: " ++ err

runTests :: IO ()
runTests = do
    putStrLn "Running tests with debug output..."
    let cmd = "stack test --test-arguments='--test-option=--color=always --test-option=--verbose'"
    (code, out, err) <- readProcessWithExitCode "bash" ["-c", cmd] ""
    if code == ExitSuccess
        then do
            putStrLn "All tests passed!"
            putStrLn $ "Output: " ++ take 500 out
        else do
            hPutStrLn stderr "Tests failed!"
            hPutStrLn stderr $ "Error: " ++ err

setBreakpoint :: [String] -> IO ()
setBreakpoint [] = putStrLn "Usage: break <file> <line>"
setBreakpoint [file] = putStrLn "Usage: break <file> <line>"
setBreakpoint (file:lineStr:_) = do
    case reads lineStr of
        [(line, "")] -> do
            putStrLn $ printf "Setting breakpoint in %s at line %d" file line
            result <- tryAddBreakpoint file line
            case result of
                Left e -> hPutStrLn stderr $ printf "Failed to set breakpoint: %s" (show (e :: IOException))
                Right () -> putStrLn "Breakpoint added successfully"
        _ -> putStrLn $ "Invalid line number: " ++ lineStr

addLogging :: [String] -> IO ()
addLogging [] = putStrLn "Usage: log <file>"
addLogging (file:_) = do
    exists <- doesFileExist file
    if not exists
        then hPutStrLn stderr $ "File not found: " ++ file
        else do
            putStrLn $ printf "Adding debug logging to %s" file
            result <- tryAddLogging file
            case result of
                Left e -> hPutStrLn stderr $ printf "Failed to add logging: %s" (show (e :: IOException))
                Right () -> putStrLn "Debug logging added successfully"

enableTracing :: IO ()
enableTracing = do
    putStrLn "Enabling execution tracing..."
    putStrLn "Use this function to add trace statements to your code"
    putStrLn "Example calls:"
    putStrLn "  Debug.debugTrace \"Parser.hs:42\" \"Starting parse\""
    putStrLn "  Debug.debugBreakpoint \"Compiler.hs:123\" \"Compilation complete\""

runProfiling :: IO ()
runProfiling = do
    putStrLn "Running performance profiling..."
    let cmd = "stack build --profile"
    (code, _, err) <- readProcessWithExitCode "bash" ["-c", cmd] ""
    if code == ExitSuccess
        then do
            putStrLn "Profile build successful!"
            putStrLn "Run 'stack exec -- typus -- +RTS -p' to generate profile"
        else do
            hPutStrLn stderr "Profile build failed!"
            hPutStrLn stderr $ "Error: " ++ err

cleanProject :: IO ()
cleanProject = do
    putStrLn "Cleaning build artifacts..."
    let cmd = "stack clean"
    (code, _, err) <- readProcessWithExitCode "bash" ["-c", cmd] ""
    if code == ExitSuccess
        then putStrLn "Clean completed successfully!"
        else hPutStrLn stderr $ "Clean failed: " ++ err

-- Helper functions
tryAddBreakpoint :: FilePath -> Int -> IO (Either IOException ())
tryAddBreakpoint file line = try $ do
    content <- readFile file
    let lines = lines content
    if length lines >= line
        then do
            let (before, after) = splitAt (line - 1) lines
            let currentLine = after !! 0
            let breakpointLine = currentLine ++ " -- DEBUG BREAKPOINT"
            let newContent = unlines $ before ++ [breakpointLine] ++ drop 1 after
            writeFile file newContent
            putStrLn $ printf "Added breakpoint at line %d" line
        else hPutStrLn stderr $ printf "Line %d does not exist in file" line

tryAddLogging :: FilePath -> IO (Either IOException ())
tryAddLogging file = try $ do
    content <- readFile file
    let lines = lines content
    let modifiedLines = addDebugLogStatements lines
    writeFile file (unlines modifiedLines)
    putStrLn $ printf "Added debug logging to %s" file

addDebugLogStatements :: [String] -> [String]
addDebugLogStatements [] = []
addDebugLogStatements (line:rest) =
    let newLine = if shouldAddLog line
                   then line ++ " -- DEBUG LOG: " ++ getLogMessage line
                   else line
    in newLine : addDebugLogStatements rest

shouldAddLog :: String -> Bool
shouldAddLog line = any (`isPrefixOf` (map toLower line))
    [ "let ", "case ", "if ", "do ", "where ", "function ", "def " ]

getLogMessage :: String -> String
getLogMessage line =
    case words line of
        (firstWord:_) -> printf "Entering %s" firstWord
        [] -> "Entering function"

-- Utility functions
take :: Int -> String -> String
take n s = if length s > n then take n s ++ "..." else s

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)