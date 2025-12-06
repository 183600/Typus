#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import qualified Parser
import qualified Compiler.DependentTypeChecker as DTC

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    case Parser.parseTypus contents of
        Left err -> putStrLn $ "Parse error: " <> err
        Right typusFile -> do
            let extracted = DTC.extractDependentTypeContent typusFile
            putStrLn "=== EXTRACTED DEPENDENT TYPE CONTENT ==="
            putStrLn extracted
            putStrLn "=== END ==="
