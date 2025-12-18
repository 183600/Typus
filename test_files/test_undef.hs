#!/usr/bin/env runhaskell

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser (parseTypus, TypusFile(..))
import Compiler (compile, renderCompilationError)

main :: IO ()
main = do
    let source = "package main\nfunc main() {\n    println(undefinedVar)\n}"
    let typusFile = TypusFile (T.pack source) "test.typus"
    case parseTypus source of
        Left err -> putStrLn $ "Parse error: " ++ err
        Right parsed -> 
            case compile parsed of
                Left errs -> do
                    putStrLn "Compilation errors:"
                    putStrLn (renderCompilationError errs)
                Right goCode -> do
                    putStrLn "Compilation succeeded:"
                    putStrLn goCode
