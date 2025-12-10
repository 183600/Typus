#!/usr/bin/env runhaskell

import System.IO
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Compiler.Parser (parseTypus)
import Compiler.IR as IR
import Compiler.TypeChecker as TypeChecker
import Compiler.Compiler (compileWithPackage)

main :: IO ()
main = do
    -- Read the ownership.typus file
    content <- readFile "test/fixtures/full_project/ownership.typus"
    putStrLn "=== ownership.typus content ==="
    putStrLn content
    putStrLn ""
    
    -- Parse the file
    case parseTypus content of
        Left err -> do
            putStrLn "Parse error:"
            print err
        Right parsed -> do
            putStrLn "=== Parse successful ==="
            
            -- Try to compile with package
            packageFiles <- do
                files <- listDirectory "test/fixtures/full_project"
                let typusFiles = filter (".typus" `isSuffixOf`) files
                forM typusFiles $ \f -> do
                    fc <- readFile $ "test/fixtures/full_project/" ++ f
                    case parseTypus fc of
                        Left err -> error $ "Parse error in " ++ f ++ ": " ++ show err
                        Right p -> return ("test/fixtures/full_project/" ++ f, p)
            
            putStrLn $ "Package files: " ++ show (map fst packageFiles)
            
            case compileWithPackage parsed packageFiles of
                Left errs -> do
                    putStrLn "Compilation errors:"
                    mapM_ print errs
                Right goCode -> do
                    putStrLn "=== Generated Go code ==="
                    putStrLn goCode
                    putStrLn ""
                    
                    -- Try to diagnose type errors
                    case TypeChecker.diagnoseTypeErrorsWithPackage parsed packageFiles of
                        Left errs -> do
                            putStrLn "Type errors:"
                            mapM_ print errs
                        Right diagnostics -> do
                            putStrLn "Type diagnostics:"
                            mapM_ print diagnostics