#!/usr/bin/env stack runhaskell

import System.Directory
import System.FilePath ((</>))
import CompilerUtils
import Control.Monad.Except (runExceptT)

main :: IO ()
main = do
    -- Read the ownership.typus file
    content <- readFile "test/fixtures/full_project/ownership.typus"
    putStrLn "=== ownership.typus content ==="
    putStrLn content
    putStrLn ""
    
    -- Create a compiler context
    ctx <- newCompilerContextWithExecutor silentLogger (GoExecutor {
        goRun = \_ _ _ -> return "",
        goBuild = \_ _ -> return "",
        goMod = \_ -> return "",
        goShouldSkip = return False
    })
    
    -- Try to check the file
    result <- runExceptT $ checkSingleFile ctx "test/fixtures/full_project/ownership.typus"
    case result of
        Left err -> do
            putStrLn "Error:"
            print err
        Right _ -> do
            putStrLn "Check successful"