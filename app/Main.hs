module Main (main) where

import Cli (parseArgs)
import Cli.Runner (runCli)
import System.Exit (exitFailure)
import Tooling.Error (renderToolingError)

main :: IO ()
main = do
    cliArgs <- parseArgs
    result <- runCli cliArgs
    case result of
        Left err -> putStrLn ("Error: " ++ renderToolingError err) >> exitFailure
        Right _  -> pure ()
