module Main (main) where

import qualified Cli.Runner as CliRunner
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main = do
  args <- getArgs
  exitCode <- CliRunner.runWithArgs args
  exitWith exitCode
