module CLISpec (tests) where

import System.Environment (withArgs)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), testCase )

import Cli (Args(..), parseArgs)

tests :: TestTree
tests =
  testGroup "CLI argument parsing"
    [ testCase "parses convert command" $ do
        result <- withArgs ["convert", "input.typus", "-o", "output.go"] parseArgs
        result @?= Convert "input.typus" "output.go"

    , testCase "parses --version option" $ do
        result <- withArgs ["--version"] parseArgs
        result @?= Version
    ]
