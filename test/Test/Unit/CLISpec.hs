module Test.Unit.CLISpec (tests) where

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

    , testCase "parses build strict embed flag" $ do
        result <- withArgs ["build", "--strict-embed"] parseArgs
        result @?= Build True []

    , testCase "parses run defaults to non-strict" $ do
        result <- withArgs ["run", "example.typus"] parseArgs
        result @?= Run False ["example.typus"]

    , testCase "parses run with strict embed flag" $ do
        result <- withArgs ["run", "--strict-embed", "example.typus"] parseArgs
        result @?= Run True ["example.typus"]
    ]
