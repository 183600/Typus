module Test.Unit.CommandLineDebugSpec (tests) where

import CommandLineDebug
  ( CommandLineDebugConfig(..)
  , DebugCommandResult(..)
  , defaultCLIDebugConfig
  , processDebugCommand
  , runWithCLIDebug
  )
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup "CommandLineDebug"
    [ testCase "does not trigger breakpoint when condition is False" $ do
        config <- defaultCLIDebugConfig
        let location = "unit-test-location"
        writeIORef (cldBreakConditions config) (Map.singleton location (const False))

        executed <- newIORef False
        result <- timeout 1000000 (runWithCLIDebug config location (writeIORef executed True))
        result @?= Just ()

        wasExecuted <- readIORef executed
        wasExecuted @?= True
    , testCase "disable command keeps debugging disabled when already disabled" $ do
        config <- defaultCLIDebugConfig

        firstResult <- processDebugCommand config "unit-test-location" ["disable"]
        firstResult @?= AwaitMoreInput
        firstEnabled <- readIORef (cldEnabled config)
        firstEnabled @?= False

        secondResult <- processDebugCommand config "unit-test-location" ["disable"]
        secondResult @?= AwaitMoreInput
        secondEnabled <- readIORef (cldEnabled config)
        secondEnabled @?= False
    ]
