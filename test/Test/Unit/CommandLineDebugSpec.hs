module Test.Unit.CommandLineDebugSpec (tests) where

import CommandLineDebug
  ( CommandLineDebugConfig(..)
  , DebugCommandResult(..)
  , clearBreakpoints
  , defaultCLIDebugConfig
  , processDebugCommand
  , runWithCLIDebug
  , setBreakpoint
  , setDebugLevel
  , toggleDebugOutput
  )
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

    , testCase "setBreakpoint registers unique breakpoints" $ do
        config <- defaultCLIDebugConfig
        setBreakpoint config "main"
        setBreakpoint config "main"
        setBreakpoint config "helper"
        breakpoints <- readIORef (cldBreakpoints config)
        breakpoints @?= Set.fromList ["helper", "main"]

    , testCase "clearBreakpoints removes all stored breakpoints" $ do
        config <- defaultCLIDebugConfig
        setBreakpoint config "orphaned"
        clearBreakpoints config
        breakpoints <- readIORef (cldBreakpoints config)
        breakpoints @?= Set.empty

    , testCase "toggleDebugOutput flips enabled flag" $ do
        config <- defaultCLIDebugConfig
        initial <- readIORef (cldEnabled config)
        initial @?= True
        toggleDebugOutput config
        afterFirst <- readIORef (cldEnabled config)
        afterFirst @?= False
        toggleDebugOutput config
        afterSecond <- readIORef (cldEnabled config)
        afterSecond @?= True

    , testCase "setDebugLevel updates the log level reference" $ do
        config <- defaultCLIDebugConfig
        setDebugLevel config 5
        level <- readIORef (cldLogLevel config)
        level @?= 5

    , testCase "processDebugCommand toggles enable state through commands" $ do
        config <- defaultCLIDebugConfig
        disableResult <- processDebugCommand config "unit-test-location" ["disable"]
        disableResult @?= AwaitMoreInput
        disabled <- readIORef (cldEnabled config)
        disabled @?= False
        enableResult <- processDebugCommand config "unit-test-location" ["enable"]
        enableResult @?= AwaitMoreInput
        reEnabled <- readIORef (cldEnabled config)
        reEnabled @?= True
    ]
