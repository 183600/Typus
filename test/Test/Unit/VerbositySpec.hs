module Test.Unit.VerbositySpec (tests) where

import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import TestSupport.Verbosity
  ( Verbosity(..)
  , getVerbosity
  , whenVerbose
  )

tests :: TestTree
tests =
  testGroup "Verbosity helpers"
    [ testCase "defaults to quiet when env var is missing" $
        withEnvVar Nothing $ do
          verbosity <- getVerbosity
          verbosity @?= Quiet

    , testCase "parses truthy values case-insensitively" $
        withEnvVar (Just "  YeS  ") $ do
          verbosity <- getVerbosity
          verbosity @?= Verbose

    , testCase "treats other values as quiet" $
        withEnvVar (Just "maybe") $ do
          verbosity <- getVerbosity
          verbosity @?= Quiet

    , testCase "whenVerbose executes action only when verbose" $ do
        counter <- newIORef (0 :: Int)
        whenVerbose Verbose (increment counter)
        whenVerbose Quiet (increment counter)
        finalCount <- readIORef counter
        finalCount @?= 1
    ]
  where
    envVar :: String
    envVar = "TYPUS_TEST_VERBOSE"

    increment ref = modifyIORef' ref (+1)

    withEnvVar :: Maybe String -> IO a -> IO a
    withEnvVar desired action =
      bracket acquire restore (const action)
      where
        acquire = do
          original <- lookupEnv envVar
          case desired of
            Nothing ->
              case original of
                Nothing -> pure ()
                Just _  -> unsetEnv envVar
            Just value  -> setEnv envVar value
          pure original
        restore original =
          case original of
            Nothing     -> unsetEnv envVar
            Just value  -> setEnv envVar value
