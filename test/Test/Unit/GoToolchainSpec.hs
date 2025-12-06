{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.GoToolchainSpec (tests) where

import Control.Exception (bracket_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import GoToolchain
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeBaseName, takeExtension)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

withEnvOverride :: String -> Maybe String -> IO a -> IO a
withEnvOverride key newValue action = do
  original <- lookupEnv key
  let apply Nothing = unsetEnv key
      apply (Just val) = setEnv key val
  bracket_ (apply newValue) (apply original) action


tests :: TestTree
tests =
  testGroup "GoToolchain"
    [ testCase "isEnvVarEnabled recognises truthy values" $ do
        withEnvOverride "TYPUS_TEST_FLAG" (Just "YeS") $ do
          enabled <- isEnvVarEnabled "TYPUS_TEST_FLAG"
          enabled @?= True
        withEnvOverride "TYPUS_TEST_FLAG" (Just "0") $ do
          enabled <- isEnvVarEnabled "TYPUS_TEST_FLAG"
          enabled @?= False

    , testCase "shouldSkipGoToolchain mirrors environment state" $ do
        withEnvOverride "TYPUS_SKIP_GO_BUILD" (Just "0") $ do
          result <- shouldSkipGoToolchain
          result @?= False
        withEnvOverride "TYPUS_SKIP_GO_BUILD" (Just "1") $ do
          result <- shouldSkipGoToolchain
          result @?= True

    , testCase "writeGoModule creates a go.mod file with canned contents" $ do
        withSystemTempDirectory "gotoolchain" $ \tmp -> do
          result <- runExceptT (writeGoModule tmp)
          case result of
            Left err -> assertFailure ("writeGoModule failed: " ++ show err)
            Right _ -> do
              let goModPath = tmp </> "go.mod"
              exists <- doesFileExist goModPath
              assertBool "go.mod should exist" exists
              contents <- readFile goModPath
              contents @?= goModContents

    , testCase "createTempGoFile derives the prefix from the source file" $ do
        withSystemTempDirectory "gotoolchain" $ \tmp -> do
          let source = "/tmp/examples/server.typus"
          result <- runExceptT (createTempGoFile source tmp)
          case result of
            Left err -> assertFailure ("createTempGoFile failed: " ++ show err)
            Right path -> do
              exists <- doesFileExist path
              assertBool "temporary Go file should exist" exists
              takeExtension path @?= ".go"
              assertBool "derived file name should keep the source prefix" ("server-" `isPrefixOf` takeBaseName path)

    , testCase "withTemporaryGoProject writes go.mod and reuses the provided prefix" $ do
        let prefix = "cli-run"
        result <- runExceptT $ withTemporaryGoProject prefix $ \tmp -> do
          let goModPath = tmp </> "go.mod"
          liftIO $ do
            existsDir <- doesDirectoryExist tmp
            assertBool "temporary directory should exist" existsDir
            existsFile <- doesFileExist goModPath
            assertBool "go.mod should exist" existsFile
          pure (takeBaseName tmp)
        case result of
          Left err -> assertFailure ("withTemporaryGoProject failed: " ++ show err)
          Right dirBase -> assertBool "prefix should appear in directory name" (prefix `isPrefixOf` dirBase)

    , testCase "runGoCommand logs when Go is skipped" $ do
        withEnvOverride "TYPUS_SKIP_GO_BUILD" (Just "1") $ do
          ref <- newIORef []
          exec <- defaultGoExecutor (\msg -> modifyIORef' ref (msg :))
          runResult <- runExceptT (runGoCommand exec ["build", "./..."])
          case runResult of
            Left err -> assertFailure ("runGoCommand failed unexpectedly: " ++ show err)
            Right _ -> pure ()
          logs <- readIORef ref
          assertBool "expected skip message" (any (isPrefixOf "Skipping Go command") logs)
    ]
