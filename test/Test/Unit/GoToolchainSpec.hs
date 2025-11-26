module Test.Unit.GoToolchainSpec (tests) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, (@?=), testCase)

import GoToolchain
  ( createTempGoFile
  , goModContents
  , isEnvVarEnabled
  , shouldSkipGoToolchain
  , withTemporaryGoProject
  , writeGoModule
  )

tests :: TestTree
tests =
  testGroup "GoToolchain"
    [ testCase "isEnvVarEnabled recognizes truthy values" $ do
        let envName = "TYPUS_TEST_ENV"
        forM_ ["1", "true", "YES", "On"] $ \val ->
          withEnvVar envName (Just val) $ do
            enabled <- isEnvVarEnabled envName
            assertBool ("expected " ++ val ++ " to be treated as enabled") enabled

    , testCase "isEnvVarEnabled returns False when variable is unset" $ do
        withEnvVar "TYPUS_TEST_ENV" Nothing $ do
          enabled <- isEnvVarEnabled "TYPUS_TEST_ENV"
          enabled @?= False

    , testCase "shouldSkipGoToolchain respects the skip environment flag" $ do
        withEnvVar "TYPUS_SKIP_GO_BUILD" (Just "true") $ do
          result <- shouldSkipGoToolchain
          result @?= True
        withEnvVar "TYPUS_SKIP_GO_BUILD" Nothing $ do
          result <- shouldSkipGoToolchain
          result @?= False

    , testCase "writeGoModule creates go.mod with expected contents" $
        withSystemTempDirectory "typus-go-mod" $ \tmpDir -> do
          result <- runExceptT (writeGoModule tmpDir)
          result @?= Right ()
          contents <- readFile (tmpDir </> "go.mod")
          contents @?= goModContents

    , testCase "withTemporaryGoProject initializes go.mod before running action" $ do
        result <- runExceptT $
          withTemporaryGoProject "toolchain-spec" $ \dir -> do
            let goModPath = dir </> "go.mod"
            liftIO $ do
              exists <- doesFileExist goModPath
              assertBool "expected go.mod to exist" exists
            contents <- liftIO $ readFile goModPath
            liftIO $ contents @?= goModContents
            pure "ok"
        result @?= Right "ok"

    , testCase "createTempGoFile derives file name from source path" $
        withSystemTempDirectory "typus-go-file" $ \tmpDir -> do
          result <- runExceptT $ do
            tempPath <- createTempGoFile "/app/src/example-source.typus" tmpDir
            liftIO $ do
              exists <- doesFileExist tempPath
              assertBool "expected generated Go file to exist" exists
              takeExtension tempPath @?= ".go"
              let fileName = takeFileName tempPath
              assertBool "expected generated file name to use source prefix"
                ("example-source" `isPrefixOf` fileName)
          result @?= Right ()
    ]

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name newValue action =
  bracket
    (do
        original <- lookupEnv name
        setVar newValue
        pure original)
    setVar
    (const action)
  where
    setVar (Just value) = setEnv name value
    setVar Nothing = unsetEnv name
