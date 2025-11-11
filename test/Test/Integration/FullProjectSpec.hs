module Test.Integration.FullProjectSpec (tests) where

import CompilerUtils
    ( batchCheck
    , batchConvert
    , convertFile
    , newCompilerContextWithExecutor
    , silentLogger
    )
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, sort)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    )
import System.FilePath (replaceExtension, takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

import GoToolchain (GoExecutor(..))
import Tooling.Error (ToolingError, renderToolingError)

fixtureRoot :: FilePath
fixtureRoot = "test" </> "fixtures" </> "full_project"

-- | End-to-end integration tests that exercise the real compiler utilities
-- against the comprehensive fixture project. These tests are wired into the
-- main Tasty suite so `stack test` covers unit, integration, and full workflows.
tests :: TestTree
tests =
  testGroup "Full project workflow"
    [ testCase "batchConvert mirrors the project structure" testBatchConvertFullProject
    , testCase "batchCheck triggers Go validation" testBatchCheckInvokesGo
    , testCase "batchCheck honours the skip flag" testBatchCheckSkipFlag
    , testCase "convertFile passes Go sources through unchanged" testConvertGoPassthrough
    ]

testBatchConvertFullProject :: IO ()
testBatchConvertFullProject = withSystemTempDirectory "typus-full-project" $ \tmpDir -> do
  ensureFixtureExists
  (executor, _runsRef) <- recordingExecutor
  ctx <- newCompilerContextWithExecutor silentLogger executor
  expectSuccess =<< runExceptT (batchConvert ctx fixtureRoot tmpDir)

  typusRelPaths <- collectRelativePathsWithExtension fixtureRoot ".typus"
  goRelPaths <- collectRelativePathsWithExtension tmpDir ".go"

  let expectedGo = sort (map (`replaceExtension` "go") typusRelPaths)
  sort goRelPaths @?= expectedGo

  forM_ expectedGo $ \relPath -> do
    let goPath = tmpDir </> relPath
    exists <- doesFileExist goPath
    assertBool ("Converted file missing: " ++ relPath) exists
    when exists $ do
      goContents <- readFile goPath
      assertBool "Generated Go source should have a package declaration" ("package main" `isInfixOf` goContents)

testBatchCheckInvokesGo :: IO ()
testBatchCheckInvokesGo = do
  ensureFixtureExists
  (executor, runsRef) <- recordingExecutor
  ctx <- newCompilerContextWithExecutor silentLogger executor

  expectSuccess =<< runExceptT (batchCheck ctx fixtureRoot)

  invocations <- readIORef runsRef
  assertBool "Expected go build to be invoked" (not (null invocations))
  assertBool "Expected go build command to be issued"
    (any ("build" `elem`) (map fst invocations))

testBatchCheckSkipFlag :: IO ()
testBatchCheckSkipFlag = do
  ensureFixtureExists
  (executor, runsRef) <- skippingExecutor
  ctx <- newCompilerContextWithExecutor silentLogger executor

  expectSuccess =<< runExceptT (batchCheck ctx fixtureRoot)

  invocations <- readIORef runsRef
  invocations @?= []

testConvertGoPassthrough :: IO ()
testConvertGoPassthrough = withSystemTempDirectory "typus-go-passthrough" $ \tmpDir -> do
  let inputPath = tmpDir </> "input.go"
      outputPath = tmpDir </> "output.go"
      goSource = unlines
        [ "package main"
        , "func main() {"
        , "    println(\"hello\")"
        , "}"
        ]
  writeFile inputPath goSource

  (executor, _runsRef) <- recordingExecutor
  ctx <- newCompilerContextWithExecutor silentLogger executor
  expectSuccess =<< runExceptT (convertFile ctx inputPath outputPath)

  outputExists <- doesFileExist outputPath
  assertBool "Expected converted Go file to exist" outputExists
  when outputExists $ do
    content <- readFile outputPath
    content @?= goSource

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

expectSuccess :: Either ToolingError a -> IO a
expectSuccess result =
  case result of
    Left err -> assertFailure ("Tooling step failed: " ++ renderToolingError err)
    Right value -> pure value

ensureFixtureExists :: IO ()
ensureFixtureExists = do
  exists <- doesDirectoryExist fixtureRoot
  assertBool ("Full project fixture missing: " ++ fixtureRoot) exists

collectRelativePathsWithExtension :: FilePath -> String -> IO [FilePath]
collectRelativePathsWithExtension root ext = sort <$> go ""
  where
    go prefix = do
      let dir = if null prefix then root else root </> prefix
      entries <- listDirectory dir
      fmap concat . forM entries $ \entry -> do
        let rel = if null prefix then entry else prefix </> entry
            path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then go rel
          else pure [rel | takeExtension rel == ext]

recordingExecutor :: IO (GoExecutor, IORef [([String], FilePath)])
recordingExecutor = do
  ref <- newIORef []
  let exec = GoExecutor
        { goShouldSkip = pure False
        , goRunCommandInDir = \args dir -> ExceptT $ do
            modifyIORef' ref ((args, dir) :)
            pure (Right ())
        }
  pure (exec, ref)

skippingExecutor :: IO (GoExecutor, IORef [([String], FilePath)])
skippingExecutor = do
  ref <- newIORef []
  let exec = GoExecutor
        { goShouldSkip = pure True
        , goRunCommandInDir = \args dir -> ExceptT $ do
            modifyIORef' ref ((args, dir) :)
            pure (Right ())
        }
  pure (exec, ref)
