module Test.Integration.PipelineSpec (tests) where

import Data.List (isInfixOf)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, (@?=), testCase)

import qualified Compiler
import qualified Ownership
import qualified Parser
import SourceLocation (locatedValue)

fixturePath :: FilePath
fixturePath = "test" </> "data"

loadFixture :: FilePath -> IO String
loadFixture name = readFile (fixturePath </> name)

parseFixture :: FilePath -> IO Parser.TypusFile
parseFixture name = do
  contents <- loadFixture name
  case Parser.parseTypus contents of
    Left err -> assertFailure ("parseTypus failed: " <> err)
    Right parsed -> pure parsed

assertGoBuilds :: String -> IO ()
assertGoBuilds goCode = do
  mGo <- resolveGoCommand
  case mGo of
    Nothing ->
      assertFailure "Go toolchain is missing. Install Go or set TYPUS_FAKE_GO to a validation script."
    Just go -> withSystemTempDirectory "typus-integration" $ \tmp -> do
      let goFile = tmp </> "generated.go"
          exeFile = tmp </> "generated"
      writeFile goFile goCode
      (exitCode, _stdout, stderr) <- readProcessWithExitCode go ["build", "-o", exeFile, goFile] ""
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code ->
          assertFailure ("go build failed with exit code " <> show code <> ": " <> stderr)
  where
    resolveGoCommand :: IO (Maybe FilePath)
    resolveGoCommand = do
      realGo <- findExecutable "go"
      case realGo of
        Just path -> pure (Just path)
        Nothing -> do
          envOverride <- lookupEnv "TYPUS_FAKE_GO"
          case envOverride of
            Just candidate -> pure (Just candidate)
            Nothing -> do
              let bundled = "scripts" </> "fake-go.sh"
              exists <- doesFileExist bundled
              pure (if exists then Just bundled else Nothing)

tests :: TestTree
tests =
  testGroup "Integration"
    [ testCase "compiles sample program and optionally builds with Go" $ do
        typusFile <- parseFixture "simple_go_code.typus"
        goCode <- case Compiler.compile typusFile of
          Left err -> assertFailure ("compile failed: " <> Compiler.renderCompilationError err)
          Right goSrc -> pure goSrc
        assertBool "generated Go code should mention package main" ("package main" `isInfixOf` goCode)
        assertGoBuilds goCode

    , testCase "ownership analysis flags complex violations" $ do
        source <- loadFixture "complex_ownership_code.typus"
        let errors = Ownership.analyzeOwnership source
        assertBool "expected to find at least one ownership violation" (not (null errors))

    , testCase "dependent type directives propagate from fixture" $ do
        typusFile <- parseFixture "code_with_dependent_types.typus"
        let Parser.FileDirectives { Parser.fdDependentTypes = dtFlag } = Parser.tfDirectives typusFile
            dtEnabled = fmap locatedValue dtFlag
        dtEnabled @?= Just True
    ]
