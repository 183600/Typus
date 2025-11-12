module Test.Integration.IntegratedCompilerSpec (tests) where

import Control.Exception (bracket)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
  ( (@?=)
  , assertBool
  , assertFailure
  , testCase
  )

import CompilerUtils (batchCheck, newCompilerContextWithExecutor, silentLogger)
import GoToolchain (GoExecutor(..), isEnvVarEnabled)
import IntegratedCompiler
  ( AnalysisResult(..)
  , CombinedError(..)
  , CompilerConfig(..)
  , ErrorSeverity(..)
  , IntegratedCompileResult(..)
  , analysisToCombined
  , compileWithIntegratedAnalyzers
  , defaultCompilerConfig
  )

import Tooling.Error (ToolingError)

-- | Integration scenarios for the integrated compiler entrypoint.
tests :: TestTree
tests =
  testGroup "Integrated Compiler"
    [ testCase "surfaces multi-analyzer failures and propagates diagnostics" $ do
        result <- compileWithIntegratedAnalyzers multiAnalyzerSource defaultCompilerConfig
        assertBool "expected compilation to fail" (not (success result))

        case analysisResult result of
          Nothing -> assertFailure "expected analysis result"
          Just analysis -> do
            assertBool "expected ownership errors" (not (null (ownershipErrors analysis)))
            assertBool "expected dependent type errors" (not (null (dependentTypeErrors analysis)))

            let combined = analysisToCombined analysis
            assertBool "expected cross-analysis error" (any hasCrossError combined)

            let warningMessage = "declared but never used"
            assertBool "expected analysis warning" (any (warningMessage `isInfixOf`) (analysisWarnings analysis))
            assertBool "warnings should propagate to compilationWarnings"
              (any (warningMessage `isInfixOf`) (compilationWarnings result))

            compilationInfo result @?= analysisInfo analysis

        let filtered = filteredErrors result
        assertBool "filtered errors should include dependent type diagnostics"
          (any hasDependentTypeError filtered)
        assertBool "filtered errors should include ownership diagnostics"
          (any hasOwnershipError filtered)
        assertBool "filtered errors should include cross-analysis diagnostics"
          (any hasCrossError filtered)

    , testCase "filters warning-only diagnostics based on errorReportingLevel" $ do
        let config = defaultCompilerConfig { errorReportingLevel = Fatal }
        result <- compileWithIntegratedAnalyzers warningOnlySource config
        assertBool "expected compilation to succeed" (success result)
        filteredErrors result @?= []

        let warningMessage = "declared but never used"
        assertBool "warnings should surface externally" (any (warningMessage `isInfixOf`) (compilationWarnings result))

        case analysisResult result of
          Nothing -> assertFailure "expected analysis result"
          Just analysis ->
            assertBool "analysis warnings should continue to be reported"
              (any (warningMessage `isInfixOf`) (analysisWarnings analysis))

        assertBool "compiled Go code should be emitted" (not (null (compiledCode result)))

    , testGroup "batchCheck honors TYPUS_SKIP_GO_BUILD"
        [ testCase "invokes Go toolchain when skip flag is absent" $
            withEnvVar "TYPUS_SKIP_GO_BUILD" Nothing $ do
              runCount <- newIORef 0
              let executor = GoExecutor
                    { goShouldSkip = isEnvVarEnabled "TYPUS_SKIP_GO_BUILD"
                    , goRunCommandInDir = recordingRunner runCount
                    }
              ctx <- newCompilerContextWithExecutor silentLogger executor
              withSampleTypusProject $ \dir -> do
                result <- runExceptT (batchCheck ctx dir)
                result @?= Right ()
                runInvocations <- readIORef runCount
                runInvocations @?= 1

        , testCase "skips Go toolchain when skip flag is enabled" $
            withEnvVar "TYPUS_SKIP_GO_BUILD" (Just "1") $ do
              runCount <- newIORef 0
              let executor = GoExecutor
                    { goShouldSkip = isEnvVarEnabled "TYPUS_SKIP_GO_BUILD"
                    , goRunCommandInDir = recordingRunner runCount
                    }
              ctx <- newCompilerContextWithExecutor silentLogger executor
              withSampleTypusProject $ \dir -> do
                result <- runExceptT (batchCheck ctx dir)
                result @?= Right ()
                runInvocations <- readIORef runCount
                runInvocations @?= 0
        ]
    ]

hasCrossError :: CombinedError -> Bool
hasCrossError (CrossAnalyzerError _ severity nested) =
  severity >= Error || any hasCrossError nested
hasCrossError _ = False

hasOwnershipError :: CombinedError -> Bool
hasOwnershipError (OwnershipErrorCombined _ _) = True
hasOwnershipError (CrossAnalyzerError _ _ nested) = any hasOwnershipError nested
hasOwnershipError _ = False

hasDependentTypeError :: CombinedError -> Bool
hasDependentTypeError (DependentTypeErrorCombined _ _) = True
hasDependentTypeError (CrossAnalyzerError _ _ nested) = any hasDependentTypeError nested
hasDependentTypeError _ = False

withSampleTypusProject :: (FilePath -> IO a) -> IO a
withSampleTypusProject action =
  withSystemTempDirectory "typus-batch-check" $ \dir -> do
    let typusFile = dir </> "sample.typus"
    writeFile typusFile batchCheckSource
    action dir

recordingRunner :: IORef Int -> [String] -> FilePath -> ExceptT ToolingError IO ()
recordingRunner counter _args _dir = ExceptT $ do
  modifyIORef' counter (+ 1)
  pure (Right ())

withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name mValue action =
  bracket (lookupEnv name) restoreOriginal $ \_ -> do
    case mValue of
      Nothing -> unsetIfSet
      Just value -> setEnv name value
    action
  where
    restoreOriginal original =
      case original of
        Nothing -> unsetIfSet
        Just value -> setEnv name value

    unsetIfSet = do
      current <- lookupEnv name
      case current of
        Nothing -> pure ()
        Just _ -> unsetEnv name

multiAnalyzerSource :: String
multiAnalyzerSource = unlines
  [ "//! ownership: on"
  , "//! dependent_types: on"
  , "package main"
  , ""
  , "{//! dependent_types: on}"
  , "type BadAlias = UnknownType"
  , "}"
  , ""
  , "func consume(x owned String) {"
  , "    println(x)"
  , "}"
  , ""
  , "func main() {"
  , "    var s owned String = \"hello\""
  , "    var unused owned String = \"spare\""
  , "    consume(s)"
  , "    println(s)"
  , "}"
  ]

warningOnlySource :: String
warningOnlySource = unlines
  [ "//! ownership: on"
  , "//! dependent_types: on"
  , "package main"
  , ""
  , "func main() {"
  , "    var unused owned String = \"hello\""
  , "    println(\"ok\")"
  , "}"
  ]

batchCheckSource :: String
batchCheckSource = unlines
  [ "package main"
  , ""
  , "func main() {"
  , "    println(\"hi\")"
  , "}"
  ]
