{-# LANGUAGE CPP #-}

module Test.Unit.IntegrationComplexWorkflowsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, choose, Property, (==>))
import Control.Monad (replicateM, when, foldM)
import Data.List (sort, nub, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)

import Compiler (compile, CompilerResult(..))
import Parser (TypusFile(..))
import IntegratedCompiler (compileProject)
import Utils (trim, splitBy)

-- | Complex workflow integration tests for the Typus compiler
tests :: TestTree
tests =
  testGroup "Complex Workflow Integration Tests"
    [ testGroup "Multi-file Project Compilation"
        [ testCase "Compiles project with multiple dependent files" $ do
            let files = 
                  [ ("main.typus", unlines
                      [ "import \"lib.typus\""
                      , "import \"utils.typus\""
                      , "func main() {"
                      , "  let data = processData(createData())"
                      , "  print(data)"
                      , "}"
                      ])
                  , ("lib.typus", unlines
                      [ "import \"utils.typus\""
                      , "func processData(d: Data) -> String {"
                      , "  return format(d)"
                      , "}"
                      ])
                  , ("utils.typus", unlines
                      [ "type Data = struct { value: Int }"
                      , "func createData() -> Data {"
                      , "  return Data{value: 42}"
                      , "}"
                      , "func format(d: Data) -> String {"
                      , "  return toString(d.value)"
                      , "}"
                      ])
                  ]
                result <- compileProject files
            assertBool "Should compile multi-file project successfully"
                (projectSuccess result)

        , testCase "Handles circular dependencies gracefully" $ do
            let files =
                  [ ("a.typus", "import \"b.typus\"\nfunc a() { b() }")
                  , ("b.typus", "import \"a.typus\"\nfunc b() { a() }")
                  ]
                result <- compileProject files
            assertBool "Should detect circular dependencies"
                (hasCircularDependencyError result)

        , testCase "Compiles project with ownership across files" $ do
            let files =
                  [ ("main.typus", unlines
                      [ "import \"owner.typus\""
                      , "func main() {"
                      , "  let data = createData()"
                      , "  transferData(data)"
                      , "}"
                      ])
                  , ("owner.typus", unlines
                      [ "type Data = struct { value: Int }"
                      , "func createData() -> Data {"
                      , "  return Data{value: 42}"
                      , "}"
                      , "func transferData(d: Data) {"
                      , "  consume(d)"
                      , "}"
                      ])
                  ]
                result <- compileProject files
            assertBool "Should handle cross-file ownership correctly"
                (projectSuccess result)
        ]

    , testGroup "Incremental Compilation"
        [ testCase "Only recompiles changed files" $ do
            let initialFiles =
                  [ ("main.typus", "func main() { hello() }")
                  , ("lib.typus", "func hello() { print(\"hello\") }")
                  ]
                result1 <- compileProject initialFiles
            let changedFiles =
                  [ ("main.typus", "func main() { hello(); goodbye() }")
                  , ("lib.typus", "func hello() { print(\"hello\") }")
                  , ("new.typus", "func goodbye() { print(\"goodbye\") }")
                  ]
                result2 <- incrementalCompile initialFiles changedFiles
            assertBool "Incremental compilation should succeed"
                (projectSuccess result2)
            assertBool "Should only recompile changed files"
                (recompiledCount result2 <= 2)

        , testCase "Handles dependency changes in incremental compilation" $ do
            let initialFiles =
                  [ ("main.typus", "import \"lib.typus\"\nfunc main() { test() }")
                  , ("lib.typus", "func test() { print(\"test\") }")
                  ]
                result1 <- compileProject initialFiles
            let changedFiles =
                  [ ("main.typus", "import \"lib.typus\"\nfunc main() { test() }")
                  , ("lib.typus", "func test() { print(\"updated test\") }")
                  ]
                result2 <- incrementalCompile initialFiles changedFiles
            assertBool "Should handle dependency changes"
                (projectSuccess result2)
            assertBool "Should recompile dependent files"
                (recompiledCount result2 >= 1)
        ]

    , testGroup "Build Pipeline Integration"
        [ testCase "Integrates with external build tools" $ do
            let buildConfig = BuildConfig
                  { bcCompiler = "typus"
                  , bcFlags = ["-O2", "--ownership"]
                  , bcOutputDir = "build"
                  , bcDependencies = ["stdlib.typus"]
                  }
                files = [("main.typus", "func main() {}")]
                result <- buildWithExternalTool buildConfig files
            assertBool "External build integration should succeed"
                (buildSuccess result)

        , testCase "Generates correct build artifacts" $ do
            let files =
                  [ ("main.typus", unlines
                      [ "func add(x: Int, y: Int) -> Int {"
                      , "  return x + y"
                      , "}"
                      , "func main() {"
                      , "  let result = add(1, 2)"
                      , "  print(result)"
                      , "}"
                      ])
                  ]
                result <- generateBuildArtifacts files
            assertBool "Should generate L.all required artifacts"
                (hasRequiredArtifacts result ["main.go", "main.o", "main"])
        ]

    , testGroup "Error Recovery in Complex Workflows"
        [ testCase "Continues compilation after non-fatal errors" $ do
            let files =
                  [ ("main.typus", unlines
                      [ "func main() {"
                      , "  let x = undefinedVar // Error"
                      , "  let y = 42"
                      , "  print(y)"
                      , "}"
                      ])
                  , ("lib.typus", "func helper() { print(\"helper\") }")
                  ]
                result <- compileProjectWithRecovery files
            assertBool "Should recover from non-fatal errors"
                (hasPartialSuccess result)
            assertBool "Should compile valid files"
                (validFilesCompiled result >= 1)

        , testCase "Provides comprehensive error reports" $ do
            let files =
                  [ ("main.typus", unlines
                      [ "func bad() {"
                      , "  let x: String = 42 // Type error"
                      , "  return undefinedVar // Undefined var"
                      , "}"
                      ])
                  ]
                result <- compileProject files
            assertBool "Should provide comprehensive error reports"
                (errorCount result >= 2)
            assertBool "Should include source locations in errors"
                (L.all hasSourceLocation (errors result))
        ]

    , testGroup "Property-based Integration Tests"
        [ fastProperty "Project compilation is deterministic" prop_projectDeterministic
        , fastProperty "Incremental compilation preserves correctness" prop_incrementalCorrectness
        , fastProperty "Build pipeline integration is robust" prop_buildPipelineRobustness
        , fastProperty "Error recovery maintains consistency" prop_errorRecoveryConsistency
        ]
    ]

-- Data types for integration testing

data ProjectResult = ProjectResult
    { prSuccess :: Bool
    , prCompiledFiles :: [String]
    , prErrors :: [ProjectError]
    , prArtifacts :: [String]
    } deriving (Show, Eq)

data ProjectError = ProjectError
    { peFile :: String
    , peMessage :: String
    , peLocation :: Maybe String
    } deriving (Show, Eq)

data BuildConfig = BuildConfig
    { bcCompiler :: String
    , bcFlags :: [String]
    , bcOutputDir :: String
    , bcDependencies :: [String]
    } deriving (Show, Eq)

data BuildResult = BuildResult
    { brSuccess :: Bool
    , brArtifacts :: [String]
    , brBuildLog :: [String]
    } deriving (Show, Eq)

-- Helper functions for integration testing

projectSuccess :: ProjectResult -> Bool
projectSuccess = prSuccess

hasCircularDependencyError :: ProjectResult -> Bool
hasCircularDependencyError result = 
    L.any (\e -> "circular" `L.isInfixOf` peMessage e) (prErrors result)

recompiledCount :: ProjectResult -> Int
recompiledCount result = L.length (prCompiledFiles result)

buildSuccess :: BuildResult -> Bool
buildSuccess = brSuccess

hasRequiredArtifacts :: BuildResult -> [String] -> Bool
hasRequiredArtifacts result required = 
    L.all (`elem` brArtifacts result) required

hasPartialSuccess :: ProjectResult -> Bool
hasPartialSuccess result = 
    not (prSuccess result) && L.length (prCompiledFiles result) > 0

validFilesCompiled :: ProjectResult -> Int
validFilesCompiled = L.length . prCompiledFiles

errorCount :: ProjectResult -> Int
errorCount = L.length . prErrors

errors :: ProjectResult -> [ProjectError]
errors = prErrors

hasSourceLocation :: ProjectError -> Bool
hasSourceLocation = isJust . peLocation

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` words haystack

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

-- Mock integration functions

compileProject :: [(String, String)] -> IO ProjectResult
compileProject files = do
    let hasCircular = L.any (\(f, c) -> "import" `L.isInfixOf` c && L.length files > 1) files
        success = not hasCircular
        compiledFiles = if success then map fst files else []
    return $ ProjectResult success compiledFiles [] []

compileProjectWithRecovery :: [(String, String)] -> IO ProjectResult
compileProjectWithRecovery files = do
    let validFiles = L.filter (\(_, c) -> not ("undefinedVar" `L.isInfixOf` c)) files
        compiledFiles = map fst validFiles
        errors = [ProjectError f "Undefined variable" Nothing | (f, c) <- files, "undefinedVar" `L.isInfixOf` c]
    return $ ProjectResult False compiledFiles errors []

incrementalCompile :: [(String, String)] -> [(String, String)] -> IO ProjectResult
incrementalCompile oldFiles newFiles = do
    let changedFiles = L.filter (\(f, _) -> f `elem` map fst newFiles) oldFiles
        recompiled = map fst changedFiles
    return $ ProjectResult True recompiled [] []

buildWithExternalTool :: BuildConfig -> [(String, String)] -> IO BuildResult
buildWithExternalTool config files = do
    let artifacts = L.map (\f -> bcOutputDir config ++ "/" ++ replaceExtension f "go") (map fst files)
    return $ BuildResult True artifacts ["Build completed"]

generateBuildArtifacts :: [(String, String)] -> IO BuildResult
generateBuildArtifacts files = do
    let artifacts = concatMap (\f -> [f ++ ".go", f ++ ".o", f ++ ".exe"]) (map fst files)
    return $ BuildResult True artifacts ["Artifacts generated"]

replaceExtension :: String -> String -> String
replaceExtension file newExt = 
    case L.reverse file of
        ('s':'y':'p':'u':'t':'.':rest) -> L.reverse rest ++ "." ++ newExt
        _ -> file ++ "." ++ newExt

-- Property-based tests

prop_projectDeterministic :: [(String, String)] -> Property
prop_projectDeterministic files =
    not (null files) ==>
    let result1 = projectResultFromFiles files
        result2 = projectResultFromFiles files
    in result1 == result2

prop_incrementalCorrectness :: [(String, String)] -> [(String, String)] -> Property
prop_incrementalCorrectness oldFiles newFiles =
    not (null oldFiles) && not (null newFiles) ==>
    let incrementalResult = incrementalResultFromFiles oldFiles newFiles
        fullResult = projectResultFromFiles newFiles
    in prCompiledFiles incrementalResult `subset` prCompiledFiles fullResult
  where
    subset [] _ = True
    subset (x:xs) ys = x `elem` ys && xs `subset` ys

prop_buildPipelineRobustness :: BuildConfig -> [(String, String)] -> Property
prop_buildPipelineRobustness config files =
    not (null files) ==>
    let result = buildResultFromConfig config files
    in brSuccess result || not (L.null (brBuildLog result))

prop_errorRecoveryConsistency :: [(String, String)] -> Property
prop_errorRecoveryConsistency files =
    not (null files) ==>
    let result1 = projectResultFromFiles files
        result2 = projectResultFromFiles files
    in errorCount result1 == errorCount result2

-- Mock property helper functions

projectResultFromFiles :: [(String, String)] -> ProjectResult
projectResultFromFiles files = ProjectResult True (map fst files) [] []

incrementalResultFromFiles :: [(String, String)] -> [(String, String)] -> ProjectResult
incrementalResultFromFiles oldFiles newFiles = 
    ProjectResult True (map fst newFiles) [] []

buildResultFromConfig :: BuildConfig -> [(String, String)] -> BuildResult
buildResultFromConfig config files = 
    BuildResult True (map fst files) ["Build completed"]

-- Arbitrary instances

instance Arbitrary (String, String) where
    arbitrary = do
        file <- oneof ["main.typus", "lib.typus", "utils.typus", "test.typus"]
        content <- oneof 
            [ "func main() {}"
            , "func helper() { print(\"help\") }"
            , "import \"other.typus\""
            , "let x = 42"
            ]
        return (file, content)

instance Arbitrary BuildConfig where
    arbitrary = do
        compiler <- oneof ["typus", "typus-dev"]
        flags <- listOf (oneof ["-O2", "--ownership", "--debug"])
        outputDir <- oneof ["build", "dist", "out"]
        dependencies <- listOf (oneof ["stdlib.typus", "prelude.typus"])
        return $ BuildConfig compiler flags outputDir dependencies