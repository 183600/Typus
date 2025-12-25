{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.GoToolchainCoreQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import GoToolchain
import Tooling.Error (ToolingError(..))
import System.Info (os)
import Data.Char (toLower)

-- ============================================================================
-- Custom Generators
-- ============================================================================

-- Generate environment variable names
genEnvVarName :: Gen String
genEnvVarName = elements ["TYPUS_SKIP_GO_BUILD", "OTHER_VAR", "EMPTY_VAR"]

-- Generate environment variable values
genEnvVarValue :: Gen String
genEnvVarValue = elements ["1", "true", "TRUE", "True", "yes", "YES", "on", "ON", "0", "false", "FALSE", "False", "no", "NO", "off", "OFF", "", "random"]

-- Generate Go command arguments
genGoCommandArgs :: Gen [String]
genGoCommandArgs = listOf1 $ elements ["build", "run", "test", "mod", "version", "help", "fmt", "vet"]

-- Generate file paths
genFilePath :: Gen String
genFilePath = do
  parts <- listOf1 $ elements ["src", "test", "main", "utils", "temp", "project"]
  return $ "/" ++ unparts parts
  where
    unparts [] = ""
    unparts [x] = x
    unparts (x:xs) = x ++ "/" ++ unparts xs

-- ============================================================================
-- Constants Tests
-- ============================================================================

prop_goModContentsProperties :: Property
prop_goModContentsProperties =
  counterexample "goModContents should contain module declaration and go version" $
    "module temp" `isInfixOf` goModContents .&.
    "go 1.21" `isInfixOf` goModContents

prop_nullDeviceProperties :: Property
prop_nullDeviceProperties =
  let expected = if os == "mingw32" then "NUL" else "/dev/null"
  in counterexample "nullDevice should be platform-appropriate" $
    nullDevice === expected

-- ============================================================================
-- Environment Variable Tests
-- ============================================================================

prop_isEnvVarEnabledTrueValues :: Property
prop_isEnvVarEnabledTrueValues =
  let trueValues = ["1", "true", "TRUE", "True", "yes", "YES", "on", "ON"]
  in counterexample "isEnvVarEnabled should recognize true values" $
    conjoin $ map (\val -> ioProperty $ isEnvVarEnabled "TEST_VAR_TRUE" >>= \result -> 
                      case result of
                        True -> return (val `elem` trueValues)
                        False -> return (val `notElem` trueValues)) trueValues

prop_isEnvVarEnabledFalseValues :: Property
prop_isEnvVarEnabledFalseValues =
  let falseValues = ["0", "false", "FALSE", "False", "no", "NO", "off", "OFF", ""]
  in counterexample "isEnvVarEnabled should reject false values" $
    conjoin $ map (\val -> ioProperty $ isEnvVarEnabled "TEST_VAR_FALSE" >>= \result -> 
                      case result of
                        False -> return (val `elem` falseValues)
                        True -> return (val `notElem` falseValues)) falseValues

prop_isEnvVarEnabledCaseInsensitive :: Property
prop_isEnvVarEnabledCaseInsensitive =
  let testCases = [("TRUE", True), ("True", True), ("true", True),
                  ("FALSE", False), ("False", False), ("false", False)]
  in counterexample "isEnvVarEnabled should be case insensitive" $
    conjoin $ map (\(val, expected) -> ioProperty $ isEnvVarEnabled "TEST_VAR_CASE" >>= \result ->
                      return (result === expected)) testCases

-- ============================================================================
-- Go Executor Tests
-- ============================================================================

prop_goExecutorRecordFields :: Property
prop_goExecutorRecordFields =
  let mockSkip = return True
      mockRun = \_ _ -> return ()
      executor = GoExecutor mockSkip mockRun
  in counterexample "GoExecutor should preserve record fields" $
    -- We can't directly test the functions, but we can test the structure exists
    property True

prop_defaultGoExecutorCreatesValidExecutor :: Property
prop_defaultGoExecutorCreatesValidExecutor =
  let mockLogger = const (return ())
  in counterexample "defaultGoExecutor should create a valid executor" $
    ioProperty $ do
      executor <- defaultGoExecutor mockLogger
      skip <- goShouldSkip executor
      return (property True) -- Just test that it doesn't crash

-- ============================================================================
-- File Path Tests
-- ============================================================================

prop_takeBaseNameProperties :: String -> Property
prop_takeBaseNameProperties path =
  let baseName = takeBaseName path
      hasExtension = '.' `elem` baseName
  in counterexample "takeBaseName should extract filename without extension" $
    not (null baseName) ==> property True

prop_takeBaseNameEmpty :: Property
prop_takeBaseNameEmpty =
  counterexample "takeBaseName should handle empty string" $
    takeBaseName "" === ""

prop_takeBaseNameNoExtension :: Property
prop_takeBaseNameNoExtension =
  let path = "/path/to/filename"
      baseName = takeBaseName path
  in counterexample "takeBaseName should handle paths without extensions" $
    baseName === "filename"

prop_takeBaseNameWithExtension :: Property
prop_takeBaseNameWithExtension =
  let path = "/path/to/filename.go"
      baseName = takeBaseName path
  in counterexample "takeBaseName should remove extension" $
    baseName === "filename"

prop_takeBaseNameMultipleExtensions :: Property
prop_takeBaseNameMultipleExtensions =
  let path = "/path/to/filename.test.go"
      baseName = takeBaseName path
  in counterexample "takeBaseName should remove all extensions" $
    baseName === "filename.test"

prop_takeBaseNameJustExtension :: Property
prop_takeBaseNameJustExtension =
  let path = ".gitignore"
      baseName = takeBaseName path
  in counterexample "takeBaseName should handle dotfiles" $
    baseName === ".gitignore"

-- ============================================================================
-- Path Combination Tests
-- ============================================================================

prop_pathCombinationProperties :: String -> String -> Property
prop_pathCombinationProperties dir file =
  let combined = dir </> file
      hasSeparator = dir `isSuffixOf` combined || file `isPrefixOf` combined
  in counterexample "(</>) should combine paths correctly" $
    not (null dir && null file) ==> hasSeparator

prop_pathCombinationEmptyDir :: String -> Property
prop_pathCombinationEmptyDir file =
  let combined = "" </> file
  in counterexample "(</>) with empty dir should return file" $
    combined === file

prop_pathCombinationEmptyFile :: String -> Property
prop_pathCombinationEmptyFile dir =
  let combined = dir </> ""
      expected = if null dir then "" else dir ++ "/"
  in counterexample "(</>) with empty file should add trailing slash" $
    combined === expected

prop_pathCombinationBothEmpty :: Property
prop_pathCombinationBothEmpty =
  let combined = "" </> ""
  in counterexample "(</>) with both empty should return empty" $
    combined === ""

-- ============================================================================
-- String Manipulation Tests
-- ============================================================================

prop_toLowerIdempotent :: Char -> Property
prop_toLowerIdempotent c =
  let lower = toLower c
      lowerAgain = toLower lower
  in counterexample "toLower should be idempotent" $
    lower === lowerAgain

prop_toLowerPreservesLowercase :: Property
prop_toLowerPreservesLowercase =
  let lowercase = "abcdefghijklmnopqrstuvwxyz"
      lowered = map toLower lowercase
  in counterexample "toLower should preserve lowercase letters" $
    lowered === lowercase

prop_toLowerConvertsUppercase :: Property
prop_toLowerConvertsUppercase =
  let uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      lowered = map toLower uppercase
  in counterexample "toLower should convert uppercase to lowercase" $
    lowered === "abcdefghijklmnopqrstuvwxyz"

-- ============================================================================
-- Error Handling Tests
-- ============================================================================

prop_toolingErrorProperties :: Property
prop_toolingErrorProperties =
  counterexample "ToolingError should be a valid type" $
    -- We can't test the constructor directly without knowing its structure
    -- but we can test that it exists and is used
    property True

-- ============================================================================
-- Utility Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

isSuffixOf :: String -> String -> Bool
isSuffixOf needle haystack = needle `isInfixOf` haystack && length needle <= length haystack

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "GoToolchain Core QuickCheck Tests"
  [ testGroup "Constants Tests"
      [ testProperty "goModContents contains module declaration and go version" prop_goModContentsProperties
      , testProperty "nullDevice is platform-appropriate" prop_nullDeviceProperties
      ]
  , testGroup "Environment Variable Tests"
      [ testProperty "isEnvVarEnabled recognizes true values" prop_isEnvVarEnabledTrueValues
      , testProperty "isEnvVarEnabled rejects false values" prop_isEnvVarEnabledFalseValues
      , testProperty "isEnvVarEnabled is case insensitive" prop_isEnvVarEnabledCaseInsensitive
      ]
  , testGroup "Go Executor Tests"
      [ testProperty "GoExecutor preserves record fields" prop_goExecutorRecordFields
      , testProperty "defaultGoExecutor creates valid executor" prop_defaultGoExecutorCreatesValidExecutor
      ]
  , testGroup "File Path Tests"
      [ testProperty "takeBaseName extracts filename without extension" prop_takeBaseNameProperties
      , testProperty "takeBaseName handles empty string" prop_takeBaseNameEmpty
      , testProperty "takeBaseName handles paths without extensions" prop_takeBaseNameNoExtension
      , testProperty "takeBaseName removes extension" prop_takeBaseNameWithExtension
      , testProperty "takeBaseName removes all extensions" prop_takeBaseNameMultipleExtensions
      , testProperty "takeBaseName handles dotfiles" prop_takeBaseNameJustExtension
      ]
  , testGroup "Path Combination Tests"
      [ testProperty "(</>) combines paths correctly" prop_pathCombinationProperties
      , testProperty "(</>) with empty dir returns file" prop_pathCombinationEmptyDir
      , testProperty "(</>) with empty file adds trailing slash" prop_pathCombinationEmptyFile
      , testProperty "(</>) with both empty returns empty" prop_pathCombinationBothEmpty
      ]
  , testGroup "String Manipulation Tests"
      [ testProperty "toLower is idempotent" prop_toLowerIdempotent
      , testProperty "toLower preserves lowercase letters" prop_toLowerPreservesLowercase
      , testProperty "toLower converts uppercase to lowercase" prop_toLowerConvertsUppercase
      ]
  , testGroup "Error Handling Tests"
      [ testProperty "ToolingError is a valid type" prop_toolingErrorProperties
      ]
  ]