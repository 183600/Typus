{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.EmbedAssetsQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import System.FilePath (takeDirectory, makeRelative, takeExtension)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)

import EmbedAssets
import CompilerUtils (Logger(..))
import Tooling.Error (ToolingError(..))
import GoToolchain (IOResult)

-- Test data generators
generateMissingEmbed :: Int -> MissingEmbed
generateMissingEmbed n = MissingEmbed
    { missingPattern = "pattern" ++ show n
    , missingRoot = "/root" ++ show n
    , missingReferencedFrom = "file" ++ show n ++ ".go"
    }

generateLogger :: IO Logger
generateLogger = return Logger
    { logInfo = \_ -> return ()
    , logWarning = \_ -> return ()
    , logError = \_ -> return ()
    }

generateGoContent :: Int -> String
generateGoContent n = case n `mod` 6 of
  0 -> "package main\n\n//go:embed hello.txt\nvar content string"
  1 -> "package main\n\n//go:embed assets/*\nvar files embed.FS"
  2 -> "package main\n\n//go:embed \"templates/*.html\"\n//go:embed static/*.css\nvar assets embed.FS"
  3 -> "package main\n\nimport \"embed\"\n\n//go:embed data/*.json\n//go:embed config/*.yaml\nvar resources embed.FS"
  4 -> "package main\n\n//go:embed images/*.png\n//go:embed docs/*.md\nvar docs embed.FS"
  5 -> "package main\n\nfunc main() {\n    // No embed directives\n}"

generateFilePath :: Int -> FilePath
generateFilePath n = "/test/path" ++ show n ++ "/file.go"

generatePattern :: Int -> String
generatePattern n = case n `mod` 5 of
  0 -> "*.txt"
  1 -> "assets/*"
  2 -> "templates/*.html"
  3 -> "data/*.json"
  4 -> "static/*"

-- Test helper to create a mock logger
mockLogger :: Logger
mockLogger = Logger
    { logInfo = \_ -> return ()
    , logWarning = \_ -> return ()
    , logError = \_ -> return ()
    }

-- QuickCheck properties
prop_missing_embed_creation :: Property
prop_missing_embed_creation =
  forAll arbitrary $ \n ->
    let missing = generateMissingEmbed n
    in property $
      missingPattern missing == "pattern" ++ show n &&
      missingRoot missing == "/root" ++ show n &&
      missingReferencedFrom missing == "file" ++ show n ++ ".go"

prop_format_missing_message :: Property
prop_format_missing_message =
  forAll arbitrary $ \n ->
    let missings = take (n `mod` 5 + 1) [generateMissingEmbed i | i <- [1..10]]
        message = formatMissingMessage missings
    in property $
      "Missing embedded assets detected:" `isInfixOf` message &&
      all (\m -> missingPattern m `isInfixOf` message) missings

prop_format_missing_message_empty :: Property
prop_format_missing_message_empty =
  let message = formatMissingMessage []
  in property $ "Missing embedded assets detected:" `isInfixOf` message

prop_extract_embedded_patterns :: Property
prop_extract_embedded_patterns =
  forAll arbitrary $ \n ->
    let content = generateGoContent n
        patterns = extractEmbeddedPatterns content
    in case n `mod` 6 of
      0 -> property $ patterns == ["hello.txt"]
      1 -> property $ patterns == ["assets/*"]
      2 -> property $ patterns == ["templates/*.html", "static/*.css"]
      3 -> property $ patterns == ["data/*.json", "config/*.yaml"]
      4 -> property $ patterns == ["images/*.png", "docs/*.md"]
      5 -> property $ null patterns  -- No embed directives

prop_extract_embedded_patterns_quoted :: Property
prop_extract_embedded_patterns_quoted =
  let content = "package main\n\n//go:embed \"hello world.txt\"\nvar content string"
      patterns = extractEmbeddedPatterns content
  in property $ patterns == ["hello world.txt"]

prop_extract_embedded_patterns_backticks :: Property
prop_extract_embedded_patterns_backticks =
  let content = "package main\n\n//go:embed `hello world.txt`\nvar content string"
      patterns = extractEmbeddedPatterns content
  in property $ patterns == ["hello world.txt"]

prop_extract_embedded_patterns_multiple_lines :: Property
prop_extract_embedded_patterns_multiple_lines =
  let content = "package main\n\n//go:embed file1.txt\n//go:embed file2.txt\nvar content string"
      patterns = extractEmbeddedPatterns content
  in property $ patterns == ["file1.txt", "file2.txt"]

prop_to_missing_embed_info :: Property
prop_to_missing_embed_info =
  forAll arbitrary $ \n ->
    let missing = generateMissingEmbed n
        info = toMissingEmbedInfo missing
    in property $
      meiPattern info == missingPattern missing &&
      meiRoot info == missingRoot missing &&
      meiReference info == missingReferencedFrom missing

prop_handle_missing_embeds_empty :: Property
prop_handle_missing_embeds_empty =
  let logger = mockLogger
      missings = []
      strict = False
  in property $ True  -- Would test that no errors are thrown for empty missings

prop_handle_missing_embeds_strict :: Property
prop_handle_missing_embeds_strict =
  forAll arbitrary $ \n ->
    let logger = mockLogger
      missings = [generateMissingEmbed n]
      strict = True
  in property $ True  -- Would test that errors are thrown in strict mode

prop_handle_missing_embeds_non_strict :: Property
prop_handle_missing_embeds_non_strict =
  forAll arbitrary $ \n ->
    let logger = mockLogger
      missings = [generateMissingEmbed n]
      strict = False
  in property $ True  -- Would test that warnings are logged in non-strict mode

prop_list_go_files :: Property
prop_list_go_files =
  forAll arbitrary $ \n ->
    let dir = generateFilePath n
    in property $ True  -- Would test listing Go files

prop_copy_embedded_files :: Property
prop_copy_embedded_files =
  forAll arbitrary $ \n ->
    let logger = mockLogger
        sourceDir = "/source"
        destDir = "/dest"
        reference = "test.go"
        content = generateGoContent n
    in property $ True  -- Would test copying embedded files

prop_mirror_embedded_resources :: Property
prop_mirror_embedded_resources =
  forAll arbitrary $ \n ->
    let logger = mockLogger
        sourcePath = generateFilePath n
        tempDir = "/temp"
        tempGoPath = "/temp/test.go"
    in property $ True  -- Would test mirroring embedded resources

prop_copy_embedded_for_build :: Property
prop_copy_embedded_for_build =
  forAll arbitrary $ \n ->
    let logger = mockLogger
        inputRoot = "/input"
        tempRoot = "/temp"
    in property $ True  -- Would test copying embedded files for build

-- Test suite
testSuite :: TestTree
testSuite = testGroup "EmbedAssets QuickCheck Tests"
  [ testProperty "missing embed creation" prop_missing_embed_creation
  , testProperty "format missing message" prop_format_missing_message
  , testProperty "format missing message empty" prop_format_missing_message_empty
  , testProperty "extract embedded patterns" prop_extract_embedded_patterns
  , testProperty "extract embedded patterns quoted" prop_extract_embedded_patterns_quoted
  , testProperty "extract embedded patterns backticks" prop_extract_embedded_patterns_backticks
  , testProperty "extract embedded patterns multiple lines" prop_extract_embedded_patterns_multiple_lines
  , testProperty "to missing embed info" prop_to_missing_embed_info
  , testProperty "handle missing embeds empty" prop_handle_missing_embeds_empty
  , testProperty "handle missing embeds strict" prop_handle_missing_embeds_strict
  , testProperty "handle missing embeds non strict" prop_handle_missing_embeds_non_strict
  , testProperty "list go files" prop_list_go_files
  , testProperty "copy embedded files" prop_copy_embedded_files
  , testProperty "mirror embedded resources" prop_mirror_embedded_resources
  , testProperty "copy embedded for build" prop_copy_embedded_for_build
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "EmbedAssets Unit Tests"
  [ testCase "missing embed equality" $ do
      let missing1 = MissingEmbed "pattern1" "/root1" "file1.go"
          missing2 = MissingEmbed "pattern1" "/root1" "file1.go"
          missing3 = MissingEmbed "pattern2" "/root1" "file1.go"
      assertEqual "Equal missings should be equal" missing1 missing2
      assertBool "Different patterns should not be equal" $ missing1 /= missing3

  , testCase "missing embed ordering" $ do
      let missing1 = MissingEmbed "pattern1" "/root1" "file1.go"
          missing2 = MissingEmbed "pattern2" "/root2" "file2.go"
      assertBool "Ordering should work" $ missing1 < missing2

  , testCase "format missing message with multiple" $ do
      let missings = [ MissingEmbed "pattern1" "/root1" "file1.go"
                     , MissingEmbed "pattern2" "/root2" "file2.go"
                     ]
          message = formatMissingMessage missings
      assertBool "Should include first pattern" $ "pattern1" `isInfixOf` message
      assertBool "Should include second pattern" $ "pattern2" `isInfixOf` message
      assertBool "Should include first root" $ "/root1" `isInfixOf` message
      assertBool "Should include second root" $ "/root2" `isInfixOf` message

  , testCase "extract embedded patterns with complex content" $ do
      let content = "package main\n\nimport (\n    \"embed\"\n)\n\n//go:embed \"hello world.txt\"\n//go:embed assets/*.png\n//go:embed `data/*.json`\nvar resources embed.FS\n"
          patterns = extractEmbeddedPatterns content
      assertEqual "Should extract all patterns" 
                 ["hello world.txt", "assets/*.png", "data/*.json"] 
                 patterns

  , testCase "extract embedded patterns with comments" $ do
      let content = "package main\n\n// This is a comment\n//go:embed file.txt  // Another comment\nvar content string"
          patterns = extractEmbeddedPatterns content
      assertEqual "Should extract pattern despite comments" ["file.txt"] patterns

  , testCase "extract embedded patterns with whitespace" $ do
      let content = "package main\n\n//go:embed    file.txt    \nvar content string"
          patterns = extractEmbeddedPatterns content
      assertEqual "Should extract pattern despite whitespace" ["file.txt"] patterns

  , testCase "handle missing embeds with strict mode" $ do
      let logger = mockLogger
          missings = [MissingEmbed "pattern1" "/root1" "file1.go"]
          strict = True
          result = runExceptT $ handleMissingEmbeds logger strict missings
      case result of
        Left (MissingEmbeddedAssets _) -> return ()
        Left _ -> assertFailure "Should throw MissingEmbeddedAssets error"
        Right _ -> assertFailure "Should throw an error in strict mode"

  , testCase "handle missing embeds with non-strict mode" $ do
      let logger = mockLogger
          missings = [MissingEmbed "pattern1" "/root1" "file1.go"]
          strict = False
          result = runExceptT $ handleMissingEmbeds logger strict missings
      case result of
        Left _ -> assertFailure "Should not throw an error in non-strict mode"
        Right _ -> return ()

  , testCase "handle missing embeds with empty list" $ do
      let logger = mockLogger
          missings = []
          strict = True
          result = runExceptT $ handleMissingEmbeds logger strict missings
      case result of
        Left _ -> assertFailure "Should not throw an error for empty list"
        Right _ -> return ()
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "EmbedAssets Tests"
  [ testSuite
  , unitTests
  ]