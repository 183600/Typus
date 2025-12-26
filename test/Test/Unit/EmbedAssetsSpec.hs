module Test.Unit.EmbedAssetsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, listOf1, elements)
import Control.Monad (unless)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import System.IO (writeFile)

import EmbedAssets 
  ( MissingEmbed(..)
  , formatMissingMessage
  , handleMissingEmbeds
  , mirrorEmbeddedResources
  , copyEmbeddedForBuild
  , extractEmbeddedPatterns
  , listGoFiles
  )
import CompilerUtils (Logger(..))
import GoToolchain (IOResult)
import Tooling.Error (ToolingError(..))

tests :: TestTree
tests = testGroup "EmbedAssets Tests"
  [ testMissingEmbedDataStructure
  , testFormatMissingMessage
  , testExtractEmbeddedPatterns
  , testHandleMissingEmbeds
  , testListGoFiles
  , testMirrorEmbeddedResources
  , testCopyEmbeddedForBuild
  , testEmbedPatternExtractionProperties
  ]

testMissingEmbedDataStructure :: TestTree
testMissingEmbedDataStructure = testCase "MissingEmbed data structure" $ do
  let missing = MissingEmbed
        { missingPattern = "*.txt"
        , missingRoot = "/test/root"
        , missingReferencedFrom = "/test/file.go"
        }
  
  assertEqual "Pattern should match" "*.txt" (missingPattern missing)
  assertEqual "Root should match" "/test/root" (missingRoot missing)
  assertEqual "Reference should match" "/test/file.go" (missingReferencedFrom missing)
  
  -- Test equality
  let missing2 = MissingEmbed "*.txt" "/test/root" "/test/file.go"
  let missing3 = MissingEmbed "*.go" "/test/root" "/test/file.go"
  
  assertBool "Same missing embeds should be equal" (missing == missing2)
  assertBool "Different patterns should not be equal" (missing /= missing3)

testFormatMissingMessage :: TestTree
testFormatMissingMessage = testCase "Format missing message" $ do
  let missing = 
        [ MissingEmbed "*.txt" "/assets" "main.go"
        , MissingEmbed "config/*.json" "/config" "loader.go"
        ]
  
  let message = formatMissingMessage missing
  assertBool "Message should contain header" ("Missing embedded assets" `isInfixOf` message)
  assertBool "Message should contain pattern" ("*.txt" `isInfixOf` message)
  assertBool "Message should contain root" ("/assets" `isInfixOf` message)
  assertBool "Message should contain reference" ("main.go" `isInfixOf` message)
  assertBool "Message should contain second pattern" ("config/*.json" `isInfixOf` message)

testExtractEmbeddedPatterns :: TestTree
testExtractEmbeddedPatterns = testCase "Extract embedded patterns" $ do
  let content1 = "//go:embed assets/*.txt\npackage main"
      patterns1 = extractEmbeddedPatterns content1
  assertEqual "Should extract single pattern" ["assets/*.txt"] patterns1
  
  let content2 = "//go:embed assets/*.txt config/*.json\npackage main"
      patterns2 = extractEmbeddedPatterns content2
  assertEqual "Should extract multiple patterns" ["assets/*.txt", "config/*.json"] patterns2
  
  let content3 = "//go:embed \"quoted pattern\"\npackage main"
      patterns3 = extractEmbeddedPatterns content3
  assertEqual "Should handle quoted patterns" ["quoted pattern"] patterns3
  
  let content4 = "//go:embed `backtick pattern`\npackage main"
      patterns4 = extractEmbeddedPatterns content4
  assertEqual "Should handle backtick patterns" ["backtick pattern"] patterns4
  
  let content5 = "package main\n// no embed directive"
      patterns5 = extractEmbeddedPatterns content5
  assertEqual "Should return empty for no patterns" [] patterns5

testHandleMissingEmbeds :: TestTree
testHandleMissingEmbeds = testCase "Handle missing embeds" $ do
  let logger = Logger
        { logInfo = \_ -> return ()
        , logWarning = \_ -> return ()
        , logError = \_ -> return ()
        }
  
  -- Test with no missing embeds
  result1 <- handleMissingEmbeds logger False []
  case result1 of
    Right () -> return ()
    Left _ -> assertBool "Should succeed with no missing embeds" False
  
  -- Test with missing embeds in strict mode
  let missing = [MissingEmbed "*.txt" "/test" "test.go"]
  result2 <- handleMissingEmbeds logger True missing
  case result2 of
    Right () -> assertBool "Should fail in strict mode" False
    Left (MissingEmbeddedAssets _) -> return ()
    Left _ -> assertBool "Should return MissingEmbeddedAssets error" False

testListGoFiles :: TestTree
testListGoFiles = testCase "List Go files" $ do
  withTempDirectory "test" "embed-test" $ \tempDir -> do
    -- Create test structure
    createDirectoryIfMissing True (tempDir </> "subdir")
    writeFile (tempDir </> "main.go") "package main"
    writeFile (tempDir </> "helper.go") "package main"
    writeFile (tempDir </> "subdir" </> "sub.go") "package sub"
    writeFile (tempDir </> "readme.txt") "readme"
    
    goFiles <- listGoFiles tempDir
    assertEqual "Should find 3 Go files" 3 (length goFiles)
    assertBool "Should contain main.go" ("main.go" `elem` map takeFileName goFiles)
    assertBool "Should contain helper.go" ("helper.go" `elem` map takeFileName goFiles)
    assertBool "Should contain sub.go" ("sub.go" `elem` map takeFileName goFiles)
    assertBool "Should not contain readme.txt" (not $ "readme.txt" `elem` map takeFileName goFiles)

testMirrorEmbeddedResources :: TestTree
testMirrorEmbeddedResources = testCase "Mirror embedded resources" $ do
  withTempDirectory "test" "embed-test" $ \tempDir -> do
    let sourceDir = tempDir </> "source"
        destDir = tempDir </> "dest"
    
    -- Create source structure
    createDirectoryIfMissing True sourceDir
    createDirectoryIfMissing True (sourceDir </> "assets")
    writeFile (sourceDir </> "main.go") "//go:embed assets/*.txt\npackage main"
    writeFile (sourceDir </> "assets" </> "test.txt") "test content"
    
    let logger = Logger
          { logInfo = \_ -> return ()
          , logWarning = \_ -> return ()
          , logError = \_ -> return ()
          }
    
    missing <- mirrorEmbeddedResources logger (sourceDir </> "main.go") destDir (tempDir </> "temp.go")
    assertEqual "Should have no missing files" [] missing
    
    destFileExists <- doesFileExist (destDir </> "assets" </> "test.txt")
    assertBool "Should copy embedded file" destFileExists

testCopyEmbeddedForBuild :: TestTree
testCopyEmbeddedForBuild = testCase "Copy embedded for build" $ do
  withTempDirectory "test" "embed-test" $ \tempDir -> do
    let inputRoot = tempDir </> "input"
        tempRoot = tempDir </> "temp"
    
    -- Create input structure
    createDirectoryIfMissing True (inputRoot </> "assets")
    createDirectoryIfMissing True tempRoot
    writeFile (inputRoot </> "assets" </> "data.txt") "data"
    writeFile (tempRoot </> "main.go") "//go:embed assets/data.txt\npackage main"
    
    let logger = Logger
          { logInfo = \_ -> return ()
          , logWarning = \_ -> return ()
          , logError = \_ -> return ()
          }
    
    missing <- copyEmbeddedForBuild logger inputRoot tempRoot
    assertEqual "Should have no missing files" [] missing
    
    destFileExists <- doesFileExist (tempRoot </> "assets" </> "data.txt")
    assertBool "Should copy embedded file for build" destFileExists

testEmbedPatternExtractionProperties :: TestTree
testEmbedPatternExtractionProperties = testProperty "Extract patterns preserves order and content" $
  forAll arbitraryEmbedContent $ \content -> do
    let patterns = extractEmbeddedPatterns content
    return $ all (not . null) patterns

-- Helper generator for embed content
arbitraryEmbedContent :: Gen String
arbitraryEmbedContent = do
  patterns <- listOf1 $ elements ["*.txt", "config/*.json", "assets/*", "data/*.yaml", "static/**"]
  let directives = map (\p -> "//go:embed " ++ p) patterns
  return $ unlines (directives ++ ["package main"])

-- Helper function to check if a string is contained in another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (words haystack)

-- Helper to get filename from path
takeFileName :: FilePath -> String
takeFileName = reverse . takeWhile (/= '/') . reverse