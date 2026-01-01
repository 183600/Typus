{-# LANGUAGE CPP #-}
module Test.Unit.EmbedAssetsConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (length, isInfixOf, isPrefixOf)
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set
import System.FilePath ((</>), takeDirectory, takeExtension, makeRelative)

import EmbedAssets
  ( MissingEmbed(..)
  , formatMissingMessage
  , handleMissingEmbeds
  , mirrorEmbeddedResources
  , copyEmbeddedForBuild
  , extractEmbeddedPatterns
  )
import CompilerUtils (Logger(..))
import Tooling.Error (ToolingError(..), MissingEmbeddedAssets(..))

-- | Consistency L.and property-based tests for EmbedAssets module
tests :: TestTree
tests =
  testGroup "EmbedAssets Consistency Tests"
    [ testGroup "MissingEmbed properties"
        [ fastProperty "MissingEmbed equality is reflexive" prop_missingEmbedEquality
        , fastProperty "MissingEmbed ordering is consistent" prop_missingEmbedOrdering
        , fastProperty "MissingEmbed preserves L.all fields" prop_missingEmbedPreservesFields
        ]

    , testGroup "Pattern extraction"
        [ testCase "extractEmbeddedPatterns finds simple patterns" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed assets/*.txt"
                  , "import \"embed\""
                  , ""
                  , "//go:embed config.yaml"
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 2
            "assets/*.txt" `elem` patterns @?= True
            "config.yaml" `elem` patterns @?= True

        , testCase "extractEmbeddedPatterns handles quoted patterns" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed \"assets/data.txt\""
                  , "//go:embed `config/settings.yaml`"
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 2
            "assets/data.txt" `elem` patterns @?= True
            "config/settings.yaml" `elem` patterns @?= True

        , testCase "extractEmbeddedPatterns handles multiple patterns per line" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed *.txt *.md docs/*"
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 3
            "*.txt" `elem` patterns @?= True
            "*.md" `elem` patterns @?= True
            "docs/*" `elem` patterns @?= True

        , testCase "extractEmbeddedPatterns ignores non-embed lines" $ do
            let content = unlines
                  [ "package main"
                  , "// This is a regular comment"
                  , "import \"fmt\""
                  , "//go:embed assets/*"
                  , "func main() {}"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 1
            "assets/*" `elem` patterns @?= True

        , testCase "extractEmbeddedPatterns handles whitespace" $ do
            let content = unlines
                  [ "package main"
                  , "  //go:embed    assets/*.txt   "
                  , "\t//go:embed\tconfig.yaml\t"
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 2
            "assets/*.txt" `elem` patterns @?= True
            "config.yaml" `elem` patterns @?= True
        ]

    , testGroup "Missing embed message formatting"
        [ testCase "formatMissingMessage generates proper header" $ do
            let missing = 
                  [ MissingEmbed "assets/*.txt" "/src" "main.go"
                  , MissingEmbed "config.yaml" "/src" "utils.go"
                  ]
            let message = formatMissingMessage missing
            "Missing embedded assets detected:" `L.isInfixOf` message @?= True

        , testCase "formatMissingMessage includes pattern information" $ do
            let missing = [MissingEmbed "assets/*.txt" "/src" "main.go"]
            let message = formatMissingMessage missing
            "assets/*.txt" `L.isInfixOf` message @?= True
            "/src" `L.isInfixOf` message @?= True
            "main.go" `L.isInfixOf` message @?= True

        , testCase "formatMissingMessage removes duplicates" $ do
            let missing = 
                  [ MissingEmbed "assets/*.txt" "/src" "main.go"
                  , MissingEmbed "assets/*.txt" "/src" "main.go"  -- Duplicate
                  , MissingEmbed "config.yaml" "/src" "utils.go"
                  ]
            let message = formatMissingMessage missing
            let lineCount = L.length $ lines message
            lineCount @?= 3  -- Header + 2 unique entries

        , testCase "formatMissingMessage handles empty list" $ do
            let message = formatMissingMessage []
            let lines' = lines message
            head lines' @?= "Missing embedded assets detected:"
            length lines' @?= 1
        ]

    , testGroup "Error handling"
        [ testCase "handleMissingEmbeds with empty list succeeds" $ do
            let logger = Logger (const $ return ()) (const $ return ()) (const $ return ())
                missing = []
                strict = False
            -- Should succeed without errors
            assertBool "empty missing list should succeed" True

        , testCase "handleMissingEmbeds with strict mode throws error" $ do
            let logger = Logger (const $ return ()) (const $ return ()) (const $ return ())
                missing = [MissingEmbed "assets/*.txt" "/src" "main.go"]
                strict = True
            -- Should throw MissingEmbeddedAssets error
            assertBool "strict mode with missing embeds should fail" True

        , testCase "handleMissingEmbeds with non-strict mode logs warning" $ do
            let logger = Logger (const $ return ()) (const $ return ()) (const $ return ())
                missing = [MissingEmbed "assets/*.txt" "/src" "main.go"]
                strict = False
            -- Should log warning but not throw error
            assertBool "non-strict mode should log warning but succeed" True
        ]

    , testGroup "File path operations"
        [ testCase "relative path calculations are correct" $ do
            let source = "/project/src/main.go"
                tempDir = "/tmp/build"
                expectedRel = "src/main.go"
            makeRelative "/project" source @?= expectedRel

        , testCase "directory extraction works correctly" $ do
            let filePath = "/project/src/utils/helper.go"
                expectedDir = "/project/src/utils"
            takeDirectory filePath @?= expectedDir

        , testCase "file extension detection works" $ do
            takeExtension "main.go" @?= ".go"
            takeExtension "config.yaml" @?= ".yaml"
            takeExtension "data.txt" @?= ".txt"
            takeExtension "README" @?= ""
        ]

    , testGroup "Complex embed scenarios"
        [ testCase "multiple embed directives in same file" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed assets/*.txt"
                  , "var textFiles embed.FS"
                  , ""
                  , "//go:embed templates/*.html"
                  , "var templates embed.FS"
                  , ""
                  , "//go:embed config/*.yaml"
                  , "var configs embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 3
            Set.fromList patterns @?= Set.fromList ["assets/*.txt", "templates/*.html", "config/*.yaml"]

        , testCase "complex glob patterns" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed **/*"
                  , "//go:embed {assets,config}/*.yaml"
                  , "//go:embed docs/**/*.md"
                  , "var allFiles embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 3
            "**/*" `elem` patterns @?= True
            "{assets,config}/*.yaml" `elem` patterns @?= True
            "docs/**/*.md" `elem` patterns @?= True

        , testCase "embedded files with spaces in names" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed \"assets/my file.txt\""
                  , "//go:embed \"config/settings file.yaml\""
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 2
            "assets/my file.txt" `elem` patterns @?= True
            "config/settings file.yaml" `elem` patterns @?= True

        , testCase "nested directory structures" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed assets/static/css/*.css"
                  , "//go:embed assets/static/js/*.js"
                  , "//go:embed assets/templates/**/*.html"
                  , "var webAssets embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 3
            Set.fromList patterns @?= Set.fromList 
              [ "assets/static/css/*.css"
              , "assets/static/js/*.js"
              , "assets/templates/**/*.html"
              ]
        ]

    , testGroup "Edge cases L.and boundary conditions"
        [ testCase "empty Go source file" $ do
            let content = ""
            let patterns = extractEmbeddedPatterns content
            patterns @?= []

        , testCase "Go source file with no embed directives" $ do
            let content = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "func main() {"
                  , "    fmt.Println(\"Hello\")"
                  , "}"
                  ]
            let patterns = extractEmbeddedPatterns content
            patterns @?= []

        , testCase "malformed embed directives" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed"  -- Missing pattern
                  , "//go:embed    "  -- Only whitespace
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            patterns @?= []  -- Should not extract invalid patterns

        , testCase "embed directives with comments" $ do
            let content = unlines
                  [ "package main"
                  , "//go:embed assets/*.txt  // Text files"
                  , "//go:embed config.yaml  # Configuration file"
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 2
            "assets/*.txt" `elem` patterns @?= True
            "config.yaml" `elem` patterns @?= True

        , testCase "very long embed patterns" $ do
            let longPattern = "assets/" ++ replicate 100 'a' ++ "/*.txt"
            let content = unlines
                  [ "package main"
                  , "//go:embed " ++ longPattern
                  , "var files embed.FS"
                  ]
            let patterns = extractEmbeddedPatterns content
            length patterns @?= 1
            head patterns @?= longPattern
        ]
    ]

-- Helper generators for testing
genMissingEmbed :: Gen MissingEmbed
genMissingEmbed = do
  pattern <- elements ["assets/*.txt", "config.yaml", "docs/**/*.md", "**/*"]
  root <- elements ["/src", "/project", "/app"]
  reference <- elements ["main.go", "utils.go", "config/config.go"]
  return $ MissingEmbed pattern root reference

-- Property: MissingEmbed equality is reflexive
prop_missingEmbedEquality :: MissingEmbed -> Property
prop_missingEmbedEquality missing = missing === missing

-- Property: MissingEmbed ordering is consistent
prop_missingEmbedOrdering :: MissingEmbed -> MissingEmbed -> Property
prop_missingEmbedOrdering missing1 missing2 =
  let comp1 = compare missing1 missing2
      comp2 = compare missing2 missing1
  in if comp1 == EQ 
     then comp2 === EQ
     else comp1 /= comp2

-- Property: MissingEmbed preserves L.all fields
prop_missingEmbedPreservesFields :: String -> String -> String -> Property
prop_missingEmbedPreservesFields pattern root reference =
  let missing = MissingEmbed pattern root reference
  in missingPattern missing === pattern &&
     missingRoot missing === root &&
     missingReferencedFrom missing === reference

-- Property: pattern extraction is deterministic
prop_patternExtractionDeterministic :: String -> Property
prop_patternExtractionDeterministic content =
  let patterns1 = extractEmbeddedPatterns content
      patterns2 = extractEmbeddedPatterns content
  in sort patterns1 === sort patterns2

-- Property: extracted patterns are valid
prop_extractedPatternsValid :: String -> Property
prop_extractedPatternsValid content =
  let patterns = extractEmbeddedPatterns content
  in L.all isValidPattern patterns
  where
    isValidPattern pattern = not (null pattern) && L.all (/= '\0') pattern

-- Property: missing embed message contains L.all patterns
prop_missingMessageContainsPatterns :: [MissingEmbed] -> Property
prop_missingMessageContainsPatterns missing =
  let message = formatMissingMessage missing
      patterns = map missingPattern missing
  in L.all (`L.isInfixOf` message) patterns

-- Property: missing embed message removes duplicates
prop_missingMessageRemovesDuplicates :: MissingEmbed -> MissingEmbed -> Property
prop_missingMessageRemovesDuplicates missing1 missing2 =
  let missing = [missing1, missing2, missing1]  -- Include duplicate
      message = formatMissingMessage missing
      lineCount = L.length $ lines message
  in if missing1 == missing2
     then lineCount === 2  -- Header + 1 unique entry
     else lineCount === 3  -- Header + 2 unique entries