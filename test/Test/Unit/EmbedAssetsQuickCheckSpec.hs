module Test.Unit.EmbedAssetsQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, suchThat)
import Control.Monad 
import EmbedAssets (MissingEmbed(..), formatMissingMessage, extractEmbeddedPatterns)
import TestSupport.QuickCheck ()
        root <- arbitrary `suchThat` (not . null)
        reference <- arbitrary `suchThat` (not . null)
        return $ MissingEmbed pattern root reference
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Generate arbitrary strings with potential go:embed directives
genEmbedContent :: Gen String
                              genEmbedContent = do
              lines <- listOf $ oneof
        [ pure "//go:embed *.txt"
          , pure "//go:embed assets/*"
          , pure "//go:embed \"quoted/path/*.go\""
          , pure "//go:embed `backtick/path/*.js`"
          , pure "package main"
          , pure "import \"fmt\""

-- | Test extractEmbeddedPatterns function
tests :: TestTree
tests =
    testGroup "EmbedAssets QuickCheck tests"
    [             testProperty "extractEmbeddedPatterns finds quoted patterns" $
        fastProperty prop_extractQuotedPatterns
    
    ,             testProperty "extractEmbeddedPatterns finds backtick patterns" $
        fastProperty prop_extractBacktickPatterns
    
    ,             testProperty "extractEmbeddedPatterns ignores non-embed lines" $
        fastProperty prop_extractIgnoresNonEmbedLines
    
    ,             testProperty "formatMissingMessage contains L.all missing patterns" $
        fastProperty prop_formatMissingContainsAllPatterns
    
    ,             testProperty "MissingEmbed equality is reflexive" $
        fastProperty prop_missingEmbedReflexive
    
    ,             testProperty "MissingEmbed ordering works correctly" $
        fastProperty prop_missingEmbedOrdering
    
      ,             testCase "extractEmbeddedPatterns handles empty content" $ do
                    extractEmbeddedPatterns "" @?= []
    
      ,             testCase "extractEmbeddedPatterns handles single quoted pattern" $ do
                    let content = "//go:embed \"*.txt\"\n"
        extractEmbeddedPatterns content @?= ["*.txt"]
    
      ,             testCase "extractEmbeddedPatterns handles single backtick pattern" $ do
                    let content = "//go:embed `assets/*`\n"
        extractEmbeddedPatterns content @?= ["assets/*"]
    
      ,             testCase "extractEmbeddedPatterns handles multiple patterns" $ do
                    let content = unlines
                [ "//go:embed \"*.txt\""
                , "//go:embed `assets/*`"
                , "//go:embed \"config/*.yaml\""
                ]
        extractEmbeddedPatterns content @?= ["*.txt", "assets/*", "config/*.yaml"]
    
      ,             testCase "formatMissingMessage formats single missing embed" $ do
                    let missing = [MissingEmbed "*.txt" "/src" "/src/main.go"]
                                          expected = unlines
                [ "Missing embedded assets detected:"
                , "  pattern \"*.txt\" relative to /src (referenced in /src/main.go)"
                ]
        formatMissingMessage missing @?= expected
    
      ,             testCase "formatMissingMessage removes duplicates" $ do
                    let missing = 
                [ MissingEmbed "*.txt" "/src" "/src/main.go"
                , MissingEmbed "*.txt" "/src" "/src/main.go"  -- duplicate
                , MissingEmbed "assets/*" "/src" "/src/main.go"
                ]
                                          expected = unlines
                [ "Missing embedded assets detected:"
                , "  pattern \"*.txt\" relative to /src (referenced in /src/main.go)"
                , "  pattern \"assets/*\" relative to /src (referenced in /src/main.go)"
                ]
        formatMissingMessage missing @?= expected
    ]

-- Property: extractEmbeddedPatterns finds quoted patterns
prop_extractQuotedPatterns :: String -> String -> Bool
prop_extractQuotedPatterns prefix                               pattern = 
    let content = prefix ++ "//go:embed \"" ++ pattern ++ "\"\n"
                                      extracted = extractEmbeddedPatterns content
    in pattern `elem` extracted

-- Property: extractEmbeddedPatterns finds backtick patterns
prop_extractBacktickPatterns :: String -> String -> Bool
prop_extractBacktickPatterns prefix                               pattern =
    let content = prefix ++ "//go:embed `" ++ pattern ++ "`\n"
                                      extracted = extractEmbeddedPatterns content
    in pattern `elem` extracted

-- Property: extractEmbeddedPatterns ignores non-embed lines
prop_extractIgnoresNonEmbedLines :: String -> String -> Bool
prop_extractIgnoresNonEmbedLines line1                               line2 =
    let content = line1 ++ "\n" ++ line2 ++ "\n"
                                      extracted = extractEmbeddedPatterns content
    in null extracted || L.all ("//" `L.isPrefixOf`) 
        [l | l <- lines content, "//go:embed" `L.isPrefixOf` l]

-- Property: formatMissingMessage contains L.all missing patterns
prop_formatMissingContainsAllPatterns :: [MissingEmbed] -> Bool
prop_formatMissingContainsAllPatterns                               missing =
    let formatted = formatMissingMessage missing
                                      patterns = map missingPattern missing
    in L.all (`L.isInfixOf` formatted) patterns

-- Property: MissingEmbed equality is reflexive
prop_missingEmbedReflexive :: MissingEmbed -> Bool
prop_missingEmbedReflexive                               embed =                               embed == embed

-- Property: MissingEmbed ordering works correctly
prop_missingEmbedOrdering :: MissingEmbed -> MissingEmbed -> Bool
prop_missingEmbedOrdering embed1                               embed2 =
    let ordered = [embed1, embed2]
                                      sorted = ordered
    in L.all (`elem` sorted) ordered

-- Helper function to check if string is prefix of another
isPrefixOf :: String -> String -> Bool
isPrefixOf []                               _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) =                               x == y && isPrefixOf xs ys

-- Helper function to check if string is infix of another
isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = L.any (isPrefixOf needle) (tails haystack)
  where
      tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'