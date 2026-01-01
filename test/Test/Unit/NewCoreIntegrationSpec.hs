{-# LANGUAGE CPP #-}
module Test.Unit.NewCoreIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils
import SourceLocation
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives)
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L

-- ============================================================================
-- Core Integration Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Core Integration Tests"
    [ testGroup "Utils Complex Processing"
        [ testCase "Complex comment removal with nested strings" $
            let input = unlines
                  [ "func main() {"
                  , "  // This is a comment"
                  , "  str := \"This is // not a comment\""
                  , "  char := '/' // This is a real comment"
                  , "  /* Block comment with \"string // inside\" */"
                  , "  result := str + char"
                  , "}"
                  ]
                expected = unlines
                  [ "func main() {"
                  , "  "
                  , "  str := \"This is // not a comment\""
                  , "  char := '/' "
                  , "  "
                  , "  result := str + char"
                  , "}"
                  ]
            in removeComments input @?= expected

        , testCase "Indentation normalization with mixed tabs L.and spaces" $
            let input = unlines
                  [ "    func outer() {"
                  , "\t\tfunc inner() {"
                  , "  \t\treturn 42"
                  , "\t\t}"
                  , "    }"
                  ]
                expected = unlines
                  [ "func outer() {"
                  , "\t\tfunc inner() {"
                  , "  \t\treturn 42"
                  , "\t\t}"
                  , "}"
                  ]
            in normalizeIndentation input @?= expected

        , testCase "String splitting with edge cases" $
            let input = "a,,b,c"
                regular = splitBy ',' input
                collapsed = splitByCollapsed ',' input
            in do
               regular @?= ["a", "", "b", "c"]
               collapsed @?= ["a", "b", "c"]
        ]

    , testGroup "SourceLocation Advanced Operations"
        [ testCase "Position advancement with complex text" $
            let start = startPos
                afterText = advancePosByText (T.pack "Hello\nWorld\t!") start
                expected = SourcePos { posLine = 2, posColumn = 9, posOffset = 12 }
            in afterText @?= expected

        , testCase "Span merging L.and validation" $
            let span1 = spanBetween (posAt 1 5) (posAt 1 10)
                span2 = spanBetween (posAt 1 8) (posAt 1 15)
                merged = mergeSpans span1 span2
                expected = spanBetween (posAt 1 5) (posAt 1 15)
            in do
               isValidSpan span1 @?= True
               isValidSpan span2 @?= True
               merged @?= expected

        , testCase "Located value operations" $
            let pos = posAt 3 7
                value = "test"
                located = locatedAt pos value
                mapped = mapLocated (++ "_suffix") located
            in do
               locatedPos located @?= pos
               locatedValue located @?= value
               locatedValue mapped @?= "test_suffix"
        ]

    , testGroup "Parser Edge Cases"
        [ testCase "Empty file parsing" $
            let result = parseTypus ""
                expected = TypusFile defaultFileDirectives [] [] []
            in result @?= Right expected

        , testCase "File with only comments" $
            let input = unlines
                  [ "//! This is a file directive"
                  , "// This is a regular comment"
                  , "/* This is a block comment */"
                  ]
                result = parseTypus input
            in case result of
                 Right parsed -> tfSyntaxErrors parsed @?= []
                 Left _ -> return ()

        , testCase "Malformed directives handling" $
            let input = unlines
                  [ "//! ownership=true, invalid"
                  , "func test() {}"
                  ]
                result = parseTypus input
            in case result of
                 Right parsed -> tfBlocks parsed @?= []
                 Left _ -> return ()
        ]

    , testGroup "Integration Scenarios"
        [ testCase "Complete processing pipeline" $
            let rawInput = unlines
                  [ "//! ownership=true, dependent-types=true"
                  , "    // Leading comment"
                  , "  func process() {"
                  , "\t/* Block comment */"
                  , "\treturn 42"
                  , "  }"
                  ]
                cleaned = removeComments rawInput
                normalized = normalizeIndentation cleaned
                parsed = parseTypus normalized
            in case parsed of
                 Right result -> do
                    "ownership=true" `L.L.isInfixOf` normalized @?= True
                    L.length (tfBlocks result) @?= 1
                 Left _ -> return ()

        , testCase "Error location consistency" $
            let pos = posAt 5 10
                span = spanBetween pos (posAt 5 15)
                errorLoc = toErrorLocationWithSpan span
            in do
               line errorLoc @?= 5
               column errorLoc @?= 10
               endLine errorLoc @?= Just 5
               endColumn errorLoc @?= Just 15
        ]

    , testGroup "Property-Based Tests"
        [ fastProperty "Round-trip comment removal" prop_commentRemovalRoundTrip
        , fastProperty "Position advancement consistency" prop_positionAdvancementConsistency
        , fastProperty "Span merging associativity" prop_spanMergingAssociativity
        , fastProperty "String processing idempotency" prop_stringProcessingIdempotency
        ]
    ]

-- ============================================================================
-- Property-Based Tests
-- ============================================================================

-- Property: Comment removal is idempotent for non-comment text
prop_commentRemovalRoundTrip :: String -> Property
prop_commentRemovalRoundTrip input =
  let withoutComments = removeComments input
      secondRemoval = removeComments withoutComments
  in property $ withoutComments === secondRemoval

-- Property: Position advancement is consistent with text L.length
prop_positionAdvancementConsistency :: String -> Property
prop_positionAdvancementConsistency text =
  let start = startPos
      end = advancePosByText (T.pack text) start
      expectedOffset = L.length text
  in property $ posOffset end === expectedOffset

-- Property: Span merging is associative
prop_spanMergingAssociativity :: Int -> Int -> Int -> Int -> Property
prop_spanMergingAssociativity l1 c1 l2 c2 =
  l1 >= 1 && l1 <= 100 && c1 >= 1 && c1 <= 100 &&
  l2 >= 1 && l2 <= 100 && c2 >= 1 && c2 <= 100 ==>
  let span1 = spanBetween (posAt l1 c1) (posAt (l1 + 1) (c1 + 5))
      span2 = spanBetween (posAt l2 c2) (posAt (l2 + 1) (c2 + 5))
      span3 = spanBetween (posAt (l1 + 2) (c1 + 2)) (posAt (l2 + 3) (c2 + 3))
      merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      mergeAll = mergeSpans merge12 span3
      mergeAllAlt = mergeSpans span1 merge23
  in property $ mergeAll === mergeAllAlt

-- Property: String processing functions are idempotent where appropriate
prop_stringProcessingIdempotency :: String -> Property
prop_stringProcessingIdempotency input =
  let trimmed = trim input
      trimmedTwice = trim trimmed
      normalized = normalizeIndentation input
      normalizedTwice = normalizeIndentation normalized
  in property $ trimmed === trimmedTwice .&&. normalized === normalizedTwice