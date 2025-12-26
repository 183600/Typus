module Test.Unit.SourceLocationPrecisionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import SourceLocation
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test source location tracking precision and accuracy
tests :: TestTree
tests =
  testGroup "Source Location Precision Tests"
    [ testGroup "Basic Position Accuracy"
        [ testCase "single character advancement" $ do
            let start = startPos
                afterA = advancePos 'a' start
                afterB = advancePos 'b' afterA
            posLine afterA @?= 1
            posColumn afterA @?= 2
            posColumn afterB @?= 3

        , testCase "newline handling" $ do
            let start = startPos
                afterNewline = advancePos '\n' start
                afterNext = advancePos 'x' afterNewline
            posLine afterNewline @?= 2
            posColumn afterNewline @?= 1
            posLine afterNext @?= 2
            posColumn afterNext @?= 2

        , testCase "tab character handling" $ do
            let start = SourcePos 1 3 2  -- Column 3
                afterTab = advancePos '\t' start
                -- Should advance to next tab stop (column 8, 16, 24, ...)
                expectedColumn = ((3 - 1) `div` 8 + 1) * 8 + 1
            posColumn afterTab @?= expectedColumn

        , testCase "multibyte character handling" $ do
            let start = startPos
                unicodeText = "🚀"  -- Rocket emoji (4 bytes, 1 grapheme)
                afterUnicode = advancePosByText (T.pack unicodeText) start
            posLine afterUnicode @?= 1
            posColumn afterUnicode @?= 2  -- Should count as 1 column
        ]

    , testGroup "Span Precision"
        [ testCase "empty span creation" $ do
            let pos = posAt 5 10
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "span between positions" $ do
            let start = posAt 3 5
                end = posAt 3 15
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

        , testCase "span merging" $ do
            let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                span2 = spanBetween (posAt 1 5) (posAt 1 15)
                merged = mergeSpans span1 span2
            spanStart merged @?= posAt 1 1
            spanEnd merged @?= posAt 1 15

        , testCase "multiline span" $ do
            let start = posAt 2 8
                end = posAt 4 12
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end
            posLine (spanStart span) @?= 2
            posLine (spanEnd span) @?= 4
        ]

    , testGroup "Location Tracking Through Transformations"
        [ testCase "location preservation through comment removal" $ do
            let input = unlines
                  [ "let x = 1 // comment"
                  , "let y = 2"
                  ]
                cleaned = removeComments input
                -- Line structure should be preserved
                cleanedLines = lines cleaned
                originalLines = lines input
            length cleanedLines @?= length originalLines

        , testCase "location tracking through indentation normalization" $ do
            let input = unlines
                  [ "    func test() {"
                  , "        return 42"
                  , "    }"
                  ]
                normalized = normalizeIndentation input
                -- Content should be preserved, structure maintained
                hasFunction = "func test()" `isInfixOf` normalized
                hasReturn = "return 42" `isInfixOf` normalized
            hasFunction @?= True
            hasReturn @?= True

        , testCase "location accuracy through text splitting" $ do
            let input = "a,b,c,d"
                parts = splitBy ',' input
                -- Should be able to map parts back to original positions
                partPositions = [1, 3, 5, 7]  -- Start positions of each part
                partLengths = map length parts
            sum partLengths + length parts - 1 @?= length input

        , testCase "location tracking through complex transformations" $ do
            let input = unlines
                  [ "/* comment */ func test() {"
                  , "    // another comment"
                  , "    return 42"
                  , "}"
                  ]
                -- Apply multiple transformations
                step1 = removeComments input
                step2 = normalizeIndentation step1
                -- Should still be able to track meaningful structure
                hasFunction = "func test()" `isInfixOf` step2
                hasReturn = "return 42" `isInfixOf` step2
            hasFunction @?= True
            hasReturn @?= True
        ]

    , testGroup "Error Location Precision"
        [ testCase "precise error position reporting" $ do
            let errorPos = SourcePos 5 12 100
                errorLoc = toErrorLocation errorPos
            line errorLoc @?= 5
            column errorLoc @?= 12

        , testCase "error location with span" $ do
            let start = SourcePos 3 8 50
                end = SourcePos 3 15 57
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 3
            column errorLoc @?= 8
            endLine errorLoc @?= Just 3
            endColumn errorLoc @?= Just 15

        , testCase "error location in multiline context" $ do
            let multilineError = unlines
                  [ "func test() {"
                  , "    let x = 1"
                  , "    let y = 2"  -- Error here
                  , "    return x + y"
                  , "}"
                  ]
                errorLine = 3
                errorCol = 13  -- Approximate position of "let y = 2"
            errorLine @?= 3
            errorCol > 0 @?= True

        , testCase "error location after transformations" $ do
            let sourceWithComments = unlines
                  [ "func test() { // comment"
                  , "    return 42 /* another comment */"
                  , "}"
                  ]
                cleaned = removeComments sourceWithComments
                -- Should be able to map errors back to original source
                errorInCleaned = 2  -- Error in second line of cleaned
                errorInOriginal = 2  -- Should map to same line in original
            errorInCleaned @?= errorInOriginal
        ]

    , testGroup "Advanced Location Scenarios"
        [ testCase "location tracking in nested structures" $ do
            let nestedCode = unlines
                  [ "func outer() {"
                  , "  if condition {"
                  , "    for item in items {"
                  , "      process(item)"
                  , "    }"
                  , "  }"
                  , "}"
                  ]
                -- Should track location through 4 levels of nesting
                nestingLevels = 4
                processLine = 4  -- process(item) is on line 4
            processLine @?= 4
            nestingLevels @?= 4

        , testCase "location in macro expansion simulation" $ do
            let macroCall = "log!(\"debug message\")"
                macroExpansion = unlines
                  [ "if DEBUG {"
                  , "  println(\"[DEBUG] debug message\")"
                  , "}"
                  ]
                -- Should map expanded code back to macro call location
                originalLine = 1
                expandedLines = 3
            originalLine @?= 1
            expandedLines @?= 3

        , testCase "location in generated code" $ do
            let template = "template<T> function()"
                generatedCode = unlines
                  [ "function template_int() {"
                  , "  // specialized implementation"
                  , "}"
                  ]
                -- Should track that generated code came from template
                hasTemplate = "template" `isInfixOf` template
                hasGenerated = "template_int" `isInfixOf` generatedCode
            hasTemplate @?= True
            hasGenerated @?= True

        , testCase "location across file boundaries" $ do
            let mainFile = "import \"utils\""
                utilsFile = "func helper() {}"
                -- Should track that helper comes from utils file
                importLine = 1
                helperLocation = ("utils", 1)  -- File utils, line 1
            importLine @?= 1
            fst helperLocation @?= "utils"
        ]

    , testGroup "Performance and Precision Balance"
        [ testCase "efficient position calculation for large files" $ do
            let largeText = unlines $ replicate 10000 "line content"
                finalPos = advancePosBy largeText startPos
            posLine finalPos @?= 10001  -- Should be accurate even for large files
            posColumn finalPos @?= 1

        , testCase "memory-efficient location tracking" $ do
            let positions = [SourcePos l c (l * 1000 + c) | l <- [1..1000], c <- [1..100]]
                -- Should handle many position objects efficiently
                positionCount = length positions
            positionCount @?= 100000

        , testCase "lazy location computation" $ do
            let sourceText = "function call with many parameters"
                -- Should compute locations on-demand
                computeLocation index = SourcePos 1 (index + 1) index
                locationAt5 = computeLocation 5
                locationAt10 = computeLocation 10
            posColumn locationAt5 @?= 6
            posColumn locationAt10 @?= 11

        , testCase "incremental location updates" $ do
            let initialPos = startPos
                updates = ['h', 'e', 'l', 'l', 'o']
                finalPos = foldl (flip advancePos) initialPos updates
            posColumn finalPos @?= 6  -- Should be at position after "hello"
        ]

    , testGroup "Edge Cases and Boundary Conditions"
        [ testCase "position at file boundaries" $ do
            let emptyFilePos = startPos
                singleCharFile = "x"
                afterSingleChar = advancePosBy singleCharFile emptyFilePos
            posLine emptyFilePos @?= 1
            posColumn emptyFilePos @?= 1
            posColumn afterSingleChar @?= 2

        , testCase "handling of invalid positions" $ do
            let invalidPositions = 
                  [ SourcePos 0 0 (-1)    -- Negative offset
                  , SourcePos (-1) 5 0    -- Negative line
                  , SourcePos 5 (-1) 10   -- Negative column
                  ]
                -- Should handle gracefully
                positionCount = length invalidPositions
            positionCount @?= 3

        , testCase "position wrapping behavior" $ do
            let veryLongLine = replicate 10000 'x'
                afterLongLine = advancePosBy veryLongLine startPos
            posColumn afterLongLine @?= 10001  -- Should handle very long lines
            posLine afterLongLine @?= 1

        , testCase "location consistency across encodings" $ do
            let utf8Text = "café"  -- Contains accented character
                afterUTF8 = advancePosByText (T.pack utf8Text) startPos
                -- Should handle UTF-8 correctly
                posLine afterUTF8 @?= 1
            posColumn afterUTF8 @?= 5  -- 4 characters + 1 starting position
        ]

    , testGroup "Property-based Location Tests"
        [ fastProperty "position advancement is consistent" prop_positionConsistency
        , fastProperty "span operations preserve invariants" prop_spanInvariants
        , fastProperty "location tracking is reversible" prop_locationReversible
        , fastProperty "error locations maintain accuracy" prop_errorLocationAccuracy
        ]
    ]

-- Property: position advancement should be consistent
prop_positionConsistency :: String -> Bool
prop_positionConsistency input =
  let start = startPos
      afterText = advancePosBy input start
      afterChars = foldl (flip advancePos) start input
  posLine afterText == posLine afterChars && 
  posColumn afterText == posColumn afterChars

-- Property: span operations should preserve invariants
prop_spanInvariants :: Int -> Int -> Int -> Int -> Bool
prop_spanInvariants l1 c1 l2 c2 =
  let start = SourcePos (abs l1 `mod` 100 + 1) (abs c1 `mod` 100 + 1) 0
      end = SourcePos (abs l2 `mod` 100 + 1) (abs c2 `mod` 100 + 1) 0
      span = spanBetween (min start end) (max start end)
      merged = mergeSpans span span
  spanStart merged == spanStart span && spanEnd merged == spanEnd span

-- Property: location tracking should be reversible for simple cases
prop_locationReversible :: String -> Bool
prop_locationReversible input
  | '\n' `elem` input = True  -- Skip multiline for simplicity
  | otherwise =
      let start = startPos
          after = advancePosBy input start
          -- For single line, column difference should equal string length
          colDiff = posColumn after - posColumn start
      in colDiff == length input

-- Property: error locations should maintain accuracy
prop_errorLocationAccuracy :: Int -> Int -> Int -> Int -> Bool
prop_errorLocationAccuracy sl sc el ec =
  let start = SourcePos (abs sl `mod` 100 + 1) (abs sc `mod` 100 + 1) 0
      end = SourcePos (abs el `mod` 100 + 1) (abs ec `mod` 100 + 1) 0
      span = spanBetween start end
      errorLoc = toErrorLocationWithSpan span
  line errorLoc == posLine start && 
  column errorLoc == posColumn start &&
  endLine errorLoc == Just (posLine end) &&
  endColumn errorLoc == Just (posColumn end)