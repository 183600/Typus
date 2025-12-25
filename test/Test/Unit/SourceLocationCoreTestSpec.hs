module Test.Unit.SourceLocationCoreTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, (==>))
import qualified Test.Tasty.QuickCheck as QC

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> QC.choose (1, 100) <*> QC.choose (1, 100) <*> QC.choose (0, 1000)

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- QC.choose (0, 50)
    let end = start { posOffset = posOffset start + endOffset
                    , posColumn = posColumn start + endOffset
                    }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "SourceLocation Core Tests"
    [ testGroup "SourcePos"
        [ testCase "startPos has correct initial values" $ do
            posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0

        , testCase "posAt creates position at specified line and column" $ do
            let pos = posAt 5 10
            posLine pos @?= 5
            posColumn pos @?= 10
            posOffset pos @?= 0

        , testCase "posAtLineCol creates position with all fields" $ do
            let pos = posAtLineCol 3 7 42
            posLine pos @?= 3
            posColumn pos @?= 7
            posOffset pos @?= 42

        , testCase "posAfter handles newline correctly" $ do
            let start = posAt 1 5
                after = posAfter '\n' start
            posLine after @?= 2
            posColumn after @?= 1
            posOffset after @?= posOffset start + 1

        , testCase "posAfter handles tab correctly (8-space tab width)" $ do
            let start = posAt 1 3
                after = posAfter '\t' start
            posLine after @?= 1
            posColumn after @?= 9  -- Next tab stop after column 3
            posOffset after @?= posOffset start + 1

        , testCase "posAfter handles regular character correctly" $ do
            let start = posAt 2 10
                after = posAfter 'x' start
            posLine after @?= 2
            posColumn after @?= 11
            posOffset after @?= posOffset start + 1
        ]

    , testGroup "SourceSpan"
        [ testCase "emptySpan creates span with same start and end" $ do
            let pos = posAt 3 5
                span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanFrom creates empty span at position" $ do
            let pos = posAt 2 8
                span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanTo creates empty span at position" $ do
            let pos = posAt 4 1
                span = spanTo pos
            spanStart span @?= pos
            spanEnd span @?= pos

        , testCase "spanBetween creates span between two positions" $ do
            let start = posAt 1 5
                end = posAt 2 10
                span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

        , testCase "mergeSpans combines spans correctly" $ do
            let span1 = spanBetween (posAt 1 5) (posAt 2 3)
                span2 = spanBetween (posAt 1 8) (posAt 3 1)
                merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2

        , testCase "isValidSpan returns true for valid spans" $ do
            let span = spanBetween (posAt 1 1) (posAt 1 5)
            assertBool "Span should be valid" $ isValidSpan span

        , testCase "isValidSpan returns false for invalid spans" $ do
            let span = spanBetween (posAt 2 10) (posAt 1 5)
            assertBool "Span should be invalid" $ not $ isValidSpan span
        ]

    , testGroup "Located"
        [ testCase "locatedAt creates located value with empty span" $ do
            let value = "test"
                pos = posAt 3 7
                located = locatedAt value pos
            locatedValue located @?= value
            locatedPos located @?= pos
            spanStart (locatedSpan located) @?= pos
            spanEnd (locatedSpan located) @?= pos

        , testCase "locatedWithSpan creates located value with custom span" $ do
            let value = 42
                pos = posAt 1 3
                span = spanBetween pos (posAt 2 1)
                located = locatedWithSpan value pos span
            locatedValue located @?= value
            locatedPos located @?= pos
            locatedSpan located @?= span

        , testCase "mapLocated transforms the value while preserving location" $ do
            let original = locatedAt "hello" (posAt 2 5)
                transformed = mapLocated length original
            locatedValue transformed @?= 5
            locatedPos transformed @?= locatedPos original
            locatedSpan transformed @?= locatedSpan original
        ]

    , testGroup "Position Advancement"
        [ testCase "advancePos advances by one character" $ do
            let start = posAt 1 5
                after = advancePos 'x' start
            posLine after @?= 1
            posColumn after @?= 6
            posOffset after @?= posOffset start + 1

        , testCase "advancePosBy advances by multiple characters" $ do
            let start = posAt 1 3
                after = advancePosBy "hello" start
            posLine after @?= 1
            posColumn after @?= 8
            posOffset after @?= posOffset start + 5

        , testCase "advancePosByText handles multiline text" $ do
            let start = posAt 2 5
                text = "hello\nworld"
                after = advancePosByText text start
            posLine after @?= 3
            posColumn after @?= 5  -- "world" length
            posOffset after @?= posOffset start + length text

        , testCase "advancePosByLine advances by lines" $ do
            let start = posAt 3 10
                after = advancePosByLine 2 start
            posLine after @?= 5
            posColumn after @?= 1
            posOffset after @?= posOffset start + 2  -- Simplified assumption
        ]

    , testGroup "QuickCheck Properties"
        [ testProperty "posAfter newline increments line and resets column" $
            \pos -> posColumn pos > 0 ==>
              let after = posAfter '\n' pos
              in posLine after == posLine pos + 1 &&
                 posColumn after == 1 &&
                 posOffset after == posOffset pos + 1

        , testProperty "mergeSpans is commutative" $
            \span1 span2 -> 
              let merged1 = mergeSpans span1 span2
                  merged2 = mergeSpans span2 span1
              in merged1 == merged2

        , testProperty "mergeSpans is associative" $
            \span1 span2 span3 ->
              let merged1 = mergeSpans (mergeSpans span1 span2) span3
                  merged2 = mergeSpans span1 (mergeSpans span2 span3)
              in merged1 == merged2

        , testProperty "spanFrom posAt creates valid span" $
            \line col ->
              let pos = posAt line col
                  span = spanFrom pos
              in isValidSpan span &&
                 spanStart span == spanEnd span

        , testProperty "mapLocated preserves position information" $
            \value pos ->
              let original = locatedAt value pos
                  transformed = mapLocated (*2) original
              in locatedPos transformed == locatedPos original &&
                 locatedSpan transformed == locatedSpan original

        , testProperty "advancePosBy advances offset by string length" $
            \pos str ->
              let after = advancePosBy str pos
              in posOffset after == posOffset pos + length str

        , testProperty "locatedValue extraction works correctly" $
            \value pos ->
              let located = locatedAt value pos
              in locatedValue located == value
        ]
    ]