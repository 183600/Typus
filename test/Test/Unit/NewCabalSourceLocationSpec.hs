module Test.Unit.NewCabalSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck ((===), Property, counterexample, forAll)

import TestSupport.QuickCheck (fastProperty)
import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , advancePosBy
  , advancePosByText
  , mergeSpans
  , spanBetween
  , emptySpan
  , locatedAt
  , isValidSpan
  , toErrorLocation
  , toErrorLocationWithSpan
  )
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T
import Data.Ord (comparing)

-- | Additional comprehensive tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "NewCabal SourceLocation Tests"
    [ testGroup "Position arithmetic edge cases"
        [ testCase "posAfter handles carriage return correctly" $ do
            let initial = SourcePos 1 5 4
                next = posAfter '\r' initial
            next @?= SourcePos 1 6 5

        , testCase "advancePosByText handles empty text" $ do
            let result = advancePosByText T.empty startPos
            result @?= startPos

        , testCase "advancePosByText handles tabs at different positions" $ do
            let pos1 = SourcePos 1 3 2
                pos2 = advancePosBy "\t" pos1
            pos2 @?= SourcePos 1 9 3  -- Should jump to next tab stop (position 9)

        , testCase "advancePosByText handles mixed newlines and tabs" $ do
            let result = advancePosByText (T.pack "a\t\nb\tc") startPos
            result @?= SourcePos 2 10 6
        ]

    , testGroup "Span operations"
        [ testCase "mergeSpans handles identical spans" $ do
            let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                result = mergeSpans span span
            result @?= span

        , testCase "mergeSpans handles nested spans" $ do
            let outer = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
                inner = SourceSpan (SourcePos 1 3 2) (SourcePos 1 7 6)
                result = mergeSpans outer inner
            result @?= outer

        , testCase "spanBetween creates valid span when start <= end" $ do
            let start = SourcePos 2 3 10
                end = SourcePos 4 5 20
                span = spanBetween start end
            span @?= SourceSpan start end
            assertBool "span should be valid" $ isValidSpan span
        ]

    , testGroup "Located values"
        [ testCase "locatedAt creates zero-length span" $ do
            let pos = SourcePos 5 10 45
                located = locatedAt pos "test"
            locatedSpan located @?= SourceSpan pos pos

        , testCase "Located values maintain order" $ do
            let pos1 = SourcePos 1 1 0
                pos2 = SourcePos 1 2 1
                loc1 = locatedAt pos1 "first"
                loc2 = locatedAt pos2 "second"
            assertBool "locations should be ordered" $ loc1 < loc2
        ]

    , testGroup "Error location conversion"
        [ testCase "toErrorLocation handles large line numbers" $ do
            let pos = SourcePos 999999 1000 50000
                errLoc = toErrorLocation pos
            line errLoc @?= 999999
            column errLoc @?= 1000

        , testCase "toErrorLocationWithSpan handles multi-line spans" $ do
            let span = SourceSpan (SourcePos 1 5 4) (SourcePos 3 2 20)
                errLoc = toErrorLocationWithSpan span
            line errLoc @?= 1
            column errLoc @?= 5
            endLine errLoc @?= Just 3
            endColumn errLoc @?= Just 2
        ]

    , testGroup "QuickCheck property tests"
        [ fastProperty "posAfter advances offset by 1" prop_posAfterAdvancesOffset
        , fastProperty "mergeSpans is commutative" prop_mergeSpansCommutative
        , fastProperty "mergeSpans is associative" prop_mergeSpansAssociative
        , fastProperty "spanBetween start <= end implies valid span" prop_spanBetweenValid
        , fastProperty "advancePosByText is consistent with advancePosBy" prop_advancePosConsistency
        ]
    ]

-- Property: posAfter should always advance offset by exactly 1
prop_posAfterAdvancesOffset :: Char -> SourcePos -> Property
prop_posAfterAdvancesOffset c pos =
  let result = posAfter c pos
  in counterexample ("pos: " ++ show pos ++ ", char: " ++ show c ++ ", result: " ++ show result) $
     posOffset result === posOffset pos + 1

-- Property: mergeSpans should be commutative
prop_mergeSpansCommutative :: SourceSpan -> SourceSpan -> Property
prop_mergeSpansCommutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property: mergeSpans should be associative
prop_mergeSpansAssociative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_mergeSpansAssociative span1 span2 span3 =
  let merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in merged1 === merged2

-- Property: spanBetween should create valid span when start <= end
prop_spanBetweenValid :: SourcePos -> SourcePos -> Property
prop_spanBetweenValid start end =
  let span = spanBetween start end
      valid = isValidSpan span
  in counterexample ("start: " ++ show start ++ ", end: " ++ show end ++ ", span: " ++ show span) $
     if start <= end then valid else True  -- Only check when start <= end

-- Property: advancePosByText should be consistent with advancePosBy
prop_advancePosConsistency :: String -> SourcePos -> Property
prop_advancePosConsistency text pos =
  let byText = advancePosByText (T.pack text) pos
      byString = advancePosBy text pos
  in byText === byString