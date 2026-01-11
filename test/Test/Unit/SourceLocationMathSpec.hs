module Test.Unit.SourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))

-- Test cases for SourcePos
testSourcePos :: TestTree
testSourcePos = testGroup "SourcePos tests"
  [ testCase "startPos has correct values" $
      startPos @?= SourcePos 1 1 0
  , testCase "posAt creates position at line and column" $
      posAt 5 10 @?= SourcePos 5 10 0
  , testCase "posAtLineCol creates position with offset" $
      posAtLineCol 3 7 15 @?= SourcePos 3 7 15
  , testCase "posAfter handles newline" $
      posAfter '\n' (SourcePos 1 5 4) @?= SourcePos 2 1 5
  , testCase "posAfter handles tab" $
      posAfter '\t' (SourcePos 1 3 2) @?= SourcePos 1 9 3
  , testCase "posAfter handles regular character" $
      posAfter 'a' (SourcePos 1 5 4) @?= SourcePos 1 6 5
  ]

-- Test cases for SourceSpan
testSourceSpan :: TestTree
testSourceSpan = testGroup "SourceSpan tests"
  [ testCase "emptySpan creates span at position" $
      let pos = posAt 2 3
          span = emptySpan pos
      in span @?= SourceSpan pos pos
  , testCase "spanFrom creates span starting at position" $
      let pos = posAt 2 3
          span = spanFrom pos
      in span @?= SourceSpan pos pos
  , testCase "spanTo creates span ending at position" $
      let pos = posAt 2 3
          span = spanTo pos
      in span @?= SourceSpan pos pos
  , testCase "spanBetween creates span between positions" $
      let start = posAt 1 1
          end = posAt 2 5
          span = spanBetween start end
      in span @?= SourceSpan start end
  , testCase "mergeSpans creates span covering both" $
      let span1 = spanBetween (posAt 1 1) (posAt 1 10)
          span2 = spanBetween (posAt 2 1) (posAt 2 5)
          merged = mergeSpans span1 span2
      in merged @?= SourceSpan (posAt 1 1) (posAt 2 5)
  , testCase "isValidSpan checks span validity" $
      let validSpan = spanBetween (posAt 1 1) (posAt 1 10)
          invalidSpan = spanBetween (posAt 1 10) (posAt 1 1)
      in do
        isValidSpan validSpan @?= True
        isValidSpan invalidSpan @?= False
  ]

-- Test cases for Located values
testLocated :: TestTree
testLocated = testGroup "Located tests"
  [ testCase "locatedAt creates located value" $
      let pos = posAt 2 3
          value = "test"
          located = locatedAt pos value
      in located @?= Located value pos (SourceSpan pos pos)
  , testCase "locatedWithSpan creates located value with span" $
      let span = spanBetween (posAt 1 1) (posAt 1 5)
          value = "test"
          located = locatedWithSpan span value
      in located @?= Located value (posAt 1 1) span
  , testCase "locatedValue extracts value" $
      let located = locatedAt (posAt 1 1) "test"
      in locatedValue located @?= "test"
  , testCase "locatedSpan extracts span" $
      let span = spanBetween (posAt 1 1) (posAt 1 5)
          located = locatedWithSpan span "test"
      in locatedSpan located @?= span
  , testCase "locatedPos extracts start position" $
      let span = spanBetween (posAt 2 3) (posAt 2 7)
          located = locatedWithSpan span "test"
      in locatedPos located @?= posAt 2 3
  , testCase "mapLocated applies function to value" $
      let located = locatedAt (posAt 1 1) "hello"
          mapped = mapLocated length located
      in locatedValue mapped @?= 5
  ]

-- Test cases for position advancement
testPositionAdvancement :: TestTree
testPositionAdvancement = testGroup "Position advancement tests"
  [ testCase "advancePosBy advances by multiple characters" $
      let start = posAt 1 1
          advanced = advancePosBy "abc" start
      in advanced @?= posAt 1 4
  , testCase "advancePosBy handles newline in string" $
      let start = posAt 1 1
          advanced = advancePosBy "a\nb" start
      in advanced @?= posAt 2 2
  , testCase "advancePosByText advances by text" $
      let start = posAt 1 1
          text = T.pack "hello"
          advanced = advancePosByText text start
      in advanced @?= posAt 1 6
  , testCase "advancePosByLine advances by lines" $
      let start = posAt 1 5
          advanced = advancePosByLine 3 start
      in advanced @?= posAt 4 1
  ]

-- Test cases for error location conversion
testErrorLocationConversion :: TestTree
testErrorLocationConversion = testGroup "Error location conversion tests"
  [ testCase "toErrorLocation converts position" $
      let pos = posAt 5 10
          errLoc = toErrorLocation pos
      in do
        filePath errLoc @?= Nothing
        line errLoc @?= 5
        column errLoc @?= 10
        endLine errLoc @?= Nothing
        endColumn errLoc @?= Nothing
  , testCase "toErrorLocationWithSpan converts span with range" $
      let span = spanBetween (posAt 3 5) (posAt 4 10)
          errLoc = toErrorLocationWithSpan span
      in do
        filePath errLoc @?= Nothing
        line errLoc @?= 3
        column errLoc @?= 5
        endLine errLoc @?= Just 4
        endColumn errLoc @?= Just 10
  ]

-- Test cases for position arithmetic
testPositionArithmetic :: TestTree
testPositionArithmetic = testGroup "Position arithmetic tests"
  [ testCase "tab advances to next tab stop" $
      let pos = posAt 1 3
          afterTab = posAfter '\t' pos
      in posColumn afterTab @?= 9
  , testCase "tab at column 8 advances to column 9" $
      let pos = posAt 1 8
          afterTab = posAfter '\t' pos
      in posColumn afterTab @?= 9
  , testCase "tab at column 9 advances to column 17" $
      let pos = posAt 1 9
          afterTab = posAfter '\t' pos
      in posColumn afterTab @?= 17
  , testCase "multiple characters advance correctly" $
      let start = posAt 1 1
          afterHello = advancePosBy "hello" start
      in afterHello @?= posAt 1 6
  ]

-- QuickCheck properties
prop_posAfter_offset_increases :: Char -> SourcePos -> Property
prop_posAfter_offset_increases c pos = 
  let newPos = posAfter c pos
  in posOffset newPos >= posOffset pos

prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid start end = 
  let span = spanBetween start end'
      end' = if start > end then start else end
  in isValidSpan span

prop_mergeSpans_contains_both :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_contains_both span1 span2 = 
  let merged = mergeSpans span1 span2
  in spanStart merged <= spanStart span1 && 
     spanEnd merged >= spanEnd span1 &&
     spanStart merged <= spanStart span2 && 
     spanEnd merged >= spanEnd span2

prop_advancePosBy_sum_of_advances :: String -> SourcePos -> Property
prop_advancePosBy_sum_of_advances s pos = 
  let advanced = advancePosBy s pos
      manualAdvanced = foldl (flip posAfter) pos s
  in advanced == manualAdvanced

prop_locatedAt_span_is_empty :: SourcePos -> String -> Property
prop_locatedAt_span_is_empty pos value = 
  let located = locatedAt pos value
      span = locSpan located
  in spanStart span == spanEnd span

tests :: TestTree
tests = testGroup "SourceLocation Math Tests"
  [ testSourcePos
  , testSourceSpan
  , testLocated
  , testPositionAdvancement
  , testErrorLocationConversion
  , testPositionArithmetic
  , testProperty "posAfter offset increases" prop_posAfter_offset_increases
  , testProperty "spanBetween creates valid span" prop_spanBetween_valid
  , testProperty "mergeSpans contains both spans" prop_mergeSpans_contains_both
  , testProperty "advancePosBy sum of advances" prop_advancePosBy_sum_of_advances
  , testProperty "locatedAt creates empty span" prop_locatedAt_span_is_empty
  ]