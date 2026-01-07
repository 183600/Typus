module Test.Unit.SourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
import Data.Monoid ((<>))

-- 测试SourcePos的属性
prop_sourcepos_monoid :: Int -> Int -> Int -> Int -> Property
prop_sourcepos_monoid l1 c1 l2 c2 = 
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
      combined = pos1 <> pos2
  in sourceLine combined >= max l1 l2 &&
     sourceColumn combined >= max c1 c2

prop_sourcepos_ordering :: Int -> Int -> Int -> Int -> Property
prop_sourcepos_ordering l1 c1 l2 c2 = 
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
  in if l1 < l2 || (l1 == l2 && c1 < c2)
     then pos1 < pos2
     else pos1 >= pos2

-- 测试startPos的属性
prop_startpos_consistency :: Property
prop_startpos_consistency = 
  let pos = startPos
  in sourceLine pos === 1 && sourceColumn pos === 1

-- 测试posAfter的属性
prop_posAfter_newline :: Int -> Int -> Property
prop_posAfter_newline line col = 
  let pos = SourcePos line col
      afterNewline = posAfter '\n' pos
  in sourceLine afterNewline === line + 1 &&
     sourceColumn afterNewline === 1

prop_posAfter_regular_char :: Int -> Int -> Char -> Property
prop_posAfter_regular_char line col c = 
  c /= '\n' ==> 
  let pos = SourcePos line col
      afterChar = posAfter c pos
  in sourceLine afterChar === line &&
     sourceColumn afterChar === col + 1

prop_posAfter_tab :: Int -> Int -> Property
prop_posAfter_tab line col = 
  let pos = SourcePos line col
      afterTab = posAfter '\t' pos
  in sourceLine afterTab === line &&
     sourceColumn afterTab >= col + 1

-- 测试SourceSpan的属性
prop_sourcespan_merge :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_sourcespan_merge l1 c1 l2 c2 l3 c3 = 
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
      pos3 = SourcePos l3 c3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
  in spanStart merged === spanStart span1 &&
     spanEnd merged === spanEnd span2

prop_sourcespan_validity :: Int -> Int -> Int -> Int -> Property
prop_sourcespan_validity l1 c1 l2 c2 = 
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
      span = spanBetween pos1 pos2
  in isValidSpan span === (pos1 <= pos2)

-- 测试emptySpan的属性
prop_emptyspan_consistency :: Property
prop_emptyspan_consistency = 
  let span = emptySpan
  in not (isValidSpan span)

-- 测试locatedAt的属性
prop_locatedat_preserves_value :: String -> Int -> Int -> Property
prop_locatedat_preserves_value s line col = 
  let pos = SourcePos line col
      located = locatedAt pos s
  in locatedValue located === s

prop_locatedat_sets_position :: String -> Int -> Int -> Property
prop_locatedat_sets_position s line col = 
  let pos = SourcePos line col
      located = locatedAt pos s
  in locatedPos located === pos

-- 测试mapLocated的属性
prop_maplocated_preserves_location :: String -> Int -> Int -> Property
prop_maplocated_preserves_location s line col = 
  let pos = SourcePos line col
      located = locatedAt pos s
      mapped = mapLocated reverse located
  in locatedPos mapped === locatedPos located

prop_maplocated_applies_function :: String -> Int -> Int -> Property
prop_maplocated_applies_function s line col = 
  let pos = SourcePos line col
      located = locatedAt pos s
      mapped = mapLocated reverse located
  in locatedValue mapped === reverse s

-- 测试LocationTracker的属性
prop_locationtracker_monad_laws :: Int -> Int -> Property
prop_locationtracker_monad_laws startLine startCol = 
  let startPos' = SourcePos startLine startCol
      action1 = getCurrentPos
      action2 = setCurrentPos startPos' >> getCurrentPos
      result1 = runLocationTracker action1 startPos'
      result2 = runLocationTracker action2 startPos'
  in snd result2 === startPos'

-- 测试advancePos的属性
prop_advancepos_string_length :: String -> Int -> Int -> Property
prop_advancepos_string_length s line col = 
  let pos = SourcePos line col
      advanced = advancePos s pos
  in sourceLine advanced >= line &&
     sourceColumn advanced >= col

prop_advancepos_empty_string :: Int -> Int -> Property
prop_advancepos_empty_string line col = 
  let pos = SourcePos line col
      advanced = advancePos "" pos
  in advanced === pos

-- 测试spanFrom和spanTo的属性
prop_spanfrom_to_consistency :: Int -> Int -> Int -> Int -> Property
prop_spanfrom_to_consistency l1 c1 l2 c2 = 
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
      span = spanFrom pos1
      finalSpan = spanTo pos2 span
  in spanStart finalSpan === pos1 &&
     spanEnd finalSpan === pos2

tests :: TestTree
tests = testGroup "SourceLocation Math Tests"
  [ testProperty "SourcePos monoid" prop_sourcepos_monoid
  , testProperty "SourcePos ordering" prop_sourcepos_ordering
  , testProperty "startPos consistency" prop_startpos_consistency
  , testProperty "posAfter newline" prop_posAfter_newline
  , testProperty "posAfter regular char" prop_posAfter_regular_char
  , testProperty "posAfter tab" prop_posAfter_tab
  , testProperty "SourceSpan merge" prop_sourcespan_merge
  , testProperty "SourceSpan validity" prop_sourcespan_validity
  , testProperty "emptySpan consistency" prop_emptyspan_consistency
  , testProperty "locatedAt preserves value" prop_locatedat_preserves_value
  , testProperty "locatedAt sets position" prop_locatedat_sets_position
  , testProperty "mapLocated preserves location" prop_maplocated_preserves_location
  , testProperty "mapLocated applies function" prop_maplocated_applies_function
  , testProperty "LocationTracker monad laws" prop_locationtracker_monad_laws
  , testProperty "advancePos string length" prop_advancepos_string_length
  , testProperty "advancePos empty string" prop_advancepos_empty_string
  , testProperty "spanFrom to consistency" prop_spanfrom_to_consistency
  ]