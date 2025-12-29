module Test.Unit.NewCabalSourceLocationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, Positive(..))
import Data.Maybe (isJust, isNothing)

import TestSupport.QuickCheck (fastProperty)
import SourceLocation

-- | QuickCheck tests for SourceLocation module covering position and span operations
tests :: TestTree
tests =
  testGroup "New Cabal SourceLocation QuickCheck Tests"
    [ testGroup "SourcePos properties"
        [ fastProperty "startPos is always at line 1, column 1" prop_startPosFixed
        , fastProperty "posAfter advances column for non-newline characters" prop_posAfterAdvancesColumn
        , fastProperty "posAfter advances line for newline characters" prop_posAfterAdvancesLine
        , fastProperty "posAt creates positions with valid coordinates" prop_posAtValid
        , fastProperty "posAtLineCol creates consistent positions" prop_posAtLineColConsistent
        ]
    
    , testGroup "SourceSpan properties"
        [ fastProperty "emptySpan has zero length" prop_emptySpanZeroLength
        , fastProperty "spanFrom creates span starting at position" prop_spanFromStart
        , fastProperty "spanTo creates span ending at position" prop_spanToEnd
        , fastProperty "spanBetween creates span that encompasses both positions" prop_spanBetweenEncompasses
        , fastProperty "mergeSpans creates span that encompasses both spans" prop_mergeSpansEncompasses
        , fastProperty "isValidSpan correctly identifies valid spans" prop_isValidSpanCorrect
        ]
    
    , testGroup "Located properties"
        [ fastProperty "locatedAt creates located value with correct position" prop_locatedAtPosition
        , fastProperty "locatedWithSpan creates located value with correct span" prop_locatedWithSpan
        , fastProperty "locatedValue extracts the original value" prop_locatedValueExtracts
        , fastProperty "locatedSpan extracts the correct span" prop_locatedSpanExtracts
        , fastProperty "mapLocated preserves location" prop_mapLocatedPreservesLocation
        ]
    
    , testGroup "Position advancement properties"
        [ fastProperty "advancePos by zero character returns same position" prop_advancePosZero
        , fastProperty "advancePos by newline advances line, resets column" prop_advancePosNewline
        , fastProperty "advancePosBy multiple characters accumulates correctly" prop_advancePosByAccumulates
        ]
    
    , testGroup "Edge cases and robustness"
        [ testCase "startPos has line 1 and column 1" $ do
            startPos @?= SourcePos 1 1
            
        , testCase "emptySpan has start and end at startPos" $ do
            emptySpan @?= SourceSpan startPos startPos
        ]
    ]

-- | Property: startPos is always at line 1, column 1
prop_startPosFixed :: Int -> Int -> Bool
prop_startPosFixed _ _ =
  let SourcePos line col = startPos
  in line == 1 && col == 1

-- | Property: posAfter advances column for non-newline characters
prop_posAfterAdvancesColumn :: SourcePos -> Char -> Bool
prop_posAfterAdvancesColumn pos ch
  | ch /= '\n' = 
      let SourcePos line col = pos
          SourcePos newLine newCol = posAfter pos ch
      in newLine == line && newCol == col + 1
  | otherwise = True -- Newline case handled by separate property

-- | Property: posAfter advances line for newline characters
prop_posAfterAdvancesLine :: SourcePos -> Bool
prop_posAfterAdvancesLine pos =
  let SourcePos line col = pos
      SourcePos newLine newCol = posAfter pos '\n'
  in newLine == line + 1 && newCol == 1

-- | Property: posAt creates positions with valid coordinates
prop_posAtValid :: Positive Int -> Positive Int -> Bool
prop_posAtValid (Positive line) (Positive col) =
  let SourcePos l c = posAt line col
  in l == line && c == col

-- | Property: posAtLineCol creates consistent positions
prop_posAtLineColConsistent :: Positive Int -> Positive Int -> Bool
prop_posAtLineColConsistent (Positive line) (Positive col) =
  let pos1 = posAt line col
      pos2 = posAtLineCol line col
  in pos1 == pos2

-- | Property: emptySpan has zero length
prop_emptySpanZeroLength :: Bool
prop_emptySpanZeroLength =
  let SourcePos startLine startCol = spanStart emptySpan
      SourcePos endLine endCol = spanEnd emptySpan
  in startLine == endLine && startCol == endCol

-- | Property: spanFrom creates span starting at position
prop_spanFromStart :: SourcePos -> String -> Bool
prop_spanFromStart pos text =
  let span = spanFrom pos text
      SourcePos startLine startCol = spanStart span
  in startLine == sourcePosLine pos && startCol == sourcePosColumn pos

-- | Property: spanTo creates span ending at position
prop_spanToEnd :: SourcePos -> String -> Bool
prop_spanToEnd pos text =
  let span = spanTo pos text
      SourcePos endLine endCol = spanEnd span
  in endLine == sourcePosLine pos && endCol == sourcePosColumn pos

-- | Property: spanBetween creates span that encompasses both positions
prop_spanBetweenEncompasses :: SourcePos -> SourcePos -> Bool
prop_spanBetweenEncompasses pos1 pos2 =
  let span = spanBetween pos1 pos2
      SourcePos startLine startCol = spanStart span
      SourcePos endLine endCol = spanEnd span
      SourcePos line1 col1 = pos1
      SourcePos line2 col2 = pos2
  in (startLine <= line1 || (startLine == line1 && startCol <= col1)) &&
     (endLine >= line2 || (endLine == line2 && endCol >= col2))

-- | Property: mergeSpans creates span that encompasses both spans
prop_mergeSpansEncompasses :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpansEncompasses span1 span2 =
  let merged = mergeSpans span1 span2
      SourcePos startLine startCol = spanStart merged
      SourcePos endLine endCol = spanEnd merged
      SourcePos start1Line start1Col = spanStart span1
      SourcePos end1Line end1Col = spanEnd span1
      SourcePos start2Line start2Col = spanStart span2
      SourcePos end2Line end2Col = spanEnd span2
  in (startLine <= start1Line || (startLine == start1Line && startCol <= start1Col)) &&
     (endLine >= end1Line || (endLine == end1Line && endCol >= end1Col)) &&
     (startLine <= start2Line || (startLine == start2Line && startCol <= start2Col)) &&
     (endLine >= end2Line || (endLine == end2Line && endCol >= end2Col))

-- | Property: isValidSpan correctly identifies valid spans
prop_isValidSpanCorrect :: SourceSpan -> SourceSpan -> Bool
prop_isValidSpanCorrect span1 span2 =
  let valid1 = isValidSpan span1
      valid2 = isValidSpan span2
      SourcePos start1Line start1Col = spanStart span1
      SourcePos end1Line end1Col = spanEnd span1
      SourcePos start2Line start2Col = spanStart span2
      SourcePos end2Line end2Col = spanEnd span2
      span1Valid = start1Line < end1Line || (start1Line == end1Line && start1Col <= end1Col)
      span2Valid = start2Line < end2Line || (start2Line == end2Line && start2Col <= end2Col)
  in valid1 == span1Valid && valid2 == span2Valid

-- | Property: locatedAt creates located value with correct position
prop_locatedAtPosition :: Int -> String -> Bool
prop_locatedAtPosition col value =
  let pos = posAt 1 col
      located = locatedAt pos value
  in locatedPos located == pos

-- | Property: locatedWithSpan creates located value with correct span
prop_locatedWithSpan :: SourceSpan -> String -> Bool
prop_locatedWithSpan span value =
  let located = locatedWithSpan span value
  in locatedSpan located == span

-- | Property: locatedValue extracts the original value
prop_locatedValueExtracts :: String -> Bool
prop_locatedValueExtracts value =
  let located = locatedAt startPos value
  in locatedValue located == value

-- | Property: locatedSpan extracts the correct span
prop_locatedSpanExtracts :: SourceSpan -> String -> Bool
prop_locatedSpanExtracts span value =
  let located = locatedWithSpan span value
  in locatedSpan located == span

-- | Property: mapLocated preserves location
prop_mapLocatedPreservesLocation :: SourcePos -> String -> Bool
prop_mapLocatedPreservesLocation pos value =
  let located = locatedAt pos value
      mapped = mapLocated length located
  in locatedPos mapped == locatedPos located && 
      locatedSpan mapped == locatedSpan located

-- | Property: advancePos by zero character returns same position
prop_advancePosZero :: SourcePos -> Bool
prop_advancePosZero pos =
  advancePos pos '\0' == pos

-- | Property: advancePos by newline advances line, resets column
prop_advancePosNewline :: SourcePos -> Bool
prop_advancePosNewline pos =
  let SourcePos line col = pos
      SourcePos newLine newCol = advancePos pos '\n'
  in newLine == line + 1 && newCol == 1

-- | Property: advancePosBy multiple characters accumulates correctly
prop_advancePosByAccumulates :: SourcePos -> String -> Bool
prop_advancePosByAccumulates pos text =
  let singleAdvances = foldl advancePos pos text
      multiAdvance = advancePosBy pos text
  in singleAdvances == multiAdvance

-- Helper functions for accessing SourcePos fields
sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col) = col

-- Helper functions for accessing SourceSpan fields
spanStart :: SourceSpan -> SourcePos
spanStart (SourceSpan start _) = start

spanEnd :: SourceSpan -> SourcePos
spanEnd (SourceSpan _ end) = end