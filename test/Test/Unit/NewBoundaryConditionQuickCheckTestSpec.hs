module Test.Unit.NewBoundaryConditionQuickCheckTestSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAfter, emptySpan, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives)
import Data.Maybe 
                              prop_trim_whitespace_only = forAll (listOf $ elements " \t\n\r") $ \ws ->
  trim                               ws == ""

prop_split_by_empty_string :: Char -> Bool
prop_split_by_empty_string                               delim = splitBy delim "" == [""]

prop_split_by_collapsed_empty_string :: Char -> Bool
prop_split_by_collapsed_empty_string                               delim = splitByCollapsed delim "" == []

prop_remove_comments_empty :: Bool
                              prop_remove_comments_empty = removeLineComments "" == ""

-- SourceLocation
prop_start_pos_positive :: Bool
                              prop_start_pos_positive = 
  let pos = startPos
  in sourceLine pos > 0 && sourceColumn pos > 0
prop_pos_after_empty_char :: Positive Int -> Positive Int -> Bool
prop_pos_after_empty_char (Positive line) (Positive col) =
  let pos = SourcePos line col
                                    newPos = posAfter pos ' '  -- 
  in sourceLine                               newPos == line && sourceColumn                               newPos == col + 1

prop_empty_span_invalid :: Bool
                              prop_empty_span_invalid = not (isValidSpan emptySpan)

prop_pos_at_zero :: Bool
                              prop_pos_at_zero = 
  let pos = SourcePos 0 0
  in sourceLine                               pos == 0 && sourceColumn                               pos == 0

-- Parser
prop_default_directives_nothing :: Bool
                              prop_default_directives_nothing =
  let FileDirectives{..} = defaultFileDirectives
      BlockDirectives{..} = defaultBlockDirectives
  in isNothing fdOwnership && isNothing fdDependentTypes && isNothing fdConstraints &&
isNothing bdOwnership && isNothing bdDependentTypes && isNothing bdConstraints

-- 
prop_trim_unicode :: Property
                              prop_trim_unicode = forAll (listOf $ arbitraryUnicodeChar) $ \s ->
  let trimmed = trim s
  in L.length trimmed <= L.length s

prop_split_by_unicode :: UnicodeChar -> String -> Bool
prop_split_by_unicode (UnicodeChar c)                               s = L.length (splitBy c s) >= 1

prop_large_string_handling :: Positive Int -> Property
prop_large_string_handling (Positive size) = 
  size <=                               1000 ==>  -- 
  let largeString = replicate size 'a'
                                    trimmed = trim largeString
                                    parts = splitBy ',' largeString
  in L.length                               trimmed == size && L.length                               parts == 1

-- 
prop_negative_positions :: Bool
                              prop_negative_positions = 
  let pos = SourcePos (-1) (-1)
  in sourceLine pos < 0 && sourceColumn pos < 0

prop_zero_length_span :: Bool
                              prop_zero_length_span = 
  let pos = SourcePos 1 1
                                    span = SourceSpan pos pos
      in isValidSpan span

-- 
prop_repeated_operations :: Small Int -> String -> Bool
prop_repeated_operations (Small n)                               s = n >= 0 && n <=                               100 ==>  -- 
  let result = L.foldl (\acc _ -> trim acc) s [1..n]
  in property $ L.length result <= L.length s

-- 
tests :: TestTree
tests =   testGroup "Boundary Condition QuickCheck Tests"
  [             testProperty "trim empty string" prop_trim_empty_string
  ,             testProperty "trim whitespace only" prop_trim_whitespace_only
  ,             testProperty "splitBy empty string" prop_split_by_empty_string
  ,             testProperty "splitByCollapsed empty string" prop_split_by_collapsed_empty_string
  ,             testProperty "removeLineComments empty" prop_remove_comments_empty
  ,             testProperty "startPos positive" prop_start_pos_positive
  ,             testProperty "posAfter empty char" prop_pos_after_empty_char
  ,             testProperty "emptySpan invalid" prop_empty_span_invalid
  ,             testProperty "posAt zero" prop_pos_at_zero
  ,             testProperty "default directives nothing" prop_default_directives_nothing
  ,             testProperty "trim unicode" prop_trim_unicode
  ,             testProperty "splitBy unicode" prop_split_by_unicode
  ,             testProperty "large string handling" prop_large_string_handling
  ,             testProperty "negative positions" prop_negative_positions
  ,             testProperty "zero L.length span" prop_zero_length_span
  ,             testProperty "repeated operations" prop_repeated_operations
  ]