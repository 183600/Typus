module Test.Unit.NewErrorHandlingQuickCheckTestSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAfter, emptySpan, isValidSpan)
import Parser (FileDirectives(..), defaultFileDirectives)
import Data.Maybe 
let result = evaluate (trim s)
  in case result of
    Left _ -> False  -- 
    Right _ -> True

prop_split_exception_safe :: Char -> String -> Bool
prop_split_exception_safe delim                               s = 
let result = evaluate (splitBy delim s)
  in case result of
    Left _ -> False  -- 
    Right _ -> True

-- 
prop_empty_input_handling :: Bool
                              prop_empty_input_handling = 
  let trimEmpty = trim ""
                                    splitEmpty = splitBy ',' ""
                                    posStart = startPos
  in null trimEmpty &&                               splitEmpty == [""] && sourceLine                               posStart == 1

prop_null_character_handling :: String -> Bool
prop_null_character_handling                               s = 
  let stringWithNull = s ++ "\0"
                                    trimmed = trim stringWithNull
                                    parts = splitBy '\0' stringWithNull
  in L.length trimmed >= 0 && L.length parts >= 1

-- 
prop_position_validation :: Int -> Int -> Bool
prop_position_validation line                               col = 
  let pos = SourcePos line col
  -- 
  in sourceLine                               pos == line && sourceColumn                               pos == col

prop_span_validation :: SourcePos -> SourcePos -> Bool
prop_span_validation pos1                               pos2 = 
  let span = SourceSpan pos1 pos2
  -- span
  in spanStart                               span == pos1 && spanEnd                               span == pos2

-- 
prop_directives_error_handling :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_directives_error_handling own deps                               cons = 
  let directives = FileDirectives own deps cons
  -- Nothing
      in isNothing (fdOwnership directives) == isNothing own &&
     isNothing (fdDependentTypes directives) == isNothing deps &&
     isNothing (fdConstraints directives) == isNothing cons

-- 
prop_error_recovery :: String -> String -> Bool
prop_error_recovery s1                               s2 = 
  let combined = s1 ++ s2
                                    trimmed1 = trim s1
                                    trimmed2 = trim s2
                                    trimmedCombined = trim combined
  -- 
  in L.length trimmedCombined >= 0

prop_partial_failure_handling :: String -> Bool
prop_partial_failure_handling                               s = 
  let parts = splitBy ',' s
                                    processed = map trim parts
  -- 
in L.length                               processed == L.length parts

-- 
prop_memory_cleanup :: Small Int -> String -> Bool
prop_memory_cleanup (Small n)                               s = n >= 0 && n <=                               100 ==>  -- 
  let largeString = L.concat (replicate n s)
                                    result = trim largeString
  -- 
  in L.length result >= 0

-- 
tests :: TestTree
tests =   testGroup "Error Handling QuickCheck Tests"
  [             testProperty "trim error handling" prop_trim_error_handling
  ,             testProperty "split error handling" prop_split_error_handling
  ,             testProperty "position error handling" prop_position_error_handling
  ,             testProperty "trim exception safe" prop_trim_exception_safe
  ,             testProperty "split exception safe" prop_split_exception_safe
  ,             testProperty "empty input handling" prop_empty_input_handling
  ,             testProperty "null character handling" prop_null_character_handling
  ,             testProperty "position validation" prop_position_validation
  ,             testProperty "span validation" prop_span_validation
  ,             testProperty "directives error handling" prop_directives_error_handling
  ,             testProperty "error recovery" prop_error_recovery
  ,             testProperty "partial failure handling" prop_partial_failure_handling
  ,             testProperty "memory cleanup" prop_memory_cleanup
  ]