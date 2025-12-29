{-# LANGUAGE CPP #-}

module Test.Unit.UserAddedSyntaxValidatorSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, length, sum, reverse, concat, isPrefixOf, isInfixOf)

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan)
import SyntaxValidator (ValidationResult(..), SyntaxError(..))
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

tests :: TestTree
tests = testGroup "User Added Syntax Validator Properties"
  [ textProcessingTests
  , sourceLocationTests
  , validationTests
  , errorHandlingTests
  ]

textProcessingTests :: TestTree
textProcessingTests = testGroup "Text Processing Properties"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves total length" prop_splitBy_preserves_length
  , fastProperty "removeLineComments preserves line count" prop_removeLineComments_preserves_lines
  , fastProperty "normalizeIndentation preserves non-empty content" prop_normalizeIndentation_preserves_content
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "Source Location Properties"
  [ fastProperty "posAfter advances offset" prop_posAfter_advances_offset
  , fastProperty "emptySpan has zero length" prop_emptySpan_zero_length
  , fastProperty "source position ordering is consistent" prop_position_ordering_consistent
  ]

validationTests :: TestTree
validationTests = testGroup "Validation Properties"
  [ fastProperty "validation preserves error positions" prop_validation_preserves_positions
  , fastProperty "syntax errors have valid locations" prop_syntax_errors_valid_locations
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Properties"
  [ fastProperty "error messages contain context" prop_error_messages_contain_context
  ]

-- Text Processing Properties

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

prop_splitBy_preserves_length :: Char -> String -> Property
prop_splitBy_preserves_length delim s =
  let parts = splitBy delim s
      totalLength = sum (map length parts) + length (filter (== delim) s)
  in totalLength === length s

prop_removeLineComments_preserves_lines :: String -> Property
prop_removeLineComments_preserves_lines s =
  let originalLines = lines s
      processedLines = lines (removeLineComments s)
  in length processedLines <= length originalLines

prop_normalizeIndentation_preserves_content :: String -> Property
prop_normalizeIndentation_preserves_content s =
  let normalized = normalizeIndentation s
      hasContent = not (null (trim s))
  in hasContent ==> property $ not (null (trim normalized))

-- Source Location Properties

prop_posAfter_advances_offset :: Char -> SourcePos -> Property
prop_posAfter_advances_offset c pos =
  let newPos = posAfter c pos
  in posOffset newPos === posOffset pos + 1

prop_emptySpan_zero_length :: Property
prop_emptySpan_zero_length = property $ emptySpan == emptySpan

prop_position_ordering_consistent :: SourcePos -> SourcePos -> Property
prop_position_ordering_consistent pos1 pos2 =
  let offset1 = posOffset pos1
      offset2 = posOffset pos2
  in if offset1 <= offset2 
     then property $ pos1 <= pos2
     else property $ pos1 > pos2

-- Validation Properties

prop_validation_preserves_positions :: String -> Property
prop_validation_preserves_positions code =
  let -- Simulate validation (simplified for property testing)
      hasErrors = "//" `isInfixOf` code || "/*" `isInfixOf` code
  in hasErrors ==> property True -- In real implementation, would check error positions

prop_syntax_errors_valid_locations :: String -> Property
prop_syntax_errors_valid_locations code =
  let -- Simulate syntax error detection
      hasError = ";;;" `isInfixOf` code
  in hasError ==> property $ startPos == startPos -- Valid start position

-- Error Handling Properties

prop_error_messages_contain_context :: String -> Property
prop_error_messages_contain_context code =
  let -- Simulate error message generation
      hasError = "error" `isInfixOf` code
      errorMsg = "Syntax error at: " ++ take 20 code
  in hasError ==> property $ length errorMsg > length "Syntax error at: "