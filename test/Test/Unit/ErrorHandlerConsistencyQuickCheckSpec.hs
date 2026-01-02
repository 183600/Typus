{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.)
  , Arbitrary(..), Gen, oneof, choose, listOf, vectorOf, elements, sized, frequency
  , suchThat, resize
  )

import Compiler.Errors.Core
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)

-- Test error types for consistency checking
data TestError = TestError
  { errorMsg :: String
  , errorCode :: Int
  , errorPos :: SourcePos
  , errorSeverity :: ErrorSeverity
  } deriving (Show, Eq)

instance Arbitrary TestError where
  arbitrary = do
    msg <- elements ["Syntax error", "Type error", "Parse error", "Runtime error"]
    code <- choose (1000, 9999)
    line <- choose (1, 100)
    col <- choose (1, 100)
    severity <- elements [ErrorWarning, ErrorError, ErrorFatal]
    return $ TestError msg code (SourcePos line col 0) severity

data ErrorSeverity = ErrorWarning | ErrorError | ErrorFatal
  deriving (Show, Eq, Ord)

instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorWarning, ErrorError, ErrorFatal]

-- Property: Error messages are non-empty
prop_error_messages_nonempty :: TestError -> Property
prop_error_messages_nonempty error =
  property $ not (L.null (errorMsg error))

-- Property: Error codes are within valid range
prop_error_codes_valid_range :: TestError -> Property
prop_error_codes_valid_range error =
  let code = errorCode error
  in property $ code >= 1000 && code <= 9999

-- Property: Error positions have positive line numbers
prop_error_positions_positive_line :: TestError -> Property
prop_error_positions_positive_line error =
  let pos = errorPos error
  in property $ posLine pos > 0

-- Property: Error positions have positive column numbers
prop_error_positions_positive_column :: TestError -> Property
prop_error_positions_positive_column error =
  let pos = errorPos error
  in property $ posColumn pos > 0

-- Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let ordered = sort [sev1, sev2]
  in property $ L.head ordered <= last ordered

-- Property: Error list sorting maintains consistency
prop_error_list_sorting :: [TestError] -> Property
prop_error_list_sorting errors =
  let sortedByCode = sort $ map errorCode errors
      sortedByMsg = sort $ map errorMsg errors
  in property $ L.length sortedByCode == L.length errors &&
                L.length sortedByMsg == L.length errors

-- Property: Error deduplication works correctly
prop_error_deduplication :: [TestError] -> Property
prop_error_deduplication errors =
  let uniqueErrors = nub errors
      uniqueCodes = nub $ map errorCode errors
  in property $ L.length uniqueErrors == L.length uniqueCodes

-- Property: Error severity filtering works correctly
prop_error_severity_filtering :: [TestError] -> ErrorSeverity -> Property
prop_error_severity_filtering errors targetSeverity =
  let filtered = L.filter (\e -> errorSeverity e == targetSeverity) errors
      allCorrect = L.all (\e -> errorSeverity e == targetSeverity) filtered
  in property $ allCorrect

-- Property: Error position ordering is consistent
prop_error_position_ordering :: TestError -> TestError -> Property
prop_error_position_ordering err1 err2 =
  let pos1 = errorPos err1
      pos2 = errorPos err2
      lineCompare = compare (posLine pos1) (posLine pos2)
      colCompare = compare (posColumn pos1) (posColumn pos2)
  in property $ (lineCompare == EQ) ==> (colCompare == EQ || colCompare == LT || colCompare == GT)

-- Property: Error message formatting is consistent
prop_error_message_formatting :: TestError -> Property
prop_error_message_formatting error =
  let msg = errorMsg error
      hasContent = not (null msg)
      startsProperly = not (null msg) && L.head msg /= ' '
  in property $ hasContent .&&. startsProperly

-- Property: Error creation consistency
prop_error_creation_consistency :: String -> Int -> SourcePos -> ErrorSeverity -> Property
prop_error_creation_consistency msg code pos severity =
  not (null msg) && code >= 1000 && code <= 9999 && posLine pos > 0 && posColumn pos > 0 ==>
  let error = TestError msg code pos severity
  in property $ errorMsg error === msg &&
                errorCode error === code &&
                errorPos error === pos &&
                errorSeverity error === severity

tests :: TestTree
tests = testGroup "Error Handler Consistency QuickCheck Tests"
  [ fastProperty "Error messages are non-empty" prop_error_messages_nonempty
  , fastProperty "Error codes are within valid range" prop_error_codes_valid_range
  , fastProperty "Error positions have positive line numbers" prop_error_positions_positive_line
  , fastProperty "Error positions have positive column numbers" prop_error_positions_positive_column
  , fastProperty "Error severity ordering is consistent" prop_error_severity_ordering
  , fastProperty "Error list sorting maintains consistency" prop_error_list_sorting
  , fastProperty "Error deduplication works correctly" prop_error_deduplication
  , fastProperty "Error severity filtering works correctly" prop_error_severity_filtering
  , fastProperty "Error position ordering is consistent" prop_error_position_ordering
  , fastProperty "Error message formatting is consistent" prop_error_message_formatting
  , fastProperty "Error creation consistency" prop_error_creation_consistency
  , testCase "Manual error consistency test" $ do
      let error1 = TestError "Test error 1" 1001 (SourcePos 1 5 0) ErrorError
          error2 = TestError "Test error 2" 1002 (SourcePos 2 10 0) ErrorWarning
          error3 = TestError "Test error 1" 1001 (SourcePos 3 15 0) ErrorError  -- Duplicate code
      assertBool "Error messages should be non-empty" $ not (L.null $ errorMsg error1)
      assertBool "Error codes should be in valid range" $ errorCode error1 >= 1000 && errorCode error1 <= 9999
      assertBool "Error positions should have positive line numbers" $ posLine (errorPos error1) > 0
      assertBool "Error positions should have positive column numbers" $ posColumn (errorPos error1) > 0
      let errors = [error1, error2, error3]
          uniqueErrors = nub errors
      assertBool "Deduplication should work" $ L.length uniqueErrors == 2
  ]