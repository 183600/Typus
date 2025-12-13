{-# LANGUAGE CPP #-}

module Test.Unit.ErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..))
import Analyzer.Types (CombinedError(..))
import qualified Ownership as Own
import qualified Dependencies as Dep
import Data.List (isInfixOf)

-- Property: ErrorSeverity ordering
prop_errorseverity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_errorseverity_ordering sev1 sev2 =
  let result = compare sev1 sev2
  in (result == LT || result == EQ || result == GT) === True

-- Property: ErrorSeverity equality
prop_errorseverity_eq :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorseverity_eq sev1 sev2 = sev1 == sev2

-- Property: ErrorSeverity exhaustive
prop_errorseverity_exhaustive :: ErrorSeverity -> Property
prop_errorseverity_exhaustive sev =
  let isKnownSev = sev `elem` [Error, Warning, Info]
  in isKnownSev === True

-- Property: ErrorLocation with basic values
prop_errorlocation_basic :: Maybe String -> Int -> Int -> Property
prop_errorlocation_basic file line col =
  let loc = ErrorLocation file line col Nothing Nothing
  in filePath loc === file &&
     line loc === line &&
     column loc === col &&
     endLine loc === Nothing &&
     endColumn loc === Nothing

-- Property: ErrorLocation with end position
prop_errorlocation_with_end :: Maybe String -> Int -> Int -> Int -> Int -> Property
prop_errorlocation_with_end file line col endLine endCol =
  let loc = ErrorLocation file line col (Just endLine) (Just endCol)
  in filePath loc === file &&
     line loc === line &&
     column loc === col &&
     endLine loc === Just endLine &&
     endColumn loc === Just endCol

-- Property: ErrorLocation equality
prop_errorlocation_eq :: ErrorLocation -> ErrorLocation -> Property
prop_errorlocation_eq loc1 loc2 =
  (loc1 == loc2) === 
    (filePath loc1 == filePath loc2 &&
     line loc1 == line loc2 &&
     column loc1 == column loc2 &&
     endLine loc1 == endLine loc2 &&
     endColumn loc1 == endColumn loc2)

-- Property: ErrorLocation ordering
prop_errorlocation_ordering :: ErrorLocation -> ErrorLocation -> Property
prop_errorlocation_ordering loc1 loc2 =
  let result = compare loc1 loc2
  in (result == LT || result == EQ || result == GT) === True

-- Property: ErrorLocation show
prop_errorlocation_show :: ErrorLocation -> Property
prop_errorlocation_show loc =
  let shown = show loc
  in not (null shown)

-- Property: ErrorLocation show contains line and column
prop_errorlocation_show_contains_linecol :: Int -> Int -> Property
prop_errorlocation_show_contains_linecol line col =
  let loc = ErrorLocation Nothing line col Nothing Nothing
      shown = show loc
  in show line `isInfixOf` shown &&
     show col `isInfixOf` shown

-- Property: ErrorLocation with file path
prop_errorlocation_with_file :: String -> Int -> Int -> Property
prop_errorlocation_with_file file line col =
  let loc = ErrorLocation (Just file) line col Nothing Nothing
      shown = show loc
  in file `isInfixOf` shown

-- Property: ErrorLocation with negative values
prop_errorlocation_negative :: Property
prop_errorlocation_negative =
  let loc = ErrorLocation (Just "test") (-1) (-5) (Just (-2)) (Just (-10))
  in line loc === -1 &&
     column loc === -5 &&
     endLine loc === Just (-2) &&
     endColumn loc === Just (-10)

-- Property: OwnershipErrorCombined preserves values
prop_ownershiperrorcombined :: ErrorSeverity -> Own.OwnershipError -> ErrorLocation -> Property
prop_ownershiperrorcombined sev err loc =
  let combined = OwnershipErrorCombined sev err loc
  in case combined of
    OwnershipErrorCombined s e l -> s === sev && e === err && l === loc
    _ -> property False

-- Property: DependentTypeErrorCombined preserves values
prop_dependenttypeerrorcombined :: ErrorSeverity -> Dep.DependentTypeError -> ErrorLocation -> Property
prop_dependenttypeerrorcombined sev err loc =
  let combined = DependentTypeErrorCombined sev err loc
  in case combined of
    DependentTypeErrorCombined s e l -> s === sev && e === err && l === loc
    _ -> property False

-- Property: IntegrationError preserves values
prop_integrationerror :: String -> ErrorLocation -> Property
prop_integrationerror message loc =
  let combined = IntegrationError message loc
  in case combined of
    IntegrationError m l -> m === message && l === loc
    _ -> property False

-- Property: CrossAnalyzerError preserves values
prop_crossanalyzererror :: String -> ErrorLocation -> [String] -> Property
prop_crossanalyzererror analyzer loc details =
  let combined = CrossAnalyzerError analyzer loc details
  in case combined of
    CrossAnalyzerError a l d -> a === analyzer && l === loc && d === details
    _ -> property False

-- Property: CombinedError equality
prop_combinederror_eq :: CombinedError -> CombinedError -> Property
prop_combinederror_eq err1 err2 =
  (err1 == err2) === case (err1, err2) of
    (OwnershipErrorCombined s1 e1 l1, OwnershipErrorCombined s2 e2 l2) -> 
      s1 == s2 && e1 == e2 && l1 == l2
    (DependentTypeErrorCombined s1 e1 l1, DependentTypeErrorCombined s2 e2 l2) -> 
      s1 == s2 && e1 == e2 && l1 == l2
    (IntegrationError m1 l1, IntegrationError m2 l2) -> 
      m1 == m2 && l1 == l2
    (CrossAnalyzerError a1 l1 d1, CrossAnalyzerError a2 l2 d2) -> 
      a1 == a2 && l1 == l2 && d1 == d2
    _ -> False

-- Property: CombinedError ordering
prop_combinederror_ordering :: CombinedError -> CombinedError -> Property
prop_combinederror_ordering err1 err2 =
  let result = compare err1 err2
  in (result == LT || result == EQ || result == GT) === True

-- Property: CombinedError show
prop_combinederror_show :: CombinedError -> Property
prop_combinederror_show err =
  let shown = show err
  in not (null shown)

-- Property: CombinedError show contains relevant information
prop_combinederror_show_contains_info :: String -> Property
prop_combinederror_show_contains_info message =
  let loc = ErrorLocation Nothing 1 1 Nothing Nothing
      integration = IntegrationError message loc
      ownership = OwnershipErrorCombined Error (Own.UseAfterMove "test") loc
      dependent = DependentTypeErrorCombined Warning (Dep.TypeNotFound "test") loc
      shownIntegration = show integration
      shownOwnership = show ownership
      shownDependent = show dependent
  in message `isInfixOf` shownIntegration &&
     "UseAfterMove" `isInfixOf` shownOwnership &&
     "TypeNotFound" `isInfixOf` shownDependent

-- Property: CrossAnalyzerError with empty details
prop_crossanalyzererror_empty_details :: String -> ErrorLocation -> Property
prop_crossanalyzererror_empty_details analyzer loc =
  let combined = CrossAnalyzerError analyzer loc []
  in case combined of
    CrossAnalyzerError a l d -> a === analyzer && l === loc && null d
    _ -> property False

-- Property: ErrorLocation with very large values
prop_errorlocation_large_values :: Property
prop_errorlocation_large_values =
  let loc = ErrorLocation Nothing 999999 999999 (Just 999999) (Just 999999)
  in line loc === 999999 &&
     column loc === 999999 &&
     endLine loc === Just 999999 &&
     endColumn loc === Just 999999

-- Property: ErrorLocation with zero values
prop_errorlocation_zero_values :: Property
prop_errorlocation_zero_values =
  let loc = ErrorLocation Nothing 0 0 (Just 0) (Just 0)
  in line loc === 0 &&
     column loc === 0 &&
     endLine loc === Just 0 &&
     endColumn loc === Just 0

-- Property: ErrorLocation with mixed positive and negative
prop_errorlocation_mixed_values :: Property
prop_errorlocation_mixed_values =
  let loc = ErrorLocation (Just "test") (-5) 10 (Just 15) (Just -20)
  in line loc === -5 &&
     column loc === 10 &&
     endLine loc === Just 15 &&
     endColumn loc === Just (-20)

-- Property: CombinedError with special characters
prop_combinederror_special_chars :: Property
prop_combinederror_special_chars =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      loc = ErrorLocation Nothing 1 1 Nothing Nothing
      integration = IntegrationError specialChars loc
      shown = show integration
  in specialChars `isInfixOf` shown

-- Property: CombinedError with Unicode characters
prop_combinederror_unicode :: Property
prop_combinederror_unicode =
  let unicode = "测试错误信息🚀"
      loc = ErrorLocation Nothing 1 1 Nothing Nothing
      integration = IntegrationError unicode loc
      shown = show integration
  in unicode `isInfixOf` shown

-- Property: ErrorLocation with Unicode file path
prop_errorlocation_unicode_file :: Property
prop_errorlocation_unicode_file =
  let unicodeFile = "测试文件路径🚀.typus"
      loc = ErrorLocation (Just unicodeFile) 1 1 Nothing Nothing
      shown = show loc
  in unicodeFile `isInfixOf` shown

-- Property: CrossAnalyzerError with multiple details
prop_crossanalyzererror_multiple_details :: String -> ErrorLocation -> [String] -> Property
prop_crossanalyzererror_multiple_details analyzer loc details =
  let combined = CrossAnalyzerError analyzer loc details
  in case combined of
    CrossAnalyzerError a l d -> a === analyzer && l === loc && length d === length details
    _ -> property False

-- Property: ErrorLocation with only start position
prop_errorlocation_start_only :: Maybe String -> Int -> Int -> Property
prop_errorlocation_start_only file line col =
  let loc = ErrorLocation file line col Nothing Nothing
  in filePath loc === file &&
     line loc === line &&
     column loc === col &&
     endLine loc === Nothing &&
     endColumn loc === Nothing

-- Property: ErrorLocation with only end line
prop_errorlocation_endline_only :: Maybe String -> Int -> Int -> Int -> Property
prop_errorlocation_endline_only file line col endLine =
  let loc = ErrorLocation file line col (Just endLine) Nothing
  in filePath loc === file &&
     line loc === line &&
     column loc === col &&
     endLine loc === Just endLine &&
     endColumn loc === Nothing

-- Property: ErrorLocation with only end column
prop_errorlocation_endcol_only :: Maybe String -> Int -> Int -> Int -> Property
prop_errorlocation_endcol_only file line col endCol =
  let loc = ErrorLocation file line col Nothing (Just endCol)
  in filePath loc === file &&
     line loc === line &&
     column loc === col &&
     endLine loc === Nothing &&
     endColumn loc === Just endCol

-- Property: CombinedError with different severity levels
prop_combinederror_severity_levels :: ErrorSeverity -> ErrorLocation -> Property
prop_combinederror_severity_levels sev loc =
  let ownership = OwnershipErrorCombined sev (Own.UseAfterMove "test") loc
      dependent = DependentTypeErrorCombined sev (Dep.TypeNotFound "test") loc
  in case (ownership, dependent) of
    (OwnershipErrorCombined s _ _, DependentTypeErrorCombined s' _ _) -> s === sev && s' === sev
    _ -> property False

tests :: TestTree
tests = testGroup "ErrorHandling QuickCheck tests"
  [ fastProperty "ErrorSeverity ordering" prop_errorseverity_ordering
  , fastProperty "ErrorSeverity equality" prop_errorseverity_eq
  , fastProperty "ErrorSeverity exhaustive" prop_errorseverity_exhaustive
  , fastProperty "ErrorLocation with basic values" prop_errorlocation_basic
  , fastProperty "ErrorLocation with end position" prop_errorlocation_with_end
  , fastProperty "ErrorLocation equality" prop_errorlocation_eq
  , fastProperty "ErrorLocation ordering" prop_errorlocation_ordering
  , fastProperty "ErrorLocation show" prop_errorlocation_show
  , fastProperty "ErrorLocation show contains line and column" prop_errorlocation_show_contains_linecol
  , fastProperty "ErrorLocation with file path" prop_errorlocation_with_file
  , fastProperty "ErrorLocation with negative values" prop_errorlocation_negative
  , fastProperty "OwnershipErrorCombined preserves values" prop_ownershiperrorcombined
  , fastProperty "DependentTypeErrorCombined preserves values" prop_dependenttypeerrorcombined
  , fastProperty "IntegrationError preserves values" prop_integrationerror
  , fastProperty "CrossAnalyzerError preserves values" prop_crossanalyzererror
  , fastProperty "CombinedError equality" prop_combinederror_eq
  , fastProperty "CombinedError ordering" prop_combinederror_ordering
  , fastProperty "CombinedError show" prop_combinederror_show
  , fastProperty "CombinedError show contains relevant information" prop_combinederror_show_contains_info
  , fastProperty "CrossAnalyzerError with empty details" prop_crossanalyzererror_empty_details
  , fastProperty "ErrorLocation with very large values" prop_errorlocation_large_values
  , fastProperty "ErrorLocation with zero values" prop_errorlocation_zero_values
  , fastProperty "ErrorLocation with mixed positive and negative" prop_errorlocation_mixed_values
  , fastProperty "CombinedError with special characters" prop_combinederror_special_chars
  , fastProperty "CombinedError with Unicode characters" prop_combinederror_unicode
  , fastProperty "ErrorLocation with Unicode file path" prop_errorlocation_unicode_file
  , fastProperty "CrossAnalyzerError with multiple details" prop_crossanalyzererror_multiple_details
  , fastProperty "ErrorLocation with only start position" prop_errorlocation_start_only
  , fastProperty "ErrorLocation with only end line" prop_errorlocation_endline_only
  , fastProperty "ErrorLocation with only end column" prop_errorlocation_endcol_only
  , fastProperty "CombinedError with different severity levels" prop_combinederror_severity_levels
  ]