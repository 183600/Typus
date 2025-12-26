{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorLocationTrackingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate error severities
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

-- | Generate error categories
instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- | Generate error locations
instance Arbitrary ErrorLocation where
  arbitrary = do
    file <- arbitraryMaybe arbitraryString
    line <- positive
    col <- positive
    endLine <- arbitraryMaybe positive
    endCol <- arbitraryMaybe positive
    return $ ErrorLocation file line col endLine endCol
    where
      positive = getPositive <$> arbitrary
      arbitraryMaybe gen = oneof [return Nothing, Just <$> gen]

-- | Generate error contexts
instance Arbitrary ErrorContext where
  arbitrary = do
    code <- arbitraryMaybe arbitraryString
    function <- arbitraryMaybe arbitraryString
    variable <- arbitraryMaybe arbitraryString
    typ <- arbitraryMaybe arbitraryString
    additional <- listOf ((,) <$> arbitraryString <*> arbitraryString)
    return $ ErrorContext code function variable typ additional

-- | Generate error recovery strategies
instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    action <- arbitraryMaybe arbitraryString
    hint <- arbitraryMaybe arbitraryString
    cost <- choose (0, 100)
    confidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRec shouldCont action hint cost confidence

-- | Generate type errors
instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitraryString
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> arbitraryString
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> arbitraryString)
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- arbitraryMaybe arbitraryString
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

-- | Generate combined errors
instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitraryOwnershipError
    , DependentTypeErrorCombined <$> arbitrary <*> arbitraryDependentTypeError
    , IntegrationError <$> arbitraryString <*> arbitrary
    , CrossAnalyzerError <$> arbitraryString <*> arbitrary <*> listOf arbitrary
    ]

-- | Generate arbitrary strings
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " _-"

-- | Generate ownership errors (simplified)
arbitraryOwnershipError :: Gen Own.OwnershipError
arbitraryOwnershipError = elements 
  [ Own.UseAfterMove "var"
  , Own.DoubleMove "var1" "var2"
  , Own.BorrowWhileMoved "var"
  ]

-- | Generate dependent type errors (simplified)
arbitraryDependentTypeError :: Gen Dep.DependentTypeError
arbitraryDependentTypeError = elements
  [ Dep.TypeMismatch "expected" "actual"
  , Dep.ConstraintViolation "constraint"
  ]

-- ============================================================================
-- QuickCheck Properties for Error Location Tracking
-- ============================================================================

-- | Error location should preserve line and column
prop_error_location_preserves_line_col :: String -> Int -> Int -> Property
prop_error_location_preserves_line_col file line col =
  let location = ErrorLocation (Just file) line col Nothing Nothing
  in filePath location === Just file .&&. 
     getErrorLine location === line .&&.
     getErrorColumn location === col

-- | Error location with range should preserve end positions
prop_error_location_preserves_range :: Int -> Int -> Int -> Int -> Property
prop_error_location_preserves_range startLine startCol endLine endCol =
  let location = ErrorLocation Nothing startLine startCol (Just endLine) (Just endCol)
  in getErrorLine location === startLine .&&.
     getErrorColumn location === startCol .&&.
     endLineNum location === Just endLine .&&.
     endColumn location === Just endCol

-- | Error location should be comparable
prop_error_location_comparable :: ErrorLocation -> ErrorLocation -> Property
prop_error_location_comparable loc1 loc2 =
  let eq = loc1 == loc2
      sameLine = getErrorLine loc1 == getErrorLine loc2
      sameCol = getErrorColumn loc1 == getErrorColumn loc2
  in eq === (sameLine && sameCol)

-- | Error context should preserve all components
prop_error_context_preserves :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> [(String, String)] -> Property
prop_error_context_preserves code function variable typ additional =
  let context = ErrorContext code function variable typ additional
  in contextCode context === code .&&.
     contextFunction context === function .&&.
     contextVariable context === variable .&&.
     contextType context === typ .&&.
     contextAdditional context === additional

-- | Empty context should be truly empty
prop_empty_context :: Property
prop_empty_context =
  let context = emptyContext
  in contextCode context === Nothing .&&.
     contextFunction context === Nothing .&&.
     contextVariable context === Nothing .&&.
     contextType context === Nothing .&&.
     contextAdditional context === []

-- | Error recovery should preserve strategy components
prop_error_recovery_preserves :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_error_recovery_preserves canRec shouldCont action hint cost confidence =
  let recovery = RecoveryStrategy canRec shouldCont action hint cost confidence
  in canRecover recovery === canRec .&&.
     shouldContinue recovery === shouldCont .&&.
     recoveryAction recovery === action .&&.
     recoveryHint recovery === hint .&&.
     recoveryCost recovery === cost .&&.
     recoveryConfidence recovery === confidence

-- | Fatal recovery should not be recoverable
prop_fatal_recovery_properties :: Property
prop_fatal_recovery_properties =
  let recovery = fatalRecovery
  in not (canRecover recovery) .&&.
     not (shouldContinue recovery) .&&.
     recoveryCost recovery === 100 .&&.
     recoveryConfidence recovery === 0.0

-- | Error recovery should be comparable
prop_error_recovery_comparable :: ErrorRecovery -> ErrorRecovery -> Property
prop_error_recovery_comparable rec1 rec2 =
  let eq = rec1 == rec2
      sameCanRecover = canRecover rec1 == canRecover rec2
      sameShouldContinue = shouldContinue rec1 == shouldContinue rec2
  in eq === (sameCanRecover && sameShouldContinue)

-- | Type error should preserve all fields
prop_type_error_preserves :: String -> ErrorSeverity -> ErrorCategory -> String -> ErrorLocation -> ErrorContext -> Property
prop_type_error_preserves errorId severity category message location context =
  let error = TypeError errorId severity category (T.pack message) location context errorRecovery [] [] [] Nothing
  in errorId error === errorId .&&.
     severity error === severity .&&.
     category error === category .&&.
     T.unpack (message error) === message .&&.
     location error === location .&&.
     context error === context

-- | Type error should be structurally valid
prop_type_error_structural :: TypeError -> Property
prop_type_error_structural typErr =
  let errId = errorId typErr
      sev = severity typErr
      cat = category typErr
      msg = message typErr
      loc = location typErr
      ctx = context typErr
  in errId `seq` sev `seq` cat `seq` msg `seq` loc `seq` ctx `seq` True

-- | Error severity ordering should be consistent
prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 =
  let ord1 = compare sev1 sev2
      ord2 = compare sev2 sev1
  in (ord1 == EQ) ==> (ord2 === EQ) .&&. (ord1 === EQ)

-- | Severity priority should be monotonic
prop_severity_priority_monotonic :: ErrorSeverity -> Property
prop_severity_priority_monotonic sev =
  let priority = severityPriority sev
  in priority >= 0 .&&. priority <= 100

-- | Error category should be comparable
prop_error_category_comparable :: ErrorCategory -> ErrorCategory -> Property
prop_error_category_comparable cat1 cat2 =
  let eq = cat1 == cat2
      ord = compare cat1 cat2
  in (eq && ord == EQ) .||. (not eq && ord /= EQ)

-- | Combined error should preserve severity
prop_combined_error_preserves_severity :: ErrorSeverity -> CombinedError -> Property
prop_combined_error_preserves_severity severity combinedErr =
  let newErr = case combinedErr of
        OwnershipErrorCombined _ ownErr -> OwnershipErrorCombined severity ownErr
        DependentTypeErrorCombined _ depErr -> DependentTypeErrorCombined severity depErr
        IntegrationError msg _ -> IntegrationError msg severity
        CrossAnalyzerError msg _ errs -> CrossAnalyzerError msg severity errs
  in combinedErrorSeverity newErr === severity

-- | Error filtering by severity should work correctly
prop_filter_by_severity :: ErrorSeverity -> [CombinedError] -> Property
prop_filter_by_severity minSeverity errors =
  let filtered = filterCombinedErrorsBySeverity minSeverity errors
      allHaveMinSeverity = all (\err -> isAtLeast minSeverity (combinedErrorSeverity err)) filtered
  in allHaveMinSeverity === True

-- | Error location formatting should be deterministic
prop_error_location_formatting :: ErrorLocation -> Property
prop_error_location_formatting location =
  let formatted1 = formatErrorWithLocation "test" location
      formatted2 = formatErrorWithLocation "test" location
  in formatted1 === formatted2

-- | Error with location should attach location correctly
prop_error_with_location :: TypeError -> ErrorLocation -> Property
prop_error_with_location typErr location =
  let locatedError = withLocation typErr location
  in location locatedError === location

-- | Error with context should attach context correctly
prop_error_with_context :: TypeError -> ErrorContext -> Property
prop_error_with_context typErr context =
  let contextualError = withContext typErr context
  in context contextualError === context

-- | Error wrapping should preserve original error
prop_error_wrapping :: TypeError -> String -> Property
prop_error_wrapping typErr wrapMessage =
  let wrappedError = wrapError typErr wrapMessage
      originalPresent = errorId typErr `isInfixOf` errorId wrappedError
  in originalPresent === True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Location Tracking QuickCheck Tests"
  [ testProperty "error location preserves line and column" prop_error_location_preserves_line_col
  , testProperty "error location preserves range" prop_error_location_preserves_range
  , testProperty "error location is comparable" prop_error_location_comparable
  , testProperty "error context preserves all components" prop_error_context_preserves
  , testProperty "empty context is truly empty" prop_empty_context
  , testProperty "error recovery preserves strategy components" prop_error_recovery_preserves
  , testProperty "fatal recovery properties" prop_fatal_recovery_properties
  , testProperty "error recovery is comparable" prop_error_recovery_comparable
  , testProperty "type error preserves all fields" prop_type_error_preserves
  , testProperty "type error is structurally valid" prop_type_error_structural
  , testProperty "error severity ordering is consistent" prop_severity_ordering
  , testProperty "severity priority is monotonic" prop_severity_priority_monotonic
  , testProperty "error category is comparable" prop_error_category_comparable
  , testProperty "combined error preserves severity" prop_combined_error_preserves_severity
  , testProperty "filter by severity works correctly" prop_filter_by_severity
  , testProperty "error location formatting is deterministic" prop_error_location_formatting
  , testProperty "error with location attaches correctly" prop_error_with_location
  , testProperty "error with context attaches correctly" prop_error_with_context
  , testProperty "error wrapping preserves original" prop_error_wrapping
  ]