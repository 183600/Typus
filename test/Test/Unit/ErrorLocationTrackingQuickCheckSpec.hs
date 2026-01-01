{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ErrorLocationTrackingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>))
import Test.Tasty.HUnit (testCase, assertBool)

import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)

-- Mock error types for testing
data MockError = MockError
  { mockErrorMessage :: String
  , mockErrorSpan :: SourceSpan
  , mockErrorSeverity :: MockSeverity
  } deriving (Show, Eq)

data MockSeverity = MockWarning | MockError | MockFatal
  deriving (Show, Eq)

data MockErrorContext = MockErrorContext
  { contextFile :: String
  , contextLine :: Int
  , contextColumn :: Int
  } deriving (Show, Eq)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary MockSeverity where
  arbitrary = elements [MockWarning, MockError, MockFatal]

instance Arbitrary MockError where
  arbitrary = MockError <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary MockErrorContext where
  arbitrary = MockErrorContext <$> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- Mock Functions (simplified versions for testing)
-- ============================================================================

mockCreateError :: String -> SourceSpan -> MockSeverity -> MockError
mockCreateError message span severity = MockError message span severity

mockGetErrorLocation :: MockError -> SourceSpan
mockGetErrorLocation = mockErrorSpan

mockUpdateErrorLocation :: MockError -> SourceSpan -> MockError
mockUpdateErrorLocation error newSpan = error { mockErrorSpan = newSpan }

mockGetContextFromError :: MockError -> MockErrorContext
mockGetContextFromError error = 
  let span = mockErrorSpan error
      start = spanStart span
  in MockErrorContext 
       (sourcePosFile start)
       (sourcePosLine start)
       (sourcePosColumn start)

mockIsErrorInSpan :: MockError -> SourceSpan -> Bool
mockIsErrorInSpan error span = 
  let errorSpan = mockErrorSpan error
      errorStart = spanStart errorSpan
      errorEnd = spanEnd errorSpan
      spanStart' = spanStart span
      spanEnd' = spanEnd span
  in (sourcePosLine errorStart > sourcePosLine spanStart' ||
      (sourcePosLine errorStart == sourcePosLine spanStart' && 
       sourcePosColumn errorStart >= sourcePosColumn spanStart')) &&
     (sourcePosLine errorEnd < sourcePosLine spanEnd' ||
      (sourcePosLine errorEnd == sourcePosLine spanEnd' && 
       sourcePosColumn errorEnd <= sourcePosColumn spanEnd'))

mockCompareErrorLocations :: MockError -> MockError -> Ordering
mockCompareErrorLocations error1 error2 = 
  let span1 = mockErrorSpan error1
      span2 = mockErrorSpan error2
      start1 = spanStart span1
      start2 = spanStart span2
  in compare (sourcePosLine start1, sourcePosColumn start1) 
            (sourcePosLine start2, sourcePosColumn start2)

mockOffsetError :: MockError -> Int -> Int -> MockError
mockOffsetError error lineOffset colOffset = 
  let span = mockErrorSpan error
      start = spanStart span
      end = spanEnd span
      newStart = start 
        { sourcePosLine = sourcePosLine start + lineOffset
        , sourcePosColumn = sourcePosColumn start + colOffset
        }
      newEnd = end
        { sourcePosLine = sourcePosLine end + lineOffset
        , sourcePosColumn = sourcePosColumn end + colOffset
        }
      newSpan = SourceSpan newStart newEnd
  in mockUpdateErrorLocation error newSpan

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Error location is preserved when creating error
prop_errorLocationPreserved :: String -> SourceSpan -> MockSeverity -> Property
prop_errorLocationPreserved message span severity = 
  let error = mockCreateError message span severity
      retrievedSpan = mockGetErrorLocation error
  in retrievedSpan === span

-- Property: Context extraction is consistent with span start
prop_contextConsistentWithSpan :: MockError -> Property
prop_contextConsistentWithSpan error = 
  let context = mockGetContextFromError error
      span = mockErrorSpan error
      start = spanStart span
  in (contextLine context === sourcePosLine start) &&
     (contextColumn context === sourcePosColumn start) &&
     (contextFile context === sourcePosFile start)

-- Property: Error in span detection is correct
prop_errorInSpanDetection :: MockError -> SourceSpan -> Property
prop_errorInSpanDetection error span = 
  let errorSpan = mockErrorSpan error
      sameSpan = errorSpan == span
      isInSpan = mockIsErrorInSpan error span
  in sameSpan ==> isInSpan

-- Property: Error location update preserves other fields
prop_locationUpdatePreservesFields :: MockError -> SourceSpan -> Property
prop_locationUpdatePreservesFields error newSpan = 
  let updatedError = mockUpdateErrorLocation error newSpan
  in (mockErrorMessage updatedError === mockErrorMessage error) &&
     (mockErrorSeverity updatedError === mockErrorSeverity error) &&
     (mockErrorSpan updatedError === newSpan)

-- Property: Error location comparison is transitive
prop_errorComparisonTransitive :: MockError -> MockError -> MockError -> Property
prop_errorComparisonTransitive error1 error2 error3 = 
  let cmp12 = mockCompareErrorLocations error1 error2
      cmp23 = mockCompareErrorLocations error2 error3
      cmp13 = mockCompareErrorLocations error1 error3
  in (cmp12 == EQ && cmp23 == EQ) ==> (cmp13 == EQ)

-- Property: Error offset changes location correctly
prop_errorOffsetChangesLocation :: MockError -> Int -> Int -> Property
prop_errorOffsetChangesLocation error lineOffset colOffset = 
  let offsetError = mockOffsetError error lineOffset colOffset
      originalSpan = mockErrorSpan error
      offsetSpan = mockErrorSpan offsetError
      originalStart = spanStart originalSpan
      offsetStart = spanStart offsetSpan
  in (sourcePosLine offsetStart === sourcePosLine originalStart + lineOffset) &&
     (sourcePosColumn offsetStart === sourcePosColumn originalStart + colOffset)

-- Property: Error offset preserves message L.and severity
prop_errorOffsetPreservesOtherFields :: MockError -> Int -> Int -> Property
prop_errorOffsetPreservesOtherFields error lineOffset colOffset = 
  let offsetError = mockOffsetError error lineOffset colOffset
  in (mockErrorMessage offsetError === mockErrorMessage error) &&
     (mockErrorSeverity offsetError === mockErrorSeverity error)

-- Property: Self offset should not change location
prop_selfOffsetNoChange :: MockError -> Property
prop_selfOffsetNoChange error = 
  let offsetError = mockOffsetError error 0 0
  in offsetError === error

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Error Location Tracking QuickCheck Tests"
  [ testProperty "Error location is preserved when creating error" prop_errorLocationPreserved
  , testProperty "Context extraction is consistent with span start" prop_contextConsistentWithSpan
  , testProperty "Error in span detection is correct" prop_errorInSpanDetection
  , testProperty "Error location update preserves other fields" prop_locationUpdatePreservesFields
  , testProperty "Error location comparison is transitive" prop_errorComparisonTransitive
  , testProperty "Error offset changes location correctly" prop_errorOffsetChangesLocation
  , testProperty "Error offset preserves message L.and severity" prop_errorOffsetPreservesOtherFields
  , testProperty "Self offset should not change location" prop_selfOffsetNoChange
  , testCase "Error location tracking edge cases" $ do
      -- Test error creation
      let span = SourceSpan (SourcePos "test.hs" 1 1) (SourcePos "test.hs" 1 10)
      let error = mockCreateError "Test error" span MockError
      assertBool "Error location should be preserved" $ 
        mockGetErrorLocation error == span
      
      -- Test context extraction
      let context = mockGetContextFromError error
      assertBool "Context should match span start" $ 
        contextLine context == 1 && contextColumn context == 1
      
      -- Test location update
      let newSpan = SourceSpan (SourcePos "test.hs" 2 5) (SourcePos "test.hs" 2 15)
      let updatedError = mockUpdateErrorLocation error newSpan
      assertBool "Updated error should have new location" $ 
        mockGetErrorLocation updatedError == newSpan
      assertBool "Updated error should preserve message" $ 
        mockErrorMessage updatedError == "Test error"
      
      -- Test offset
      let offsetError = mockOffsetError error 1 2
      let offsetSpan = mockGetErrorLocation offsetError
      let offsetStart = spanStart offsetSpan
      assertBool "Offset should change location" $ 
        sourcePosLine offsetStart == 2 && sourcePosColumn offsetStart == 3
  ]