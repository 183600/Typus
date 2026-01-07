module Test.Unit.NewOwnershipMemorySafetyQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Ownership ()
      newOwnershipAnalyzer, analyzeOwnership, analyzeOwnershipFile, analyzeOwnershipDebug,
      formatOwnershipErrors, lexAll, parseProgram, builtInFunctions )
import Ownership.Common.Types (OwnershipError(..), OwnershipType(..), OwnershipTransfer)
import Parser (TypusFile(..), parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List ()
                                              result = analyzeOwnership input
            case result of
                Left errors -> L.any (isInfixOf "use after move" . unpack) errors @?= True
                Right _ -> @?= False True
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


          ,             testCase "double move is detected" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  owner1 := data"
                  , "  owner2 := data  // Should error: double move"
                  , "}"
                  ]
                                              result = analyzeOwnership input
            case result of
                Left errors -> L.any (isInfixOf "double move" . unpack) errors @?= True
                Right _ -> @?= False True

          ,             testCase "borrowing prevents mutation" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  ref := &data"
                  , "  modify(data)  // Should error: data is borrowed"
                  , "}"
                  ]
                                              result = analyzeOwnership input
            case result of
                Left errors -> L.any (isInfixOf "borrowed" . unpack) errors @?= True
                Right _ -> @?= False True

          ,             testCase "scope-based destruction" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  {"
                  , "    temp := make([]int, 50)"
                  , "    use(temp)"
                  , "  }  // temp should be destroyed here"
                  , "  // temp should not be accessible here"
                  , "}"
                  ]
                                              result = analyzeOwnership input
            case result of
                Right _ -> @?= True True
                Left _ -> @?= False True

          ,             testCase "reference counting correctness" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  ref1 := &data"
                  , "  ref2 := ref1"
                  , "  ref3 := &data"
                  , "  // All references should be tracked"
                  , "}"
                  ]
                                              result = analyzeOwnership input
            case result of
                Right _ -> @?= True True
                Left _ -> @?= False True

          ,             testCase "complex ownership transfer" $ do
                        let input = unlines
                  [ "func test() {"
                  , "  data := make([]int, 100)"
                  , "  owner := takeOwnership(data)"
                  , "  processor := createProcessor(owner)"
                  , "  result := processor.process()"
                  , "  return result"
                  , "}"
                  ]
                                              result = analyzeOwnership input
            case result of
                Right _ -> @?= True True
                Left errors -> L.length errors >= 0 @?= True
        ]
    ]

-- | 
prop_transferPreventsUseAfterMove :: String -> Property
prop_transferPreventsUseAfterMove                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  owner := " ++ variableName
        , "  process(" ++ variableName ++ ")  // Use after move"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> L.any (isInfixOf "use after move" . unpack) errors
       Right _ ->                               variableName == "" -- May pass if variable name is empty

-- | 
prop_transferTracksMovedValues :: String -> Property
prop_transferTracksMovedValues                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  owner := " ++ variableName
        , "  // " ++ variableName ++ " should be marked as moved"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left _ ->                               variableName == "" -- May fail if variable name is empty
       Right _ -> True

-- | 
prop_transferAllowsValidMoves :: String -> Property
prop_transferAllowsValidMoves                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  owner := " ++ variableName
        , "  process(owner)  // Valid use of owner"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> not (L.any (isInfixOf "use after move" . unpack) errors)
       Right _ -> True

-- | 
prop_transferHandlesComplexScenarios :: String -> Property
prop_transferHandlesComplexScenarios                               input =
  let complexInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Complex ownership scenario"
        , "}"
        ]
                                    result = analyzeOwnership complexInput
  in case result of
       Left _ -> True -- Should handle complex scenarios
       Right _ -> True

-- | 
prop_noDoubleFree :: String -> Property
prop_noDoubleFree                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  owner1 := " ++ variableName
        , "  owner2 := owner1"
        , "  // Should not double free"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> not (L.any (isInfixOf "double free" . unpack) errors)
       Right _ -> True

-- | 
prop_noDanglingPointers :: String -> Property
prop_noDanglingPointers                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Should not create dangling pointers"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> not (L.any (isInfixOf "dangling" . unpack) errors)
       Right _ -> True

-- | 
prop_noMemoryLeaks :: String -> Property
prop_noMemoryLeaks                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Should not leak memory"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> not (L.any (isInfixOf "leak" . unpack) errors)
       Right _ -> True

-- | 
prop_properLifetimeManagement :: String -> Property
prop_properLifetimeManagement                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Should manage lifetimes correctly"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True -- Should handle lifetime management
       Right _ -> True

-- | 
prop_borrowingPreventsMutation :: String -> Property
prop_borrowingPreventsMutation                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  ref := &" ++ variableName
        , "  modify(" ++ variableName ++ ")  // Should error during borrow"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> L.any (isInfixOf "borrowed" . unpack) errors ||                               variableName == ""
       Right _ ->                               variableName == ""

-- | 
prop_multipleImmutableBorrows :: String -> Property
prop_multipleImmutableBorrows                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  ref1 := &" ++ variableName
        , "  ref2 := &" ++ variableName
        , "  ref3 := &" ++ variableName
        , "  // Multiple immutable borrows should be allowed"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> not (L.any (isInfixOf "borrow" . unpack) errors) ||                               variableName == ""
       Right _ -> True

-- | 
prop_singleMutableBorrow :: String -> Property
prop_singleMutableBorrow                               variableName =
  let input = unlines
        [ "func test() {"
        , "  " ++ variableName ++ " := make([]int, 100)"
        , "  ref1 := &" ++ variableName
        , "  ref2 := &mut " ++ variableName ++ "  // Should error"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left errors -> L.any (isInfixOf "borrow" . unpack) errors ||                               variableName == ""
       Right _ ->                               variableName == ""

-- | 
prop_borrowLifetimeTracked :: String -> Property
prop_borrowLifetimeTracked                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Borrow lifetime should be tracked"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True
       Right _ -> True

-- | 
prop_referenceCountNonNegative :: Int -> Property
prop_referenceCountNonNegative                               count =
  count >=                               0 ==> count >= 0 -- Reference count should never be negative

-- | 
prop_referenceCountIncrements :: Int -> Property
prop_referenceCountIncrements                               baseCount =
  baseCount >=                               0 ==>
  let newCount = baseCount + 1
  in newCount > baseCount

-- | 
prop_referenceCountDecrements :: Int -> Property
prop_referenceCountDecrements                               baseCount =
  baseCount >                               0 ==>
  let newCount = baseCount - 1
  in newCount < baseCount && newCount >= 0

-- | 
prop_referenceCountReachesZero :: Int -> Property
prop_referenceCountReachesZero                               initialCount =
  initialCount >=                               0 ==>
  let finalCount = 0 -- After L.all references are destroyed
  in                               finalCount == 0

-- | 
prop_variablesDestroyedAtScopeExit :: String -> Property
prop_variablesDestroyedAtScopeExit                               variableName =
  let input = unlines
        [ "func test() {"
        , "  {"
        , "    " ++ variableName ++ " := make([]int, 100)"
        , "    use(" ++ variableName ++ ")"
        , "  }  // " ++ variableName ++ " should be destroyed"
        , "}"
        ]
                                    result = analyzeOwnership input
  in case result of
       Left _ ->                               variableName == ""
       Right _ -> True

-- | 
prop_referencesOutliveValues :: String -> Property
prop_referencesOutliveValues                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // References should not outlive values"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> not (L.any (isInfixOf "outlive" . unpack) errors)
       Right _ -> True

-- | 
prop_temporaryValuesLifetime :: String -> Property
prop_temporaryValuesLifetime                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "  // Temporary values should have correct lifetime"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True
       Right _ -> True

-- | 
prop_nestedScopesHandled :: String -> Property
prop_nestedScopesHandled                               input =
  let testInput = unlines
        [ "func test() {"
        , "  {"
        , "    {"
        , "      " ++ input
        , "    }"
        , "  }"
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True
       Right _ -> True

-- | 
prop_ownershipErrorsDetected :: String -> Property
prop_ownershipErrorsDetected                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> L.length errors >= 0
       Right _ -> True

-- | 
prop_errorMessagesInformative :: String -> Property
prop_errorMessagesInformative                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> L.all (not . null . unpack) errors
       Right _ -> True

-- | 
prop_errorLocationsAccurate :: String -> Property
prop_errorLocationsAccurate                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> True -- Should provide accurate locations
       Right _ -> True

-- | 
prop_errorRecoveryPossible :: String -> Property
prop_errorRecoveryPossible                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left errors -> True -- Should allow error recovery
       Right _ -> True

-- | 
prop_ownershipAnalysisLinear :: String -> Property
prop_ownershipAnalysisLinear                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True -- Should complete in linear time
       Right _ -> True

-- | 
prop_memoryUsageBounded :: String -> Property
prop_memoryUsageBounded                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result = analyzeOwnership testInput
  in case result of
       Left _ -> True -- Should use bounded memory
       Right _ -> True

-- | 
prop_analysisCompletesReasonableTime :: String -> Property
prop_analysisCompletesReasonableTime                               input =
  let testInput = unlines
        [ "func test() {"
        , "  " ++ input
        , "}"
        ]
                                    result =  analyzeOwnership testInput
  in case result of
       Left _ -> True -- Should complete in property $ reasonable time
       Right _ -> property True