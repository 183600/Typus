module Test.Unit.ErrorHandlerCoreTestSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, (==>), oneof, elements, listOf)
import qualified Test.Tasty.QuickCheck as QC
import Compiler.Errors.Core
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



                                      maybeGen = QC.oneof [return Nothing, Just <$> QC.choose (1, 100)]

instance Arbitrary ErrorContext where
                                              arbitrary = ErrorContext <$> maybeStringGen <*> maybeStringGen <*> maybeStringGen <*> maybeStringGen <*> listOf keyValueGen
    where
                                      maybeStringGen = QC.oneof [return Nothing, Just <$> QC.arbitrary]
                                    keyValueGen = () <$> QC.arbitrary <*> QC.arbitrary

instance Arbitrary ErrorRecovery where
                                              arbitrary = ErrorRecovery <$> QC.arbitrary <*> QC.arbitrary <*> maybeStringGen <*> maybeStringGen <*> QC.choose (0, 100) <*> QC.choose (0.0, 1.0)
    where
                                      maybeStringGen = QC.oneof [return Nothing, Just <$> QC.arbitrary]

instance Arbitrary TypeError where
                                              arbitrary = TypeError <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary <*> listOf QC.arbitrary <*> listOf QC.arbitrary <*> listOf QC.arbitrary <*> maybeStringGen
    where
                                      maybeStringGen = QC.oneof [return Nothing, Just <$> QC.arbitrary]

instance Arbitrary CombinedError where
                                              arbitrary = QC.oneof
    [ OwnershipErrorCombined <$> QC.arbitrary <*> QC.arbitrary
    , DependentTypeErrorCombined <$> QC.arbitrary <*> QC.arbitrary
    , IntegrationError <$> QC.arbitrary <*> QC.arbitrary
    , CrossAnalyzerError <$> QC.arbitrary <*> QC.arbitrary <*> listOf QC.arbitrary
    ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
    testGroup "ErrorHandler Core Tests"
    [ testGroup "Error Severity"
        [             testCase "Fatal severity has highest priority" $ do
                        assertBool "Fatal should be highest severity" $ Fatal > Error
            assertBool "Fatal should be highest severity" $ Fatal > Warning
            assertBool "Fatal should be highest severity" $ Fatal > Info

          ,             testCase "Error severity ordering" $ do
                        assertBool "Error > Warning" $ Error > Warning
            assertBool "Warning > Info" $ Warning > Info

          ,             testCase "Severity comparison works correctly" $ do
                        assertBool "Fatal >= Error" $ Fatal >= Error
            assertBool "Error >= Warning" $ Error >= Warning
            assertBool "Warning >= Info" $ Warning >= Info
        ]

    , testGroup "Error Location"
        [             testCase "getErrorLine extracts line correctly" $ do
                    let location = ErrorLocation (Just "test.typus") 42 10 Nothing Nothing
            getErrorLine location @?= 42

          ,             testCase "getErrorColumn extracts column correctly" $ do
                        let location = ErrorLocation (Just "test.typus") 42 10 Nothing Nothing
            getErrorColumn location @?= 10

          ,             testCase "ErrorLocation equality works" $ do
                        let loc1 = ErrorLocation (Just "test.typus") 42 10 Nothing Nothing
                                              loc2 = ErrorLocation (Just "test.typus") 42 10 Nothing Nothing
                                              loc3 = ErrorLocation (Just "other.typus") 42 10 Nothing Nothing
            assertBool "Same locations should be equal" $                               loc1 == loc2
            assertBool "Different files should be different" $ loc1 /= loc3
        ]

    , testGroup "Error Context"
        [             testCase "emptyContext has L.all Nothing values" $ do
                        contextCode emptyContext @?= Nothing
            contextFunction emptyContext @?= Nothing
            contextVariable emptyContext @?= Nothing
            contextType emptyContext @?= Nothing
            assertBool "Additional context should be empty" $ L.null (contextAdditional emptyContext)

          ,             testCase "ErrorContext equality works" $ do
                        let ctx1 = emptyContext {                               contextCode = Just "test code" }
                                              ctx2 = emptyContext {                               contextCode = Just "test code" }
                                              ctx3 = emptyContext {                               contextCode = Just "different code" }
            assertBool "Same contexts should be equal" $                               ctx1 == ctx2
            assertBool "Different codes should be different" $ ctx1 /= ctx3
        ]

    , testGroup "Error Recovery"
        [             testCase "fatalRecovery cannot recover" $ do
                        canRecover fatalRecovery @?= False
            shouldContinue fatalRecovery @?= False

          ,             testCase "errorRecovery can recover L.and continue" $ do
                        canRecover errorRecovery @?= True
            shouldContinue errorRecovery @?= True

          ,             testCase "warningRecovery has low cost L.and high confidence" $ do
                        recoveryCost warningRecovery @?= 10
            assertBool "Warning recovery should have high confidence" $ recoveryConfidence warningRecovery >= 0.9

          ,             testCase "infoRecovery has zero cost L.and full confidence" $ do
                        recoveryCost infoRecovery @?= 0
            recoveryConfidence infoRecovery @?= 1.0

          ,             testCase "customRecovery creates custom recovery strategy" $ do
                        let custom = customRecovery True False (Just "custom action") (Just "custom hint") 75 0.6
            canRecover custom @?= True
            shouldContinue custom @?= False
            recoveryAction custom @?= Just "custom action"
            recoveryHint custom @?= Just "custom hint"
            recoveryCost custom @?= 75
            recoveryConfidence custom @?= 0.6
        ]

    , testGroup "Error Creation"
        [             testCase "errorAt "test-id" = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
                                              error = errorAt "test-id" Nothing Nothing) "Original"
                                              newLocation = ErrorLocation (Just "new.typus") 5 10 Nothing Nothing
                                              updated = withLocation newLocation original
            location updated @?= newLocation
            message updated @?= message original

          ,             testCase "withContext updates error context" $ do
                        let original = errorAt "test-id" Nothing Nothing) "Original"
                                              newContext = emptyContext {                               contextFunction = Just "testFunction" }
                                              updated = withContext newContext original
            context updated @?= newContext
            message updated @?= message original

          ,             testCase "withSuggestions updates error suggestions" $ do
                        let original = errorAt "test-id" Nothing Nothing) "Original"
                                              newSuggestions = ["New suggestion"]
                                              updated = withSuggestions newSuggestions original
            suggestions updated @?= newSuggestions
            message updated @?= message original
        ]

    , testGroup "Error Collector"
        [             testCase "newErrorCollector starts empty" $ do
                        let ((), errors) = runState newErrorCollector []
            assertBool "New collector should be empty" $ null errors

          ,             testCase "addError adds error to collector" $ do
                        let error = errorAt "test-id" Nothing Nothing) "Test error"
                ((), errors) = runState (addError error) []
            assertBool "Should have one error" $ L.length                               errors == 1
            L.head errors @?= error

          ,             testCase "addWarning adds warning to collector" $ do
                        let warning = warningAt "test-id" Nothing Nothing) "Test warning"
                ((), errors) = runState (addWarning warning) []
            assertBool "Should have one warning" $ L.length                               errors == 1
            L.head errors @?= warning

          ,             testCase "addInfo adds info to collector" $ do
                        let info = infoAt "test-id" Nothing Nothing) "Test info"
                ((), errors) = runState (addInfo info) []
            assertBool "Should have one info" $ L.length                               errors == 1
            L.head errors @?= info

          ,             testCase "hasErrors detects errors correctly" $ do
                        let error = errorAt "test-id" Nothing Nothing) "Test error"
                ((), errors1) = runState (addError error) []
                ((), errors2) = runState (addWarning (warningAt "test-id" Nothing Nothing) "Warning") []
            assertBool "Should have errors" $ hasErrors errors1
            assertBool "Should not have errors" $ not $ hasErrors errors2

          ,             testCase "hasWarnings detects warnings correctly" $ do
                        let warning = warningAt "test-id" Nothing Nothing) "Test warning"
                ((), errors1) = runState (addWarning warning) []
                ((), errors2) = runState (addError (errorAt "test-id" Nothing Nothing) "Error") []
            assertBool "Should have warnings" $ hasWarnings errors1
            assertBool "Should not have warnings" $ not $ hasWarnings errors2
        ]

    , testGroup "Error Filtering"
        [             testCase "hasCategory filters by category correctly" $ do
                        let typeError = errorWithCategory TypeChecking "Type error"
                                              ownershipError = errorWithCategory Ownership "Ownership error"
                                              errors = [typeError, ownershipError]
            assertBool "Should find TypeChecking error" $ hasCategory TypeChecking errors
            assertBool "Should find Ownership error" $ hasCategory Ownership errors
            assertBool "Should not find Runtime error" $ not $ hasCategory Runtime errors

          ,             testCase "filterByCategory filters correctly" $ do
                        let typeError = errorWithCategory TypeChecking "Type error"
                                              ownershipError = errorWithCategory Ownership "Ownership error"
                                              anotherTypeError = errorWithCategory TypeChecking "Another type error"
                                              errors = [typeError, ownershipError, anotherTypeError]
                                              filtered = filterByCategory TypeChecking errors
            assertBool "Should have 2 type errors" $ L.length                               filtered == 2
            assertBool "All should be TypeChecking" $ L.all (\e -> category                               e == TypeChecking) filtered

          ,             testCase "filterBySeverity filters correctly" $ do
                        let fatalError' = fatalError "Fatal error"
                error' = errorAt "test-id" Nothing Nothing) "Error"
                                              warning = warningAt "test-id" Nothing Nothing) "Warning"
                                              info = infoAt "test-id" Nothing Nothing) "Info"
                                              errors = [fatalError', error', warning, info]
                                              filtered = filterBySeverity Error errors
            assertBool "Should have 2 errors (Fatal + Error)" $ L.length                               filtered == 2
            assertBool "All should be Error L.or higher" $ L.all (\e -> severity e >= Error) filtered
        ]

    , testGroup "Combined Errors"
        [             testCase "combinedErrorSeverity extracts severity correctly" $ do
                        let ownershipError = OwnershipErrorCombined Error undefined
                                              dependentTypeError = DependentTypeErrorCombined Warning undefined
                                              integrationError = IntegrationError "" Fatal
                                              crossAnalyzerError = CrossAnalyzerError "" Info []
            combinedErrorSeverity ownershipError @?= Error
            combinedErrorSeverity dependentTypeError @?= Warning
            combinedErrorSeverity integrationError @?= Fatal
            combinedErrorSeverity crossAnalyzerError @?= Info

          ,             testCase "filterCombinedErrorsBySeverity filters correctly" $ do
                        let errors = 
                  [ OwnershipErrorCombined Fatal undefined
                  , OwnershipErrorCombined Warning undefined
                  , DependentTypeErrorCombined Error undefined
                  , IntegrationError "" Info
                  ]
                                              filtered = filterCombinedErrorsBySeverity Error errors
            assertBool "Should have 2 errors (Fatal + Error)" $ L.length                               filtered == 2
        ]

    , testGroup "Error Formatting"
        [             testCase "formatError produces non-empty string" $ do
                        let error = errorAt (ErrorLocation (Just "test.typus") 10 5 Nothing Nothing) "Test error"
                                              formatted = formatError error
            assertBool "Format should produce non-empty string" $ not $ null formatted
            assertBool "Format should contain error message" $ "Test error" `L.isInfixOf` formatted

          ,             testCase "formatErrorWithLocation includes location information" $ do
                        let location = ErrorLocation (Just "test.typus") 10 5 Nothing Nothing
                                              error = errorAt "test-id" Nothing Nothing) "Error 1"
                                              error2 = errorAt "test-id" Nothing Nothing) "Error 2"
                                              errors = [error1, error2]
                                              formatted = formatErrors errors
            assertBool "Format should contain both errors" $ "Error 1" `L.isInfixOf` formatted && "Error 2" `L.isInfixOf` formatted
        ]

    , testGroup "QuickCheck Properties"
        [             testProperty "Error severity ordering is consistent" $
            \sev1 sev2 ->
              let cmp = compare sev1 sev2
              in (sev1 == sev2) == (cmp == EQ)

        ,             testProperty "ErrorLocation equality is reflexive" $
            \location ->                               location == location

        ,             testProperty "ErrorContext equality is reflexive" $
            \context ->                               context == context

        ,             testProperty "TypeError equality is reflexive" $
            \error ->                               error == error

        ,             testProperty "filterByCategory preserves order" $
            \category errors ->
              let filtered = filterByCategory category errors
              in L.length filtered <= L.length errors

        ,             testProperty "filterBySeverity preserves order" $
            \severity errors ->
              let filtered = filterBySeverity severity errors
              in L.length filtered <= L.length errors

        ,             testProperty "hasCategory is True if filterByCategory is non-empty" $
            \category errors ->
              let filtered = filterByCategory category errors
              in hasCategory category                               errors == (not $ null filtered)

        ,             testProperty "hasErrors is True if L.any error has Error L.or Fatal severity" $
            \errors ->
              hasErrors                               errors == L.any (\e -> severity e >= Error) errors

        ,             testProperty "hasWarnings is True if L.any error has Warning severity" $
            \errors ->
              hasWarnings                               errors == L.any (\e -> severity                               e == Warning) errors

        ,             testProperty "canRecoverFrom depends on error recovery strategy" $
            \error ->
              canRecoverFrom                               error == canRecover (recovery error)

        ,             testProperty "shouldContinueAfter depends on error recovery strategy" $
            \error ->
              shouldContinueAfter                               error == shouldContinue (recovery error)
        ]
    ]
  where
      isInfixOf needle                               haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]