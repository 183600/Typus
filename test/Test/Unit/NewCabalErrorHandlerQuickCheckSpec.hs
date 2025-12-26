{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat, frequency)
import Data.List (sort, sortBy, intercalate)
import Data.Ord (comparing)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , errorAt
  , errorAtWithTimestamp
  , errorAtWithUTCTime
  , errorWithCategory
  , warningAt
  , warningWithCategory
  , infoAt
  , infoWithCategory
  , fatalError
  , fatalErrorWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , withTimestamp
  , withUTCTimestamp
  , wrapError
  , combineErrors
  , formatError
  , formatErrorWithLocation
  , formatErrors
  , formatErrorsWithLocation
  , canRecoverFrom
  , shouldContinueAfter
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , generateErrorReportWithTimestamp
  , generateErrorReportWithUTCTime
  , generateErrorReportIO
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , isAtLeast
  , compareSeverity
  , severityPriority
  , _unknownLocation
  , _atLocation
  , _atFileLocation
  , _atRange
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , customRecovery
  , createRecoveryStrategy
  )

import qualified Ownership.Common.Types as Own
import qualified Dependencies.TypeSystem as Dep

-- ============================================================================
-- Arbitrary instances for ErrorHandler testing
-- ============================================================================

-- Generate error severity levels
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- Generate error categories
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements 
  [ TypeChecking, Ownership, Parsing, Semantic, Runtime
  , Constraint, Inference, Integration, Unknown
  ]

-- Generate error locations
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  hasFile <- elements [True, False]
  hasRange <- elements [True, False]
  lineNum <- choose (1, 1000)
  colNum <- choose (1, 200)
  file <- if hasFile then Just <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++['_', '/', '.'])
           else pure Nothing
  (endLine, endCol) <- if hasRange
                       then do
                           endLine' <- choose (lineNum, lineNum + 100)
                           endCol' <- if endLine' == lineNum 
                                     then choose (colNum, colNum + 100)
                                     else choose (1, 200)
                           pure (Just endLine', Just endCol')
                       else pure (Nothing, Nothing)
  return $ ErrorLocation file lineNum colNum endLine endCol

-- Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  hasCode <- elements [True, False]
  hasFunction <- elements [True, False]
  hasVariable <- elements [True, False]
  hasType <- elements [True, False]
  additionalCount <- choose (0, 3)
  
  code <- if hasCode then Just <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n()[]{}")
           else pure Nothing
  function <- if hasFunction then Just <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_')
              else pure Nothing
  variable <- if hasVariable then Just <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_')
              else pure Nothing
  varType <- if hasType then Just <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_')
             else pure Nothing
  
  additional <- sequence $ replicate additionalCount $ do
    key <- listOf (elements $ ['a'..'z'] ++ '_')
    value <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ' ')
    return (key, value)
  
  return $ ErrorContext code function variable varType additional

-- Generate error recovery strategies
genErrorRecovery :: Gen ErrorRecovery
genErrorRecovery = do
  canRec <- elements [True, False]
  shouldCont <- elements [True, False]
  hasAction <- elements [True, False]
  hasHint <- elements [True, False]
  cost <- choose (0, 100)
  confidence <- choose (0.0, 1.0)
  
  action <- if hasAction then Just <$> listOf (elements $ ['a'..'z'] ++ ' ')
            else pure Nothing
  hint <- if hasHint then Just <$> listOf (elements $ ['a'..'z'] ++ ' ')
           else pure Nothing
  
  return $ RecoveryStrategy canRec shouldCont action hint cost confidence

-- Generate type errors
genTypeError :: Gen TypeError
genTypeError = do
  errId <- listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ '_')
  severity <- genErrorSeverity
  category <- genErrorCategory
  message <- T.pack <$> listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ' ')
  location <- genErrorLocation
  context <- genErrorContext
  recovery <- genErrorRecovery
  suggestionsCount <- choose (0, 3)
  suggestions <- sequence $ replicate suggestionsCount (T.pack <$> listOf (elements $ ['a'..'z'] ++ ' '))
  relatedCount <- choose (0, 2)
  related <- sequence $ replicate relatedCount genTypeError
  chainCount <- choose (0, 2)
  chain <- sequence $ replicate chainCount genTypeError
  hasTimestamp <- elements [True, False]
  timestamp <- if hasTimestamp then Just <$> listOf (elements $ ['0'..'9'] ++ ':' ++ '.' ++ ' ')
               else pure Nothing
  
  return $ TypeError errId severity category message location context recovery 
                    suggestions related chain timestamp

-- Generate combined errors
genCombinedError :: Gen CombinedError
genCombinedError = oneof
  [ do
      severity <- genErrorSeverity
      ownError <- arbitrary -- Assuming OwnershipError has Arbitrary instance
      return $ OwnershipErrorCombined severity ownError
  , do
      severity <- genErrorSeverity
      depError <- arbitrary -- Assuming DependentTypeError has Arbitrary instance
      return $ DependentTypeErrorCombined severity depError
  , do
      msg <- listOf (elements $ ['a'..'z'] ++ ' ')
      severity <- genErrorSeverity
      return $ IntegrationError msg severity
  , do
      msg <- listOf (elements $ ['a'..'z'] ++ ' ')
      severity <- genErrorSeverity
      errorCount <- choose (1, 3)
      errors <- sequence $ replicate errorCount genCombinedError
      return $ CrossAnalyzerError msg severity errors
  ]

-- ============================================================================
-- Property Tests for ErrorSeverity
-- ============================================================================

prop_severityPriorityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityPriorityOrdering sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
      ord = compareSeverity sev1 sev2
  in case ord of
    LT -> p1 < p2
    EQ -> p1 == p2
    GT -> p1 > p2

prop_isAtLeastTransitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeastTransitive minSev midSev maxSev =
  isAtLeast minSev midSev && isAtLeast midSev maxSev ==> isAtLeast minSev maxSev

prop_severityComparisonConsistent :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityComparisonConsistent sev1 sev2 =
  let p1 = severityPriority sev1
      p2 = severityPriority sev2
  in compare p1 p2 == compareSeverity sev1 sev2

-- ============================================================================
-- Property Tests for ErrorLocation
-- ============================================================================

prop_locationRangeValid :: ErrorLocation -> Property
prop_locationRangeValid loc =
  let hasRange = isJust (endLine loc) && isJust (endColumn loc)
  in hasRange ==> 
    let endL = fromMaybe (line loc) (endLine loc)
        endC = fromMaybe (column loc) (endColumn loc)
    in line loc <= endL && (line loc < endL || column loc <= endC)

prop_atLocationCreatesCorrectLocation :: Int -> Int -> Bool
prop_atLocationCreatesCorrectLocation lineNum col =
  let loc = _atLocation lineNum col
  in filePath loc == Nothing &&
     line loc == lineNum &&
     column loc == col &&
     endLine loc == Nothing &&
     endColumn loc == Nothing

prop_atFileLocationCreatesCorrectLocation :: String -> Int -> Int -> Bool
prop_atFileLocationCreatesCorrectLocation file lineNum col =
  let loc = _atFileLocation file lineNum col
  in filePath loc == Just file &&
     line loc == lineNum &&
     column loc == col &&
     endLine loc == Nothing &&
     endColumn loc == Nothing

prop_atRangeCreatesCorrectLocation :: Int -> Int -> Int -> Int -> Property
prop_atRangeCreatesCorrectLocation startLine startCol endLine endCol =
  startLine <= endLine && (startLine < endLine || startCol <= endCol) ==>
    let loc = _atRange startLine startCol endLine endCol
    in filePath loc == Nothing &&
       line loc == startLine &&
       column loc == startCol &&
       endLine loc == Just endLine &&
       endColumn loc == Just endCol

-- ============================================================================
-- Property Tests for ErrorRecovery
-- ============================================================================

prop_fatalRecoveryCannotRecover :: Property
prop_fatalRecoveryCannotRecover =
  let rec = fatalRecovery
  in not (canRecover rec) && not (shouldContinue rec)

prop_customRecoveryPreservesValues :: Bool -> Bool -> Maybe String -> Maybe String -> Int -> Float -> Property
prop_customRecoveryPreservesValues canRec shouldCont action hint cost confidence =
  confidence >= 0.0 && confidence <= 1.0 && cost >= 0 && cost <= 100 ==>
    let rec = customRecovery canRec shouldCont action hint cost confidence
    in canRecover rec == canRec &&
       shouldContinue rec == shouldCont &&
       recoveryAction rec == action &&
       recoveryHint rec == hint &&
       recoveryCost rec == cost &&
       recoveryConfidence rec == confidence

-- ============================================================================
-- Property Tests for TypeError
-- ============================================================================

prop_errorAtCreatesCorrectError :: String -> String -> ErrorLocation -> Bool
prop_errorAtCreatesCorrectError errId msg loc =
  let err = errorAt errId (T.pack msg) loc
  in errorId err == errId &&
     severity err == Error &&
     category err == Unknown &&
     T.unpack (message err) == msg &&
     location err == loc &&
     context err == emptyContext &&
     recovery err == errorRecovery &&
     null (suggestions err) &&
     null (relatedErrors err) &&
     null (errorChain err) &&
     timestamp err == Nothing

prop_errorWithCategorySetsCorrectCategory :: String -> ErrorCategory -> String -> ErrorLocation -> Bool
prop_errorWithCategorySetsCorrectCategory errId cat msg loc =
  let err = errorWithCategory errId cat (T.pack msg) loc
  in category err == cat

prop_warningAtHasWarningSeverity :: String -> String -> ErrorLocation -> Bool
prop_warningAtHasWarningSeverity errId msg loc =
  let err = warningAt errId (T.pack msg) loc
  in severity err == Warning

prop_infoAtHasInfoSeverity :: String -> String -> ErrorLocation -> Bool
prop_infoAtHasInfoSeverity errId msg loc =
  let err = infoAt errId (T.pack msg) loc
  in severity err == Info

prop_fatalErrorHasFatalSeverity :: String -> String -> ErrorLocation -> Bool
prop_fatalErrorHasFatalSeverity errId msg loc =
  let err = fatalError errId (T.pack msg) loc
  in severity err == Fatal &&
     recovery err == fatalRecovery

prop_withLocationUpdatesLocation :: TypeError -> ErrorLocation -> Bool
prop_withLocationUpdatesLocation err newLoc =
  let updatedErr = withLocation err newLoc
  in location updatedErr == newLoc

prop_withContextUpdatesContext :: TypeError -> ErrorContext -> Bool
prop_withContextUpdatesContext err newCtx =
  let updatedErr = withContext err newCtx
  in context updatedErr == newCtx

prop_withSuggestionsAppendsSuggestions :: TypeError -> [String] -> Bool
prop_withSuggestionsAppendsSuggestions err newSuggestions =
  let updatedErr = withSuggestions (map T.pack newSuggestions) err
      oldSuggestions = suggestions err
      newSuggestionsText = map T.pack newSuggestions
  in suggestions updatedErr == newSuggestionsText ++ oldSuggestions

prop_wrapErrorAddsToMessage :: TypeError -> String -> Bool
prop_wrapErrorAddsToMessage err wrapperMsg =
  let wrappedErr = wrapError (T.pack wrapperMsg) err
      originalMsg = message err
      wrappedMsg = message wrappedErr
  in wrappedMsg == T.pack wrapperMsg <> " " <> originalMsg

prop_wrapErrorAddsToChain :: TypeError -> String -> Bool
prop_wrapErrorAddsToChain err wrapperMsg =
  let wrappedErr = wrapError (T.pack wrapperMsg) err
      originalChain = errorChain err
      newChain = errorChain wrappedErr
  in newChain == err : originalChain

-- ============================================================================
-- Property Tests for Error Filtering and Analysis
-- ============================================================================

prop_hasCategoryChecksCorrectly :: ErrorCategory -> TypeError -> Bool
prop_hasCategoryChecksCorrectly cat err =
  hasCategory cat err == (category err == cat)

prop_filterByCategoryReturnsOnlyMatching :: ErrorCategory -> [TypeError] -> Bool
prop_filterByCategoryReturnsOnlyMatching cat errors =
  let filtered = filterByCategory cat errors
  in all (\e -> category e == cat) filtered

prop_filterBySeverityReturnsOnlyMatching :: ErrorSeverity -> [TypeError] -> Bool
prop_filterBySeverityReturnsOnlyMatching sev errors =
  let filtered = filterBySeverity sev errors
  in all (\e -> severity e == sev) filtered

prop_getErrorStatisticsCountsCorrectly :: [TypeError] -> Bool
prop_getErrorStatisticsCountsCorrectly errors =
  let stats = getErrorStatistics errors
      total = length errors
      fatalCount = length $ filter (\e -> severity e == Fatal) errors
      errorCount = length $ filter (\e -> severity e == Error) errors
      warningCount = length $ filter (\e -> severity e == Warning) errors
      infoCount = length $ filter (\e -> severity e == Info) errors
  in Map.findWithDefault 0 "total" stats == total &&
     Map.findWithDefault 0 "fatal" stats == fatalCount &&
     Map.findWithDefault 0 "errors" stats == errorCount &&
     Map.findWithDefault 0 "warnings" stats == warningCount &&
     Map.findWithDefault 0 "info" stats == infoCount

prop_getErrorsReturnsOnlyErrorsAndFatals :: [TypeError] -> Bool
prop_getErrorsReturnsOnlyErrorsAndFatals errors =
  let filtered = getErrors errors
  in all (\e -> severity e == Error || severity e == Fatal) filtered

prop_getWarningsReturnsOnlyWarnings :: [TypeError] -> Bool
prop_getWarningsReturnsOnlyWarnings errors =
  let filtered = getWarnings errors
  in all (\e -> severity e == Warning) filtered

prop_getInfoReturnsOnlyInfo :: [TypeError] -> Bool
prop_getInfoReturnsOnlyInfo errors =
  let filtered = getInfo errors
  in all (\e -> severity e == Info) filtered

prop_hasErrorsDetectsErrorsCorrectly :: [TypeError] -> Bool
prop_hasErrorsDetectsErrorsCorrectly errors =
  let hasErrs = hasErrors errors
      errorOrFatalExists = any (\e -> severity e == Error || severity e == Fatal) errors
  in hasErrs == errorOrFatalExists

prop_hasWarningsDetectsWarningsCorrectly :: [TypeError] -> Bool
prop_hasWarningsDetectsWarningsCorrectly errors =
  let hasWarns = hasWarnings errors
      warningExists = any (\e -> severity e == Warning) errors
  in hasWarns == warningExists

-- ============================================================================
-- Property Tests for CombinedError
-- ============================================================================

prop_combinedErrorSeverityMatches :: CombinedError -> Bool
prop_combinedErrorSeverityMatches combinedErr =
  let expectedSev = case combinedErr of
        OwnershipErrorCombined sev _ -> sev
        DependentTypeErrorCombined sev _ -> sev
        IntegrationError _ sev -> sev
        CrossAnalyzerError _ sev _ -> sev
  in combinedErrorSeverity combinedErr == expectedSev

prop_filterCombinedErrorsBySeverityWorks :: ErrorSeverity -> [CombinedError] -> Bool
prop_filterCombinedErrorsBySeverityWorks minSeverity combinedErrors =
  let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
  in all (\e -> isAtLeast minSeverity (combinedErrorSeverity e)) filtered

-- ============================================================================
-- Property Tests for Error Formatting
-- ============================================================================

prop_formatErrorIncludesSeverity :: TypeError -> Bool
prop_formatErrorIncludesSeverity err =
  let formatted = formatError err
      severityStr = case severity err of
        Fatal -> "FATAL"
        Error -> "ERROR"
        Warning -> "WARNING"
        Info -> "INFO"
  in severityStr `isInfixOf` formatted

prop_formatErrorIncludesCategory :: TypeError -> Bool
prop_formatErrorIncludesCategory err =
  let formatted = formatError err
      categoryStr = "[" ++ show (category err) ++ "]"
  in categoryStr `isInfixOf` formatted

prop_formatErrorWithLocationIncludesLocation :: TypeError -> Property
prop_formatErrorWithLocationIncludesLocation err =
  let loc = location err
      hasLocation = line loc > 0 || column loc > 0 || isJust (filePath loc)
  in hasLocation ==> 
    let formatted = formatErrorWithLocation err
        lineStr = if line loc > 0 then show (line loc) else "?"
        colStr = if column loc > 0 then show (column loc) else "?"
    in lineStr `isInfixOf` formatted && colStr `isInfixOf` formatted

prop_formatErrorsIncludesAllErrors :: [TypeError] -> Bool
prop_formatErrorsIncludesAllErrors errors =
  let formatted = formatErrors errors
      individualFormats = map formatError errors
  in all (`isInfixOf` formatted) individualFormats

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

tests :: TestTree
tests = testGroup "ErrorHandler QuickCheck Tests"
  [ testGroup "ErrorSeverity properties"
    [ fastProperty "severity priority ordering" prop_severityPriorityOrdering
    , fastProperty "isAtLeast is transitive" prop_isAtLeastTransitive
    , fastProperty "severity comparison consistent" prop_severityComparisonConsistent
    , testCase "severity priority values are correct" $ do
        severityPriority Fatal @?= 100
        severityPriority Error @?= 80
        severityPriority Warning @?= 30
        severityPriority Info @?= 10
    ]

  , testGroup "ErrorLocation properties"
    [ fastProperty "location range is valid" prop_locationRangeValid
    , fastProperty "atLocation creates correct location" prop_atLocationCreatesCorrectLocation
    , fastProperty "atFileLocation creates correct location" prop_atFileLocationCreatesCorrectLocation
    , fastProperty "atRange creates correct location" prop_atRangeCreatesCorrectLocation
    , testCase "_unknownLocation has correct values" $ do
        filePath _unknownLocation @?= Nothing
        line _unknownLocation @?= 0
        column _unknownLocation @?= 0
        endLine _unknownLocation @?= Nothing
        endColumn _unknownLocation @?= Nothing
    ]

  , testGroup "ErrorRecovery properties"
    [ fastProperty "fatal recovery cannot recover" prop_fatalRecoveryCannotRecover
    , fastProperty "custom recovery preserves values" prop_customRecoveryPreservesValues
    , testCase "built-in recovery strategies have correct properties" $ do
        canRecover errorRecovery @?= True
        shouldContinue errorRecovery @?= True
        
        canRecover warningRecovery @?= True
        shouldContinue warningRecovery @?= True
        
        canRecover infoRecovery @?= True
        shouldContinue infoRecovery @?= True
        
        canRecover fatalRecovery @?= False
        shouldContinue fatalRecovery @?= False
    ]

  , testGroup "TypeError properties"
    [ fastProperty "errorAt creates correct error" prop_errorAtCreatesCorrectError
    , fastProperty "errorWithCategory sets correct category" prop_errorWithCategorySetsCorrectCategory
    , fastProperty "warningAt has warning severity" prop_warningAtHasWarningSeverity
    , fastProperty "infoAt has info severity" prop_infoAtHasInfoSeverity
    , fastProperty "fatalError has fatal severity" prop_fatalErrorHasFatalSeverity
    , fastProperty "withLocation updates location" prop_withLocationUpdatesLocation
    , fastProperty "withContext updates context" prop_withContextUpdatesContext
    , fastProperty "withSuggestions appends suggestions" prop_withSuggestionsAppendsSuggestions
    , fastProperty "wrapError adds to message" prop_wrapErrorAddsToMessage
    , fastProperty "wrapError adds to chain" prop_wrapErrorAddsToChain
    , testCase "errorWithSuggestions creates error with suggestions" $ do
        let err = errorWithSuggestions "test001" "Test error" ["suggestion1", "suggestion2"] _unknownLocation
        suggestions err @?= [T.pack "suggestion1", T.pack "suggestion2"]
    ]

  , testGroup "Error filtering and analysis properties"
    [ fastProperty "hasCategory checks correctly" prop_hasCategoryChecksCorrectly
    , fastProperty "filterByCategory returns only matching" prop_filterByCategoryReturnsOnlyMatching
    , fastProperty "filterBySeverity returns only matching" prop_filterBySeverityReturnsOnlyMatching
    , fastProperty "getErrorStatistics counts correctly" prop_getErrorStatisticsCountsCorrectly
    , fastProperty "getErrors returns only errors and fatals" prop_getErrorsReturnsOnlyErrorsAndFatals
    , fastProperty "getWarnings returns only warnings" prop_getWarningsReturnsOnlyWarnings
    , fastProperty "getInfo returns only info" prop_getInfoReturnsOnlyInfo
    , fastProperty "hasErrors detects errors correctly" prop_hasErrorsDetectsErrorsCorrectly
    , fastProperty "hasWarnings detects warnings correctly" prop_hasWarningsDetectsWarningsCorrectly
    , testCase "getAllMessages returns all messages" $ do
        let errors = [ errorAt "e1" "error1" _unknownLocation
                     , warningAt "w1" "warning1" _unknownLocation
                     , infoAt "i1" "info1" _unknownLocation
                     ]
        getAllMessages errors @?= errors
    ]

  , testGroup "CombinedError properties"
    [ fastProperty "combined error severity matches" prop_combinedErrorSeverityMatches
    , fastProperty "filter combined errors by severity works" prop_filterCombinedErrorsBySeverityWorks
    , testCase "combined error creation works" $ do
        let ownErr = OwnershipErrorCombined Error undefined
        combinedErrorSeverity ownErr @?= Error
        
        let depErr = DependentTypeErrorCombined Warning undefined
        combinedErrorSeverity depErr @?= Warning
        
        let intErr = IntegrationError "test" Fatal
        combinedErrorSeverity intErr @?= Fatal
    ]

  , testGroup "Error formatting properties"
    [ fastProperty "formatError includes severity" prop_formatErrorIncludesSeverity
    , fastProperty "formatError includes category" prop_formatErrorIncludesCategory
    , fastProperty "formatErrorWithLocation includes location" prop_formatErrorWithLocationIncludesLocation
    , fastProperty "formatErrors includes all errors" prop_formatErrorsIncludesAllErrors
    , testCase "formatError handles empty suggestions" $ do
        let err = errorAt "test" "message" _unknownLocation
        let formatted = formatError err
        assertBool "should not include suggestions section" $ not $ "Suggestions:" `isInfixOf` formatted
    , testCase "formatError includes suggestions when present" $ do
        let err = errorWithSuggestions "test" "message" ["suggestion1"] _unknownLocation
        let formatted = formatError err
        assertBool "should include suggestions section" $ "Suggestions:" `isInfixOf` formatted
    , testCase "generateErrorReport includes statistics" $ do
        let errors = [ errorAt "e1" "error1" _unknownLocation
                     , warningAt "w1" "warning1" _unknownLocation
                     ]
        let report = generateErrorReport errors
        assertBool "should include statistics" $ "Statistics:" `isInfixOf` report
        assertBool "should include error count" $ "errors: 1" `isInfixOf` report
        assertBool "should include warning count" $ "warnings: 1" `isInfixOf` report
    ]

  , testGroup "Edge case tests"
    [ testCase "empty error list handling" $ do
        getErrors [] @?= []
        getWarnings [] @?= []
        getInfo [] @?= []
        hasErrors [] @?= False
        hasWarnings [] @?= False
        formatErrors [] @?= ""
        generateErrorReport [] `assertBool` "should generate report even for empty list" True

    , testCase "error with maximum values" $ do
        let loc = _atRange 1 1 999999 999999
        let err = errorAt "max" "max error" loc
        line (location err) @?= 1
        column (location err) @?= 1
        endLine (location err) @?= Just 999999
        endColumn (location err) @?= Just 999999

    , testCase "error with empty message" $ do
        let err = errorAt "empty" "" _unknownLocation
        T.unpack (message err) @?= ""

    , testCase "error with empty ID" $ do
        let err = errorAt "" "test message" _unknownLocation
        errorId err @?= ""

    , testCase "recovery with extreme values" $ do
        let rec = customRecovery True True (Just "action") (Just "hint") 100 0.0
        canRecover rec @?= True
        shouldContinue rec @?= True
        recoveryCost rec @?= 100
        recoveryConfidence rec @?= 0.0

    , testCase "context with all fields" $ do
        let ctx = ErrorContext (Just "code") (Just "function") (Just "variable") (Just "type") [("key1", "value1")]
        let err = errorAt "ctx" "test" _unknownLocation `withContext` ctx
        contextFunction (context err) @?= Just "function"
        contextVariable (context err) @?= Just "variable"
        contextType (context err) @?= Just "type"
        contextCode (context err) @?= Just "code"
        contextAdditional (context err) @?= [("key1", "value1")]
    ]
  ]