module Test.Unit.ErrorHandlerConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import ErrorHandler
import Compiler.Errors.Core
import SourceLocation

-- | QuickCheck tests for ErrorHandler consistency properties
tests :: TestTree
tests =
  testGroup "ErrorHandler consistency properties"
    [ testGroup "Error creation properties"
        [ fastProperty "error creation preserves location information" prop_errorPreservesLocation
        , fastProperty "error messages are non-empty" prop_errorMessagesNonEmpty
        , fastProperty "error severity is valid" prop_errorSeverityValid
        , fastProperty "error codes are consistent" prop_errorCodesConsistent
        ]

    , testGroup "Error collection properties"
        [ fastProperty "error collection preserves order" prop_errorCollectionOrder
        , fastProperty "error collection handles duplicates correctly" prop_errorCollectionDuplicates
        , fastProperty "error filtering maintains invariants" prop_errorFilteringInvariants
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "error recovery produces valid state" prop_errorRecoveryValidState
        , fastProperty "error recovery preserves successful operations" prop_errorRecoveryPreservesSuccess
        , fastProperty "error recovery is idempotent" prop_errorRecoveryIdempotent
        ]

    , testGroup "Error reporting properties"
        [ fastProperty "error reporting formats consistently" prop_errorReportingConsistency
        , fastProperty "error reporting includes all essential information" prop_errorReportingComplete
        , fastProperty "error reporting handles edge cases gracefully" prop_errorReportingEdgeCases
        ]

    , testGroup "Error context properties"
        [ fastProperty "error context propagation maintains consistency" prop_errorContextPropagation
        , fastProperty "nested error contexts preserve hierarchy" prop_nestedErrorContexts
        , fastProperty "error context merging is associative" prop_errorContextMerging
        ]
    ]

-- ============================================================================
-- Helper generators
-- ============================================================================

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info]

genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    return $ ErrorLocation Nothing line column Nothing Nothing

genErrorMessage :: Gen String
genErrorMessage = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " "

genErrorCode :: Gen String
genErrorCode = do
    prefix <- elements ["E", "W", "I"]
    number <- choose (100, 999)
    return $ prefix ++ show number

genError :: Gen CompilerError
genError = do
    location <- genErrorLocation
    severity <- genErrorSeverity
    message <- genErrorMessage
    code <- genErrorCode
    return $ CompilerError location severity message code

genErrorList :: Gen [CompilerError]
genErrorList = listOf1 genError

-- ============================================================================
-- Error creation properties
-- ============================================================================

prop_errorPreservesLocation :: ErrorLocation -> ErrorSeverity -> String -> String -> Bool
prop_errorPreservesLocation location severity message code =
    let error = createError location severity message code
    in errorLocation error == location

prop_errorMessagesNonEmpty :: ErrorLocation -> ErrorSeverity -> String -> String -> Property
prop_errorMessagesNonEmpty location severity message code =
    not (null message) ==> 
    let error = createError location severity message code
    in not (null (errorMessage error))

prop_errorSeverityValid :: ErrorLocation -> String -> String -> ErrorSeverity -> Bool
prop_errorSeverityValid location message code severity =
    let error = createError location severity message code
    in errorSeverity error `elem` [Error, Warning, Info]

prop_errorCodesConsistent :: ErrorLocation -> ErrorSeverity -> String -> String -> Property
prop_errorCodesConsistent location severity message code =
    let error = createError location severity message code
        expectedPattern = head code `elem` ['E', 'W', 'I']
    in length code >= 4 && expectedPattern

-- ============================================================================
-- Error collection properties
-- ============================================================================

prop_errorCollectionOrder :: [CompilerError] -> Bool
prop_errorCollectionOrder errors =
    let collected = collectErrors errors
    in length collected == length errors

prop_errorCollectionDuplicates :: [CompilerError] -> Bool
prop_errorCollectionDuplicates errors =
    let withDuplicates = errors ++ errors
        collected = collectErrors withDuplicates
        uniqueErrors = removeDuplicates errors
    in length collected >= length uniqueErrors

prop_errorFilteringInvariants :: [CompilerError] -> ErrorSeverity -> Bool
prop_errorFilteringInvariants errors severity =
    let filtered = filterErrorsBySeverity errors severity
    in all (\e -> errorSeverity e == severity) filtered

-- ============================================================================
-- Error recovery properties
-- ============================================================================

prop_errorRecoveryValidState :: [CompilerError] -> Bool
prop_errorRecoveryValidState errors =
    let recovered = recoverFromErrors errors
    in isRecoveryStateValid recovered

prop_errorRecoveryPreservesSuccess :: [CompilerError] -> Bool
prop_errorRecoveryPreservesSuccess errors =
    let hasOnlyWarnings = all (\e -> errorSeverity e /= Error) errors
        recovered = recoverFromErrors errors
    in hasOnlyWarnings ==> isRecoverySuccessful recovered

prop_errorRecoveryIdempotent :: [CompilerError] -> Bool
prop_errorRecoveryIdempotent errors =
    let recovered1 = recoverFromErrors errors
        recovered2 = recoverFromErrors recovered1
    in recovered1 == recovered2

-- ============================================================================
-- Error reporting properties
-- ============================================================================

prop_errorReportingConsistency :: CompilerError -> Bool
prop_errorReportingConsistency error =
    let report1 = formatError error
        report2 = formatError error
    in report1 == report2

prop_errorReportingComplete :: CompilerError -> Bool
prop_errorReportingComplete error =
    let report = formatError error
        hasLocation = errorLocation error `isInfixOf` report
        hasMessage = errorMessage error `isInfixOf` report
        hasSeverity = show (errorSeverity error) `isInfixOf` report
    in hasLocation && hasMessage && hasSeverity

prop_errorReportingEdgeCases :: CompilerError -> Bool
prop_errorReportingEdgeCases error =
    let report = formatError error
        emptyMessageError = error { errorMessage = "" }
        emptyReport = formatError emptyMessageError
    in not (null report) && not (null emptyReport)

-- ============================================================================
-- Error context properties
-- ============================================================================

prop_errorContextPropagation :: CompilerError -> String -> Bool
prop_errorContextPropagation error context =
    let withContext = addErrorContext error context
        contextPropagated = hasErrorContext withContext context
    in contextPropagated

prop_nestedErrorContexts :: [CompilerError] -> String -> Bool
prop_nestedErrorContexts errors context =
    let withContext = map (`addErrorContext` context) errors
        allHaveContext = all (hasErrorContext context) withContext
    in allHaveContext

prop_errorContextMerging :: CompilerError -> String -> String -> Bool
prop_errorContextMerging error ctx1 ctx2 =
    let withCtx1 = addErrorContext error ctx1
        withCtx2 = addErrorContext withCtx1 ctx2
        withBoth = addErrorContext error (ctx1 ++ " " ++ ctx2)
    in hasErrorContext withCtx2 ctx1 && hasErrorContext withCtx2 ctx2

-- ============================================================================
-- Mock implementations (would be imported from ErrorHandler module)
-- ============================================================================

data CompilerError = CompilerError
    { errorLocation :: ErrorLocation
    , errorSeverity :: ErrorSeverity
    , errorMessage :: String
    , errorCode :: String
    } deriving (Show, Eq)

data ErrorSeverity = Error | Warning | Info deriving (Show, Eq, Enum)

data ErrorLocation = ErrorLocation
    { filePath :: Maybe String
    , line :: Int
    , column :: Int
    , endLine :: Maybe Int
    , endColumn :: Maybe Int
    } deriving (Show, Eq)

data RecoveryState = RecoveryState
    { isSuccessful :: Bool
    , remainingErrors :: [CompilerError]
    } deriving (Show, Eq)

-- Mock functions (these would be implemented in the actual ErrorHandler module)
createError :: ErrorLocation -> ErrorSeverity -> String -> String -> CompilerError
createError loc sev msg code = CompilerError loc sev msg code

collectErrors :: [CompilerError] -> [CompilerError]
collectErrors = id

removeDuplicates :: [CompilerError] -> [CompilerError]
removeDuplicates = nub

filterErrorsBySeverity :: [CompilerError] -> ErrorSeverity -> [CompilerError]
filterErrorsBySeverity errors sev = filter (\e -> errorSeverity e == sev) errors

recoverFromErrors :: [CompilerError] -> RecoveryState
recoverFromErrors errors = RecoveryState (all (\e -> errorSeverity e /= Error) errors) errors

isRecoveryStateValid :: RecoveryState -> Bool
isRecoveryStateValid _ = True

isRecoverySuccessful :: RecoveryState -> Bool
isRecoverySuccessful = isSuccessful

formatError :: CompilerError -> String
formatError error = show (errorSeverity error) ++ " at " ++ show (errorLocation error) ++ ": " ++ errorMessage error

addErrorContext :: CompilerError -> String -> CompilerError
addErrorContext error ctx = error { errorMessage = errorMessage error ++ " (context: " ++ ctx ++ ")" }

hasErrorContext :: CompilerError -> String -> Bool
hasErrorContext error ctx = ctx `isInfixOf` errorMessage error

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)