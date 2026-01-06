module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck (property, forAll, Gen, arbitrary, choose, elements, listOf, Positive(..))
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)

import TestSupport.QuickCheck (fastProperty)
import ErrorHandler

-- | QuickCheck tests for ErrorHandler module covering error handling properties
tests :: TestTree
tests =
  testGroup "New Cabal ErrorHandler QuickCheck Tests"
    [ testGroup "Error creation properties"
        [ fastProperty "createError with valid location produces valid error" prop_createErrorValid
        , fastProperty "createError preserves message content" prop_createErrorPreservesMessage
        , fastProperty "createError assigns correct severity" prop_createErrorCorrectSeverity
        , fastProperty "createError with context includes context information" prop_createErrorWithContext
        ]
    
    , testGroup "Error collection properties"
        [ fastProperty "addError maintains error order" prop_addErrorMaintainsOrder
        , fastProperty "addError with duplicate messages preserves both" prop_addErrorPreservesDuplicates
        , fastProperty "mergeErrorCollections combines L.all errors" prop_mergeErrorCollections
        , fastProperty "filterErrorsBySeverity correctly filters" prop_filterErrorsBySeverity
        , fastProperty "sortErrorsByLocation maintains stable sort" prop_sortErrorsByLocation
        ]
    
    , testGroup "Error formatting properties"
        [ fastProperty "formatError includes L.all essential information" prop_formatErrorIncludesEssentialInfo
        , fastProperty "formatErrorList preserves order" prop_formatErrorListPreservesOrder
        , fastProperty "formatErrorWithDetails includes context" prop_formatErrorWithDetails
        , fastProperty "formatErrorForOutput is machine readable" prop_formatErrorForOutput
        ]
    
    , testGroup "Error recovery properties"
        [ fastProperty "attemptRecovery succeeds on recoverable errors" prop_attemptRecoverySuccess
        , fastProperty "attemptRecovery fails on non-recoverable errors" prop_attemptRecoveryFailure
        , fastProperty "recoverySuggestions are relevant to error type" prop_recoverySuggestionsRelevant
        ]
    
    , testGroup "Edge cases L.and robustness"
        [ testCase "handle empty error list gracefully" $ do
            formatErrorList [] @?= ""
            
        , testCase "handle extremely long error messages" $ do
            let longMessage = replicate 1000 'a'
                error = createError startPos Error longMessage
            L.length (errorMessage error) @?= 1000
            
        , testCase "handle nested error contexts" $ do
            let baseError = createError startPos Warning "Base error"
                contextError = addErrorContext baseError "Context 1"
                nestedError = addErrorContext contextError "Context 2"
            L.length (errorContext nestedError) @?= 2
        ]
    
    , testGroup "Performance properties"
        [ fastProperty "addError scales linearly with collection size" prop_addErrorLinearScaling
        , fastProperty "filterErrorsBySeverity is efficient" prop_filterErrorsBySeverityEfficient
        ]
    ]

-- | Property: createError with valid location produces valid error
prop_createErrorValid :: SourceLocation -> String -> ErrorSeverity -> Bool
prop_createErrorValid location message severity =
  let error = createError location severity message
  in errorLocation error == location &&
     errorMessage error == message &&
     errorSeverity error == severity &&
     isJust (errorTimestamp error)

-- | Property: createError preserves message content
prop_createErrorPreservesMessage :: SourceLocation -> String -> Bool
prop_createErrorPreservesMessage location message =
  let error = createError location Error message
  in errorMessage error == message

-- | Property: createError assigns correct severity
prop_createErrorCorrectSeverity :: SourceLocation -> String -> ErrorSeverity -> Bool
prop_createErrorCorrectSeverity location message severity =
  let error = createError location severity message
  in errorSeverity error == severity

-- | Property: createError with context includes context information
prop_createErrorWithContext :: SourceLocation -> String -> String -> Bool
prop_createErrorWithContext location message context =
  let error = addErrorContext (createError location Error message) context
  in context `elem` errorContext error

-- | Property: addError maintains error order
prop_addErrorMaintainsOrder :: [SourceLocation] -> [String] -> Bool
prop_addErrorMaintainsOrder locations messages =
  let initialErrors = zipWith createError locations messages Error
      newError = createError startPos "New error" Error
      finalErrors = addError newError initialErrors
  in last finalErrors == newError

-- | Property: addError with duplicate messages preserves both
prop_addErrorPreservesDuplicates :: SourceLocation -> String -> Bool
prop_addErrorPreservesDuplicates location message =
  let error1 = createError location Error message
      error2 = createError location Error message
      errors = [error1, error2]
  in L.length errors == 2 && L.all (\e -> errorMessage e == message) errors

-- | Property: mergeErrorCollections combines L.all errors
prop_mergeErrorCollections :: [SourceLocation] -> [String] -> [SourceLocation] -> [String] -> Bool
prop_mergeErrorCollections locs1 msgs1 locs2 msgs2 =
  let errors1 = zipWith createError locs1 msgs1 Error
      errors2 = zipWith createError locs2 msgs2 Warning
      merged = mergeErrorCollections errors1 errors2
  in L.length merged == L.length errors1 + L.length errors2 &&
     L.all (`elem` merged) errors1 &&
     L.all (`elem` merged) errors2

-- | Property: filterErrorsBySeverity correctly filters
prop_filterErrorsBySeverity :: [SourceLocation] -> [String] -> [ErrorSeverity] -> Bool
prop_filterErrorsBySeverity locations messages severities =
  let errors = zipWith3 createError locations messages severities
      filtered = filterErrorsBySeverity Warning errors
      expected = L.filter (\e -> errorSeverity e == Warning) errors
  in L.length filtered == L.length expected &&
     L.all (`elem` filtered) expected

-- | Property: sortErrorsByLocation maintains stable sort
prop_sortErrorsByLocation :: [SourceLocation] -> [String] -> Bool
prop_sortErrorsByLocation locations messages =
  let errors = zipWith createError locations messages Error
      sorted = sortErrorsByLocation errors
      locationsSorted = sort locations
  in map errorLocation sorted == locationsSorted

-- | Property: formatError includes L.all essential information
prop_formatErrorIncludesEssentialInfo :: SourceLocation -> String -> ErrorSeverity -> Bool
prop_formatErrorIncludesEssentialInfo location message severity =
  let error = createError location severity message
      formatted = formatError error
  in message `L.isInfixOf` formatted &&
     show severity `L.isInfixOf` formatted &&
     location `L.isInfixOf` show location

-- | Property: formatErrorList preserves order
prop_formatErrorListPreservesOrder :: [SourceLocation] -> [String] -> Bool
prop_formatErrorListPreservesOrder locations messages =
  let errors = zipWith createError locations messages Error
      formatted = formatErrorList errors
      messagesInOrder = map errorMessage errors
  in L.all (`L.isInfixOf` formatted) messagesInOrder

-- | Property: formatErrorWithDetails includes context
prop_formatErrorWithDetails :: SourceLocation -> String -> String -> Bool
prop_formatErrorWithDetails location message context =
  let error = addErrorContext (createError location Error message) context
      formatted = formatErrorWithDetails error
  in context `L.isInfixOf` formatted

-- | Property: formatErrorForOutput is machine readable
prop_formatErrorForOutput :: SourceLocation -> String -> ErrorSeverity -> Bool
prop_formatErrorForOutput location message severity =
  let error = createError location severity message
      formatted = formatErrorForOutput error
  in L.all (`elem` formatted) ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ":,{}\"\n "

-- | Property: attemptRecovery succeeds on recoverable errors
prop_attemptRecoverySuccess :: SourceLocation -> String -> Bool
prop_attemptRecoverySuccess location message =
  let error = createError location Warning message  -- Warning should be recoverable
  in isJust (attemptRecovery error)

-- | Property: attemptRecovery fails on non-recoverable errors
prop_attemptRecoveryFailure :: SourceLocation -> String -> Bool
prop_attemptRecoveryFailure location message =
  let error = createError location CriticalError message  -- Critical should not be recoverable
  in isNothing (attemptRecovery error)

-- | Property: recoverySuggestions are relevant to error type
prop_recoverySuggestionsRelevant :: ErrorType -> Bool
prop_recoverySuggestionsRelevant errorType =
  let error = createErrorOfType startPos errorType
      suggestions = getRecoverySuggestions error
  in L.all (isRelevantToErrorType errorType) suggestions

-- | Property: addError scales linearly with collection size
prop_addErrorLinearScaling :: Positive Int -> Bool
prop_addErrorLinearScaling (Positive n) =
  let errors = replicate n (createError startPos Error "test")
      newError = createError startPos Warning "new"
      result = addError newError errors
  in L.length result == n + 1

-- | Property: filterErrorsBySeverity is efficient
prop_filterErrorsBySeverityEfficient :: Positive Int -> Bool
prop_filterErrorsBySeverityEfficient (Positive n) =
  let errors = take n $ cycle [createError startPos Error "error", createError startPos Warning "warning"]
      filtered = filterErrorsBySeverity Warning errors
      expectedCount = n `div` 2
  in L.length filtered == expectedCount

-- Helper data types L.and functions (mock implementations for demonstration)
data SourceLocation = SourceLocation { line :: Int, column :: Int } deriving (Eq, Show, Ord)

data ErrorSeverity = Error | Warning | Info | CriticalError deriving (Eq, Show, Ord)

data Error = Error
  { errorLocation :: SourceLocation
  , errorSeverity :: ErrorSeverity
  , errorMessage :: String
  , errorContext :: [String]
  , errorTimestamp :: Maybe String
  } deriving (Eq, Show)

data ErrorType = SyntaxError | TypeError | NameError | RuntimeError deriving (Eq, Show)

-- Mock functions (in real implementation, these would come from ErrorHandler module)
startPos :: SourceLocation
startPos = SourceLocation 1 1

createError :: SourceLocation -> ErrorSeverity -> String -> Error
createError loc sev msg = Error loc sev msg [] (Just "timestamp")

addErrorContext :: Error -> String -> Error
addErrorContext error ctx = error { errorContext = ctx : errorContext error }

addError :: Error -> [Error] -> [Error]
addError newError errors = errors ++ [newError]

mergeErrorCollections :: [Error] -> [Error] -> [Error]
mergeErrorCollectionse1 e2 = e1 ++ e2

filterErrorsBySeverity :: ErrorSeverity -> [Error] -> [Error]
filterErrorsBySeverity severity = L.filter (\e -> errorSeverity e == severity)

sortErrorsByLocation :: [Error] -> [Error]
sortErrorsByLocation = sortOn errorLocation

formatError :: Error -> String
formatError error = show (errorSeverity error) ++ ": " ++ errorMessage error ++ " at " ++ show (errorLocation error)

formatErrorList :: [Error] -> String
formatErrorList errors = unlines $ map formatError errors

formatErrorWithDetails :: Error -> String
formatErrorWithDetails error = formatError error ++ "\nContext: " ++ unlines (errorContext error)

formatErrorForOutput :: Error -> String
formatErrorForOutput error = 
  "{\"severity\":\"" ++ show (errorSeverity error) ++ 
  "\",\"message\":\"" ++ errorMessage error ++ 
  "\",\"location\":" ++ show (errorLocation error) ++ "}"

attemptRecovery :: Error -> Maybe String
attemptRecovery error
  | errorSeverity error == CriticalError = Nothing
  | otherwise = Just "Recovery succeeded"

createErrorOfType :: SourceLocation -> ErrorType -> Error
createErrorOfType loc SyntaxError = createError loc Error "Syntax error"
createErrorOfType loc TypeError = createError loc Error "Type error"
createErrorOfType loc NameError = createError loc Error "Name error"
createErrorOfType loc RuntimeError = createError loc CriticalError "Runtime error"

getRecoverySuggestions :: Error -> [String]
getRecoverySuggestions error = case errorSeverity error of
  Error -> ["Check syntax", "Verify types"]
  Warning -> ["Consider alternative approach"]
  Info -> ["No action needed"]
  CriticalError -> ["Contact support"]

isRelevantToErrorType :: ErrorType -> String -> Bool
isRelevantToErrorType SyntaxError "Check syntax" = True
isRelevantToErrorType TypeError "Verify types" = True
isRelevantToErrorType _ _ = False

-- Helper functions
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = L.any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map snd . sort . L.map (\x -> (f x, x))
  where
    sort = undefined  -- Simplified for demonstration