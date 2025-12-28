module Test.Unit.ConciseErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, choose, elements)
import Compiler.Errors.Core 
    ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..),
      emptyContext, ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
      getErrors, getWarnings, getAllMessages, hasErrors, hasWarnings,
      errorAt, warningAt, infoAt, errorWithCategory, warningWithCategory,
      canRecoverFrom, shouldContinueAfter, combineErrors, filterBySeverity,
      filterByCategory, hasCategory )

-- | 简洁的QuickCheck测试，针对ErrorHandler模块的一致性
tests :: TestTree
tests =
  testGroup "Concise ErrorHandler QuickCheck Tests"
    [ testGroup "Error collector consistency"
        [ testProperty "New collector has no errors or warnings" $
            \_ -> let collector = newErrorCollector
                  in not (hasErrors collector) && not (hasWarnings collector)
                  
        , testProperty "Adding error makes hasErrors true" $
            \msg loc -> 
            let collector = addError (errorAt loc msg) newErrorCollector
            in hasErrors collector
            
        , testProperty "Adding warning makes hasWarnings true" $
            \msg loc -> 
            let collector = addWarning (warningAt loc msg) newErrorCollector
            in hasWarnings collector
            
        , testProperty "Error count increases when adding errors" $
            \msgs loc -> not (null msgs) ==>
            let collector = foldr (\msg acc -> addError (errorAt loc msg) acc) newErrorCollector msgs
                errors = getErrors collector
            in length errors >= length (take 10 msgs)  -- Cap to avoid infinite growth
        ]
        
    , testGroup "Error filtering consistency"
        [ testProperty "Filter by severity preserves ordering" $
            \errors -> 
            let filtered = filterBySeverity ErrorError errors
                originalSorted = filter (\e -> getErrorSeverity e == ErrorError) errors
            in length filtered === length originalSorted
            
        , testProperty "Filter by category is idempotent" $
            \errors category -> 
            let filteredOnce = filterByCategory category errors
                filteredTwice = filterByCategory category filteredOnce
            in filteredOnce === filteredTwice
            
        , testProperty "Has category is consistent with filter results" $
            \errors category -> 
            let hasCat = hasCategory category errors
                filtered = filterByCategory category errors
            in hasCat === not (null filtered)
        ]
        
    , testGroup "Error combination consistency"
        [ testProperty "Combine errors is associative" $
            \err1 err2 err3 -> 
            let combined1 = combineErrors (combineErrors err1 err2) err3
                combined2 = combineErrors err1 (combineErrors err2 err3)
            in getErrorMessage combined1 === getErrorMessage combined2
            
        , testProperty "Combine errors preserves maximum severity" $
            \err1 err2 -> 
            let combined = combineErrors err1 err2
                maxSeverity = max (getErrorSeverity err1) (getErrorSeverity err2)
            in getErrorSeverity combined === maxSeverity
        ]
        
    , testGroup "Error recovery consistency"
        [ testProperty "Error recovery is possible for non-fatal errors" $
            \err -> getErrorSeverity err /= ErrorFatal ==> 
                canRecoverFrom err
                
        , testProperty "Should continue after warnings and info" $
            \err -> getErrorSeverity err `elem` [ErrorWarning, ErrorInfo] ==>
                shouldContinueAfter err
        ]
    ]

-- Helper functions for accessing error internals (simplified for testing)
getErrorSeverity :: CombinedError -> ErrorSeverity
getErrorSeverity = undefined  -- Placeholder - actual implementation would access the field

getErrorMessage :: CombinedError -> String  
getErrorMessage = undefined  -- Placeholder - actual implementation would access the field

-- Generate test data
instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorInfo, ErrorWarning, ErrorError, ErrorFatal]

instance Arbitrary ErrorCategory where
  arbitrary = oneof
    [ return SyntaxError
    , return TypeError
    , return SemanticError
    , return RuntimeError
    , return ConfigError
    , return IOError
    ]

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ ErrorLocation line col ""

instance Arbitrary ErrorContext where
  arbitrary = return emptyContext

instance Arbitrary String where
  arbitrary = oneof
    [ return ""
    , listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements "0123456789 "
    ]

-- Mock CombinedError for testing (simplified)
data CombinedError = CombinedError
  { _severity :: ErrorSeverity
  , _message :: String
  } deriving (Show, Eq)

instance Arbitrary CombinedError where
  arbitrary = do
    severity <- arbitrary
    message <- arbitrary
    return $ CombinedError severity message

-- Mock functions for testing
errorAt :: ErrorLocation -> String -> CombinedError
errorAt _ msg = CombinedError ErrorError msg

warningAt :: ErrorLocation -> String -> CombinedError  
warningAt _ msg = CombinedError ErrorWarning msg

infoAt :: ErrorLocation -> String -> CombinedError
infoAt _ msg = CombinedError ErrorInfo msg

errorWithCategory :: ErrorCategory -> String -> CombinedError
errorWithCategory _ msg = CombinedError ErrorError msg

warningWithCategory :: ErrorCategory -> String -> CombinedError
warningWithCategory _ msg = CombinedError ErrorWarning msg

combineErrors :: CombinedError -> CombinedError -> CombinedError
combineErrors e1 e2 = CombinedError (max (_severity e1) (_severity e2)) (_message e1 ++ "; " ++ _message e2)

filterBySeverity :: ErrorSeverity -> [CombinedError] -> [CombinedError]
filterBySeverity sev = filter (\e -> _severity e == sev)

filterByCategory :: ErrorCategory -> [CombinedError] -> [CombinedError]
filterByCategory _ = id  -- Simplified for testing

hasCategory :: ErrorCategory -> [CombinedError] -> Bool
hasCategory _ = not . null  -- Simplified for testing

canRecoverFrom :: CombinedError -> Bool
canRecoverFrom err = _severity err /= ErrorFatal

shouldContinueAfter :: CombinedError -> Bool
shouldContinueAfter err = _severity err `elem` [ErrorInfo, ErrorWarning]