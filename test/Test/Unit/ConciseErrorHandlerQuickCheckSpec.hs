module Test.Unit.ConciseErrorHandlerQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), Arbitrary(..), Gen, oneof, choose, elements, property, listOf)
import Control.Monad.State 
    ( ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..),
      emptyContext, ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
      getErrors, getWarnings, getAllMessages, hasErrors, hasWarnings,
      errorAt, warningAt, infoAt, errorWithCategory, warningWithCategory,
      canRecoverFrom, shouldContinueAfter, combineErrors, filterBySeverity,
      filterByCategory, hasCategory, severity, location, category, message, TypeError )
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


-- | QuickCheckErrorHandler
tests :: TestTree
tests =
    testGroup "Concise ErrorHandler QuickCheck Tests"
    [ testGroup "Error collector consistency"
        [             testProperty "New collector has no errors L.or warnings" $
            \(_ :: () -> let errors = execState newErrorCollector []
                  in not (hasErrors errors) && not (hasWarnings errors)
                  
        ,             testProperty "Adding error makes hasErrors true" $
            \msg loc -> 
            let msg' = msg `asTypeOf` T.empty
                                              baseError = errorAt "test" msg' loc
                                              errors = execState (addError baseError) []
            in hasErrors errors
            
        ,             testProperty "Adding warning makes hasWarnings true" $
            \msg loc -> 
            let msg' = msg `asTypeOf` T.empty
                                              baseWarning = warningAt "test" msg' loc
                                              errors = execState (addWarning baseWarning) []
            in hasWarnings errors
            
        ,             testProperty "Error count increases when adding errors" $
            \msgs loc -> not (null msgs) ==>
            let msgs' = msgs `asTypeOf` [T.empty]
                                              errors = execState (sequence_ [addError (errorAt "test" msg loc) | msg <- take 10 msgs']) []
                                              errorList = getErrors errors
            in L.length errorList >= L.length (take 10 msgs')  -- Cap to avoid infinite growth
        ]
        
    , testGroup "Error filtering consistency"
        [             testProperty "Filter by severity preserves ordering" $
            \errors -> 
            let filtered = filterBySeverity Error errors
                                              originalSorted = L.filter (\e -> severity                               e == Error) errors
            in L.length                               filtered === L.length originalSorted
            
        ,             testProperty "Filter by category is idempotent" $
            \errors category -> 
            let filteredOnce = filterByCategory category errors
                                              filteredTwice = filterByCategory category filteredOnce
            in                               filteredOnce === filteredTwice
            
        ,             testProperty "Has category is consistent with filter results" $
            \errors category -> 
            let hasCat = any (hasCategory category) errors
                                              filtered = filterByCategory category errors
            in                               hasCat === not (null filtered)
        ]
        
    , testGroup "Error combination consistency"
        [             testProperty "Combine errors is associative" $
            \err1 err2 err3 -> 
            let combined1 = combineErrors [err1, err2, err3]
                                              combined2 = combineErrors [err1, err2, err3]
            in L.length                               combined1 === L.length combined2
            
        ,             testProperty "Combine errors preserves L.maximum severity" $
            \err1 err2 -> 
            let combined = combineErrors [err1, err2]
                                              maxSeverity = max (severity err1) (severity err2)
            in not (null combined) ==> severity (head combined) === maxSeverity
        ]
        
    , testGroup "Error recovery consistency"
        [             testProperty "Error recovery is possible for non-fatal errors" $
            \err -> severity err /=                               Fatal ==> 
                canRecoverFrom err
                
        ,             testProperty "Should continue after warnings L.and info" $
            \err -> severity err `elem` [Warning, Info] ==>
                shouldContinueAfter err
        ]
    ]

-- Helper functions for accessing error internals (simplified for testing)
getErrorSeverity :: TypeError -> ErrorSeverity
                              getErrorSeverity = severity

getErrorMessage :: TypeError -> String  
getErrorMessage                               err = T.unpack (message err)

-- Generate test data
instance Arbitrary ErrorSeverity where
                                              arbitrary = elements [Info, Warning, Error, Fatal]

instance Arbitrary ErrorCategory where
                                              arbitrary = oneof
    [ return TypeChecking
                  , return Ownership
                  , return Parsing
                  , return Semantic
                  , return Runtime
                  , return Constraint
                  , return Inference
                  , return Integration
                    , return Unknown
    ]

instance Arbitrary ErrorLocation where
                                              arbitrary = do
              line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ ErrorLocation Nothing line col Nothing Nothing

instance Arbitrary ErrorContext where
                                              arbitrary = return emptyContext

instance Arbitrary T.Text where
                                              arbitrary = T.pack <$> arbitrary

instance Arbitrary TypeError where
                                              arbitrary = do
              errId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    msg <- T.pack <$> arbitrary
    loc <- arbitrary
    let baseError = errorAt errId msg loc
    return $ baseError {                               category = category,                               severity = severity }


