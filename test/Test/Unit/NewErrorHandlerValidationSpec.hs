module Test.Unit.NewErrorHandlerValidationSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool, assertFailure
import Test.Tasty.QuickCheck (testProperty, Property, (==>), Positive)
      assertBool "Warning should not be blocking" $ not (isBlocking warning)
      assertBool "Error should be blocking" $ isBlocking error
      assertBool "Fatal should be blocking" $ isBlocking fatal
  
    ,             testCase "Error collection L.and aggregation" $ do
                  let pos1 = posAt "test.typus" 1 1
      let pos2 = posAt "test.typus" 2 1
      let err1 = basicError "First error" pos1
      let err2 = warningError "Second error" pos2
      let err3 = basicError "Third error" pos1
      
      let collection = newErrorCollection
      let collection1 = addError err1 collection
      let collection2 = addError err2 collection1
      let collection3 = addError err3 collection2
      
      getErrorCount collection3 @?= 3
      getErrorCountByType Error collection3 @?= 2
      getErrorCountByType Warning collection3 @?= 1
      
      let errors = getAllErrors collection3
      L.length errors @?= 3
      
      let blocking = getBlockingErrors collection3
      L.length blocking @?= 2  -- err1 L.and err3 are errors, err2 is warning
  
    ,             testCase "Error context L.and suggestions" $ do
                  let pos = posAt "test.typus" 1 1
      let contextError = errorWithContext "Missing semicolon" pos "Expected ';' after statement" ["Add ';' at the end of the line", "Check if statement is complete"]
      
      getContext contextError @?= Just "Expected ';' after statement"
      getSuggestions contextError @?= ["Add ';' at the end of the line", "Check if statement is complete"]
      
      let formatted = formatErrorWithSuggestions contextError
      assertBool "Formatted error contains context" $ "Expected ';' after statement" `L.isInfixOf` formatted
      assertBool "Formatted error contains suggestions" $ "Add ';' at the end of the line" `L.isInfixOf` formatted
  
    ,             testCase "Error recovery strategies" $ do
                  let pos = posAt "test.typus" 1 1
      let recoverableError = recoverableError "Syntax error" pos SkipToken
      
      isRecoverable recoverableError @?= True
      getRecoveryStrategy recoverableError @?= Just SkipToken
      
      let nonRecoverableError = basicError "Fatal error" pos
      isRecoverable nonRecoverableError @?= False
      getRecoveryStrategy nonRecoverableError @?= Nothing
  
    ,             testCase "Error filtering L.and prioritization" $ do
                  let pos1 = posAt "test.typus" 1 1
      let pos2 = posAt "test.typus" 1 2
      let pos3 = posAt "test.typus" 2 1
      
      let warning1 = warningError "Warning 1" pos1
      let warning2 = warningError "Warning 2" pos2
      let error1 = basicError "Error 1" pos3
      let fatal1 = fatalError "Fatal 1" pos1
      
      let collection = addError warning2 $ addError fatal1 $ addError error1 $ addError warning1 newErrorCollection
      
      let critical = getCriticalErrors collection
      L.length critical @?= 1  -- Only the fatal error
      
      let byLocation = getErrorsByLocation collection pos1
      L.length byLocation @?= 2  -- warning1 L.and fatal1
      
      let sorted = sortErrorsBySeverity collection
      getSeverity (L.head sorted) @?= Fatal
      getSeverity (last sorted) @?= Warning
  ]
  where
      (|>) = flip ($)

-- QuickCheck properties for ErrorHandler functions
prop_error_formatting_contains_location :: String -> Property
prop_error_formatting_contains_location                               msg = 
  not (null msg) ==> 
    let pos = posAt "test.typus" 1 1
                                      err = basicError msg pos
                                      formatted = formatError err
    in "test.typus:1:1" `L.isInfixOf` formatted &&
       msg `L.isInfixOf` formatted

prop_error_collection_count :: [String] -> Property
prop_error_collection_count                               msgs = 
not (null msgs) ==> 
    let collection = foldr addError newErrorCollection 
                         [basicError msg (posAt "test.typus" i 1) | (i, msg) <- zip [1..] msgs]
    in getErrorCount                               collection == L.length msgs

prop_error_severity_ordering :: Bool
                              prop_error_severity_ordering = 
let warning = warningError "test" (posAt "test.typus" 1 1)
                                    error = basicError "test" (posAt "test.typus" 1 1)
                                    fatal = fatalError "test" (posAt "test.typus" 1 1)
  in getSeverity warning < getSeverity error &&
     getSeverity error < getSeverity fatal

prop_blocking_error_filter :: [String] -> Property
prop_blocking_error_filter                               msgs = 
not (null msgs) ==> 
    let errors = [if even i then basicError msg (posAt "test.typus" i 1) 
                               else warningError msg (posAt "test.typus" i 1) 
                  | (i, msg) <- zip [1..] msgs]
                                      collection = foldr addError newErrorCollection errors
                                      blocking = getBlockingErrors collection
    in L.length                               blocking == L.length (L.filter (even . fst) (zip [1..] msgs)

prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation msg                               ctx = 
not (null msg) && not (null ctx) ==> 
    let pos = posAt "test.typus" 1 1
                                      err = errorWithContext msg pos ctx []
                                      context =  getContext err
    in property $ context == Just ctx

-- QuickCheck test suite
errorHandlerQuickCheckProperties :: TestTree
errorHandlerQuickCheckProperties = testGroup "ErrorHandler QuickCheck Properties"
  [             testProperty "error formatting contains location" prop_error_formatting_contains_location
  ,             testProperty "error collection count matches" prop_error_collection_count
  ,             testProperty "error severity ordering" prop_error_severity_ordering
  ,             testProperty "blocking error filtering" prop_blocking_error_filter
  ,             testProperty "error context preservation" prop_error_context_preservation
  ]