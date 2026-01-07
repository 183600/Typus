module Test.Unit.NewCompactErrorHandlerSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import ErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedAt, spanFrom)
import qualified Data.List as L
import Data.List 
              len <- choose (1, 50)
    elements $ L.map (:[]) ['a'..'z'] >>= \c -> 
      return $ L.concat (replicate len c)
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


-- | 
genErrorPos :: Gen SourcePos
                              genErrorPos = do
              line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SourcePos line col

-- | 
testErrorCreation :: TestTree
testErrorCreation = testGroup ""
  [             testCase "" $
      let msg = "Test error message"
                                        pos = SourcePos 1 1
                                        error = createBasicError msg pos
      in assertBool "" (msg `L.isInfixOf` formatError error)
    
    ,             testCase "" $
      let msg = "Position error"
                                        pos = SourcePos 5 10
                                        error = createErrorWithLocation msg pos
                                        formatted = formatError error
      in assertBool "" ("5:10" `L.isInfixOf` formatted)
    
    ,             testCase "" $
      let msg = "Span error"
                                        span = SourceSpan (SourcePos 3 1) (SourcePos 3 10)
                                        error = createErrorWithSpan msg span
                                        formatted = formatError error
      in assertBool "" ("3:1-3:10" `L.isInfixOf` formatted)
  ]

-- | 
testErrorClassification :: TestTree
testErrorClassification = testGroup ""
  [             testCase "" $
      let error = createSyntaxError "Missing semicolon" (SourcePos 2 5)
      in assertBool "" (isSyntaxError error)
    
    ,             testCase "" $
      let error = createSemanticError "Type mismatch" (SourcePos 1 10)
      in assertBool "" (isSemanticError error)
    
    ,             testCase "" $
      let warning = createWarning "Unused variable" (SourcePos 3 8)
      in assertBool "" (isWarning warning)
    
    ,             testCase "" $
      let fatal = createFatalError "Stack overflow" (SourcePos 10 1)
      in assertBool "" (isFatalError fatal)
  ]

-- | 
testErrorAggregation :: TestTree
testErrorAggregation = testGroup ""
  [             testCase "" $
      let errors = [ createBasicError "Error 1" (SourcePos 1 1)
                   , createBasicError "Error 2" (SourcePos 2 2)
                   , createBasicError "Error 3" (SourcePos 3 3)
                   ]
                                        aggregated = aggregateErrors errors
      in assertBool "" (L.length                               aggregated == L.length errors)
    
    ,             testCase "" $
      let errors = [ createBasicError "Error 1" (SourcePos 3 3)
                   , createBasicError "Error 2" (SourcePos 1 1)
                   , createBasicError "Error 3" (SourcePos 2 2)
                   ]
                                        sorted = sortErrorsByPosition errors
                                        positions = map getErrorPosition sorted
      in positions @?= [SourcePos 1 1, SourcePos 2 2, SourcePos 3 3]
    
    ,             testCase "" $
      let errors = [ createSyntaxError "Syntax" (SourcePos 1 1)
                   , createSemanticError "Semantic" (SourcePos 2 2)
                   , createWarning "Warning" (SourcePos 3 3)
                   ]
                                        syntaxErrors = filterByErrorType isSyntaxError errors
      in L.length syntaxErrors @?= 1
  ]

-- | 
testErrorRecovery :: TestTree
testErrorRecovery = testGroup ""
  [             testCase "" $
      let error = createBasicError "Recoverable error" (SourcePos 1 1)
                                        recoveryPoint = createRecoveryPoint (SourcePos 1 5)
                                        canRecover = canRecoverFromError error recoveryPoint
      in assertBool "" canRecover
    
    ,             testCase "" $
      let fatal = createFatalError "Fatal error" (SourcePos 1 1)
                                        recoveryPoint = createRecoveryPoint (SourcePos 1 5)
                                        canRecover = canRecoverFromError fatal recoveryPoint
      in assertBool "" (not canRecover)
    
    ,             testCase "" $
      let errors = [ createBasicError "Error 1" (SourcePos 1 1)
                   , createFatalError "Fatal" (SourcePos 2 2)
                   , createBasicError "Error 2" (SourcePos 3 3)
                   ]
                                        strategy = determineRecoveryStrategy errors
      in assertBool "" (strategy == StopCompilation)
  ]

-- | QuickCheck
testErrorHandlerProperties :: TestTree
testErrorHandlerProperties = testGroup ""
  [             testProperty "" $
      forAll genErrorPos $ \pos ->
        let msg = "Test message"
                                          error = createBasicError msg pos
                                          formatted = formatError error
                                          posStr = show (spLine pos) ++ ":" ++ show (spColumn pos)
        in posStr `L.isInfixOf` formatted
  
  ,             testProperty "" $
      \errors -> 
        let aggregated = aggregateErrors errors
        in L.length                               aggregated === L.length errors
  
  ,             testProperty "" $
      \errors ->
        let sorted = sortErrorsByPosition errors
                                          positions = map getErrorPosition sorted
            isMonotonic [] = True
            isMonotonic [_] = True
            isMonotonic (p1:p2:ps) = p1 <= p2 && isMonotonic (p2:ps)
        in isMonotonic positions
  ]

-- | 
testErrorContext :: TestTree
testErrorContext = testGroup ""
  [             testCase "" $
      let error = createBasicError "Error" (SourcePos 2 5)
                                        context = getErrorContext ["line 1", "line 2 with error", "line 3"] error
      in assertBool "" ("line 2 with error" `L.isInfixOf` context)
    
    ,             testCase "" $
      let error = createBasicError "Error" (SourcePos 2 10)
                                        context = getErrorContext ["line 2 with error here"] error
      in assertBool "" ("^" `L.isInfixOf` context)
    
    ,             testCase "" $
      let error = createErrorWithSpan "Multi-line error" 
                   (SourceSpan (SourcePos 2 1) (SourcePos 4 10)
                                        context = getErrorContext ["line 1", "line 2", "line 3", "line 4", "line 5"] error
      in assertBool "" ("line 2" `L.isInfixOf` context && "line 4" `L.isInfixOf` context)
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let aggregated = aggregateErrors []
      in L.length aggregated @?= 0
    
    ,             testCase "" $
      let error = createBasicError "" (SourcePos 1 1)
                                        formatted = formatError error
      in assertBool "" ("1:1" `L.isInfixOf` formatted)
    
    ,             testCase "" $
      let pos = SourcePos 999999 999999
                                        error = createBasicError "Large position" pos
                                        formatted = formatError error
      in assertBool "" ("999999:999999" `L.isInfixOf` formatted)
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "" $
      \n ->
        let numErrors = min 1000 (max 1 n)
                                          errors = replicate numErrors (createBasicError "Test" (SourcePos 1 1)
                                          aggregated = aggregateErrors errors
        in L.length                               aggregated === numErrors
  ]

-- | 
tests :: TestTree
tests =   testGroup "ErrorHandler"
  [ testErrorCreation
  , testErrorClassification
  , testErrorAggregation
  , testErrorRecovery
  , testErrorHandlerProperties
  , testErrorContext
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- ErrorHandler
createBasicError :: String -> SourcePos -> ErrorHandler
createBasicError msg                               pos = undefined  -- 

createErrorWithLocation :: String -> SourcePos -> ErrorHandler
createErrorWithLocation msg                               pos = undefined  -- 

createErrorWithSpan :: String -> SourceSpan -> ErrorHandler
createErrorWithSpan msg                               span = undefined  -- 

createSyntaxError :: String -> SourcePos -> ErrorHandler
createSyntaxError msg                               pos = undefined  -- 

createSemanticError :: String -> SourcePos -> ErrorHandler
createSemanticError msg                               pos = undefined  -- 

createWarning :: String -> SourcePos -> ErrorHandler
createWarning msg                               pos = undefined  -- 

createFatalError :: String -> SourcePos -> ErrorHandler
createFatalError msg                               pos = undefined  -- 

formatError :: ErrorHandler -> String
formatError                               error = undefined  -- 

isSyntaxError :: ErrorHandler -> Bool
isSyntaxError                               error = undefined  -- 

isSemanticError :: ErrorHandler -> Bool
isSemanticError                               error = undefined  -- 

isWarning :: ErrorHandler -> Bool
isWarning                               error = undefined  -- 

isFatalError :: ErrorHandler -> Bool
isFatalError                               error = undefined  -- 

aggregateErrors :: [ErrorHandler] -> [ErrorHandler]
aggregateErrors                               errors = undefined  -- 

sortErrorsByPosition :: [ErrorHandler] -> [ErrorHandler]
sortErrorsByPosition                               errors = undefined  -- 
filterByErrorType :: (ErrorHandler -> Bool) -> [ErrorHandler] -> [ErrorHandler]
filterByErrorType predicate                               errors = undefined  -- 

getErrorPosition :: ErrorHandler -> SourcePos
getErrorPosition                               error = undefined  -- 

createRecoveryPoint :: SourcePos -> RecoveryPoint
createRecoveryPoint                               pos = undefined  -- 

canRecoverFromError :: ErrorHandler -> RecoveryPoint -> Bool
canRecoverFromError error                               recoveryPoint = undefined  -- 

determineRecoveryStrategy :: [ErrorHandler] -> RecoveryStrategy
determineRecoveryStrategy                               errors = undefined  -- 
getErrorContext :: [String] -> ErrorHandler -> String
getErrorContext lines                               error = undefined  -- 

-- 
type                               ErrorHandler = String
type                               RecoveryPoint = SourcePos
data                               RecoveryStrategy = StopCompilation | ContinueCompilation deriving (Eq, Show)