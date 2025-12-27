{-# LANGUAGE CPP #-}
module Test.Unit.CompilerErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, elements, listOf, choose, suchThat)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import qualified Compiler.Errors.Core as Core
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

-- | Test compiler error handling functionality
testCompilerErrorHandling :: TestTree
testCompilerErrorHandling = testGroup "Compiler Error Handling"
  [ testErrorCreation
  , testErrorSeverity
  , testErrorLocation
  , testErrorRecovery
  , testErrorChaining
  ]

-- | Test error creation and basic properties
testErrorCreation :: TestTree
testErrorCreation = testGroup "Error Creation"
  [ fastProperty "error has valid ID" prop_errorHasValidId
  , fastProperty "error has non-empty message" prop_errorHasNonEmptyMessage
  , testCase "create syntax error" testCreateSyntaxError
  , testCase "create type error" testCreateTypeError
  , testCase "create ownership error" testCreateOwnershipError
  ]

-- | Test error severity levels
testErrorSeverity :: TestTree
testErrorSeverity = testGroup "Error Severity"
  [ fastProperty "severity is properly classified" prop_severityClassification
  , testCase "error severity ordering" testSeverityOrdering
  , testCase "warning vs error distinction" testWarningVsError
  ]

-- | Test error location tracking
testErrorLocation :: TestTree
testErrorLocation = testGroup "Error Location"
  [ fastProperty "error location is valid" prop_errorLocationValid
  , fastProperty "error span contains error position" prop_errorSpanContainsPosition
  , testCase "multi-line error location" testMultiLineErrorLocation
  , testCase "error location in source context" testErrorLocationInContext
  ]

-- | Test error recovery mechanisms
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "Error Recovery"
  [ fastProperty "recovery strategy is appropriate" prop_recoveryStrategyAppropriate
  , fastProperty "recovery suggestions are helpful" prop_recoverySuggestionsHelpful
  , testCase "error recovery after syntax error" testSyntaxErrorRecovery
  , testCase "error recovery after type error" testTypeErrorRecovery
  ]

-- | Test error chaining and propagation
testErrorChaining :: TestTree
testErrorChaining = testGroup "Error Chaining"
  [ fastProperty "error chain preserves causality" prop_errorChainPreservesCausality
  , fastProperty "related errors are grouped" prop_relatedErrorsGrouped
  , testCase "cascading type errors" testCascadingTypeErrors
  , testCase "error propagation through phases" testErrorPropagation
  ]

-- | Property tests
prop_errorHasValidId :: Core.TypeError -> Property
prop_errorHasValidId typeError =
  let errorId = Core.errorId typeError
  in not (T.null errorId) === True

prop_errorHasNonEmptyMessage :: Core.TypeError -> Property
prop_errorHasNonEmptyMessage typeError =
  let message = Core.message typeError
  in not (T.null message) === True

prop_severityClassification :: Core.ErrorSeverity -> Property
prop_severityClassification severity =
  let isValidSeverity = elem severity [Core.Error, Core.Warning, Core.Info, Core.Debug]
  in isValidSeverity === True

prop_errorLocationValid :: Core.ErrorLocation -> Property
prop_errorLocationValid location =
  case location of
    Core.SourceLocation span -> isValidSpan span
    Core.VirtualLocation _ -> True  -- Virtual locations are always valid
    Core.UnknownLocation -> True   -- Unknown locations are valid by definition

prop_errorSpanContainsPosition :: SourceSpan -> SourcePos -> Property
prop_errorSpanContainsPosition span pos =
  let contains = spanContains span pos
      validSpan = isValidSpan span
  in if validSpan then contains === True else property True

prop_recoveryStrategyAppropriate :: Core.TypeError -> Property
prop_recoveryStrategyAppropriate typeError =
  let recovery = Core.recovery typeError
      severity = Core.severity typeError
      isAppropriate = case (severity, recovery) of
        (Core.Error, Core.NoRecovery) -> False  -- Errors should have recovery
        (Core.Warning, Core.NoRecovery) -> True  -- Warnings can have no recovery
        (Core.Info, Core.NoRecovery) -> True     -- Info can have no recovery
        (Core.Debug, Core.NoRecovery) -> True    -- Debug can have no recovery
        _ -> True  -- Other combinations are valid
  in isAppropriate === True

prop_recoverySuggestionsHelpful :: Core.TypeError -> Property
prop_recoverySuggestionsHelpful typeError =
  let suggestions = Core.suggestions typeError
      recovery = Core.recovery typeError
      hasSuggestions = not (null suggestions)
      needsSuggestions = case recovery of
        Core.NoRecovery -> False
        Core.SuggestFix _ -> True
        Core.SkipNode -> True
        Core.InsertToken _ -> True
        Core.ReplaceToken _ _ -> True
        Core.RetryWithAlternative _ -> True
  in if needsSuggestions then hasSuggestions === True else property True

prop_errorChainPreservesCausality :: [Core.TypeError] -> Property
prop_errorChainPreservesCausality errors =
  let hasValidChain = all (\err -> null (Core.errorChain err) || 
                              all (\cause -> Core.timestamp cause <= Core.timestamp err) (Core.errorChain err)) errors
  in hasValidChain === True

prop_relatedErrorsGrouped :: Core.TypeError -> [Core.TypeError] -> Property
prop_relatedErrorsGrouped mainError relatedErrors =
  let allRelated = all (\err -> elem err (Core.relatedErrors mainError)) relatedErrors
  in if null relatedErrors then property True else allRelated === True

-- | Unit tests
testCreateSyntaxError :: IO ()
testCreateSyntaxError = do
  let location = Core.SourceLocation $ SourceSpan (SourcePos 1 5 4) (SourcePos 1 10 9)
      typeError = Core.TypeError
        { Core.errorId = "SYNTAX_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Unexpected token"
        , Core.location = location
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Remove the unexpected token"
        , Core.suggestions = [T.pack "Remove token", T.pack "Add missing semicolon"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12345
        }
      compilerError = CompilerError
        { ceError = typeError
        , ceSourceContext = Just "func main() { x := 5 + }"
        , ceStackTrace = ["parseExpression", "parseStatement", "parseBlock"]
        , cePhase = ParsingPhase
        }
  
  assertEqual "error ID should be SYNTAX_001" "SYNTAX_001" (Core.errorId typeError)
  assertEqual "severity should be Error" Core.Error (Core.severity typeError)
  assertEqual "phase should be ParsingPhase" ParsingPhase (cePhase compilerError)
  assertBool "message should mention unexpected token" $ 
    T.isInfixOf "Unexpected token" (Core.message typeError)

testCreateTypeError :: IO ()
testCreateTypeError = do
  let location = Core.SourceLocation $ SourceSpan (SourcePos 2 10 25) (SourcePos 2 15 30)
      typeError = Core.TypeError
        { Core.errorId = "TYPE_001"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Cannot assign string to int variable"
        , Core.location = location
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Change variable type to string or value to int"
        , Core.suggestions = [T.pack "var x string", T.pack "x := 42"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12346
        }
      compilerError = CompilerError
        { ceError = typeError
        , ceSourceContext = Just "var x int = \"hello\""
        , ceStackTrace = ["typeCheckAssignment", "typeCheckStatement", "typeCheckBlock"]
        , cePhase = TypeCheckingPhase
        }
  
  assertEqual "error ID should be TYPE_001" "TYPE_001" (Core.errorId typeError)
  assertEqual "phase should be TypeCheckingPhase" TypeCheckingPhase (cePhase compilerError)
  assertBool "message should mention type mismatch" $ 
    T.isInfixOf "string to int" (Core.message typeError)

testCreateOwnershipError :: IO ()
testCreateOwnershipError = do
  let location = Core.SourceLocation $ SourceSpan (SourcePos 3 8 40) (SourcePos 3 12 44)
      typeError = Core.TypeError
        { Core.errorId = "OWNERSHIP_001"
        , Core.severity = Core.Error
        , Core.category = Core.OwnershipViolation
        , Core.message = T.pack "Cannot move value that has been borrowed"
        , Core.location = location
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Wait for borrow to end or clone the value"
        , Core.suggestions = [T.pack "use x.clone()", T.pack "move after borrow ends"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12347
        }
      compilerError = CompilerError
        { ceError = typeError
        , ceSourceContext = Just "let y = x; // x is borrowed"
        , ceStackTrace = ["checkOwnership", "analyzeMove", "ownershipAnalysis"]
        , cePhase = OwnershipAnalysisPhase
        }
  
  assertEqual "error ID should be OWNERSHIP_001" "OWNERSHIP_001" (Core.errorId typeError)
  assertEqual "phase should be OwnershipAnalysisPhase" OwnershipAnalysisPhase (cePhase compilerError)
  assertBool "message should mention borrowed value" $ 
    T.isInfixOf "borrowed" (Core.message typeError)

testSeverityOrdering :: IO ()
testSeverityOrdering = do
  let severities = [Core.Debug, Core.Info, Core.Warning, Core.Error]
      severityOrder severity = case severity of
        Core.Debug -> 0
        Core.Info -> 1
        Core.Warning -> 2
        Core.Error -> 3
  assertBool "Debug should be less severe than Info" $
    severityOrder Core.Debug < severityOrder Core.Info
  assertBool "Info should be less severe than Warning" $
    severityOrder Core.Info < severityOrder Core.Warning
  assertBool "Warning should be less severe than Error" $
    severityOrder Core.Warning < severityOrder Core.Error

testWarningVsError :: IO ()
testWarningVsError = do
  let warning = Core.TypeError
        { Core.errorId = "WARN_001"
        , Core.severity = Core.Warning
        , Core.category = Core.UnusedVariable
        , Core.message = T.pack "Variable 'x' is never used"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 10)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.NoRecovery
        , Core.suggestions = [T.pack "Remove variable", T.pack "Prefix with '_'"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12348
        }
      error = Core.TypeError
        { Core.errorId = "ERR_001"
        , Core.severity = Core.Error
        , Core.category = Core.UndefinedVariable
        , Core.message = T.pack "Variable 'y' is not defined"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 2 5 15) (SourcePos 2 6 16)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Define variable 'y' before use"
        , Core.suggestions = [T.pack "y := 0", T.pack "var y int"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12349
        }
  
  assertEqual "warning should have Warning severity" Core.Warning (Core.severity warning)
  assertEqual "error should have Error severity" Core.Error (Core.severity error)
  assertBool "warning should allow no recovery" $ 
    Core.recovery warning == Core.NoRecovery
  assertBool "error should have recovery strategy" $ 
    Core.recovery error /= Core.NoRecovery

testMultiLineErrorLocation :: IO ()
testMultiLineErrorLocation = do
  let start = SourcePos 2 1 10
      end = SourcePos 4 5 50
      span = SourceSpan start end
      location = Core.SourceLocation span
      typeError = Core.TypeError
        { Core.errorId = "MULTILINE_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Unclosed block"
        , Core.location = location
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Add closing brace"
        , Core.suggestions = [T.pack "Add } at end of block"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12350
        }
  
  assertEqual "error should span multiple lines" location (Core.location typeError)
  assertBool "start line should be 2" $ sourcePosLine start == 2
  assertBool "end line should be 4" $ sourcePosLine end == 4

testErrorLocationInContext :: IO ()
testErrorLocationInContext = do
  let sourceCode = "func main() {\n    x := 5\n    y := x +\n}"
      pos = SourcePos 3 12 35
      span = SourceSpan pos pos
      location = Core.SourceLocation span
      typeError = Core.TypeError
        { Core.errorId = "CONTEXT_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Unexpected end of expression"
        , Core.location = location
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Complete the expression"
        , Core.suggestions = [T.pack "Add value after +"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12351
        }
      compilerError = CompilerError
        { ceError = typeError
        , ceSourceContext = Just sourceCode
        , ceStackTrace = ["parseExpression", "parseStatement"]
        , cePhase = ParsingPhase
        }
  
  assertEqual "source context should be provided" (Just sourceCode) (ceSourceContext compilerError)
  assertBool "error should be at line 3" $ sourcePosLine pos == 3
  assertBool "error should be at column 12" $ sourcePosColumn pos == 12

testSyntaxErrorRecovery :: IO ()
testSyntaxErrorRecovery = do
  let recovery = Core.SuggestFix "Add missing semicolon"
      typeError = Core.TypeError
        { Core.errorId = "RECOVERY_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Missing semicolon"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 10 10) (SourcePos 1 10 10)
        , Core.context = Core.emptyContext
        , Core.recovery = recovery
        , Core.suggestions = [T.pack "Add ; at end of statement"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12352
        }
  
  assertEqual "recovery should suggest fix" recovery (Core.recovery typeError)
  assertBool "should have suggestions" $ not (null (Core.suggestions typeError))

testTypeErrorRecovery :: IO ()
testTypeErrorRecovery = do
  let recovery = Core.RetryWithAlternative "Try using interface{}"
      typeError = Core.TypeError
        { Core.errorId = "RECOVERY_002"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Incompatible types in assignment"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 2 5 15) (SourcePos 2 10 20)
        , Core.context = Core.emptyContext
        , Core.recovery = recovery
        , Core.suggestions = [T.pack "Use interface{}", T.pack "Convert types"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12353
        }
  
  assertEqual "recovery should retry with alternative" recovery (Core.recovery typeError)
  assertBool "suggestions should mention interface" $ 
    any (T.isInfixOf "interface") (Core.suggestions typeError)

testCascadingTypeErrors :: IO ()
testCascadingTypeErrors = do
  let baseError = Core.TypeError
        { Core.errorId = "BASE_001"
        , Core.severity = Core.Error
        , Core.category = Core.UndefinedVariable
        , Core.message = T.pack "Variable 'x' not defined"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 5 5) (SourcePos 1 6 6)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Define variable 'x'"
        , Core.suggestions = [T.pack "x := 0"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12354
        }
      cascadedError = Core.TypeError
        { Core.errorId = "CASCADING_001"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Cannot infer type of undefined variable"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 10 10) (SourcePos 1 11 11)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.NoRecovery
        , Core.suggestions = []
        , Core.relatedErrors = []
        , Core.errorChain = [baseError]
        , Core.timestamp = Just 12355
        }
  
  assertBool "cascading error should reference base error" $
    baseError `elem` Core.errorChain cascadedError
  assertBool "cascading error should have later timestamp" $
    Core.timestamp cascadedError > Core.timestamp baseError

testErrorPropagation :: IO ()
testErrorPropagation = do
  let parsingError = Core.TypeError
        { Core.errorId = "PARSE_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Invalid syntax"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 5)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SkipNode
        , Core.suggestions = []
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12356
        }
      typeCheckError = Core.TypeError
        { Core.errorId = "TYPECHECK_001"
        , Core.severity = Core.Error
        , Core.category = Core.InternalError
        , Core.message = T.pack "Cannot type check invalid AST"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 5)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.NoRecovery
        , Core.suggestions = [T.pack "Fix syntax errors first"]
        , Core.relatedErrors = [parsingError]
        , Core.errorChain = []
        , Core.timestamp = Just 12357
        }
  
  assertEqual "parsing error should be in ParsingPhase" ParsingPhase $ 
    cePhase $ CompilerError parsingError Nothing [] ParsingPhase
  assertEqual "typecheck error should be in TypeCheckingPhase" TypeCheckingPhase $
    cePhase $ CompilerError typeCheckError Nothing [] TypeCheckingPhase
  assertBool "typecheck error should reference parsing error" $
    parsingError `elem` Core.relatedErrors typeCheckError

-- | Helper functions
sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _ _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col _) = col

spanContains :: SourceSpan -> SourcePos -> Bool
spanContains (SourceSpan start end) pos =
  let posLine = sourcePosLine pos
      startLine = sourcePosLine start
      endLine = sourcePosLine end
      posCol = sourcePosColumn pos
      startCol = sourcePosColumn start
      endCol = sourcePosColumn end
  in if posLine == startLine && posLine == endLine
     then posCol >= startCol && posCol <= endCol
     else if posLine == startLine
          then posCol >= startCol
          else if posLine == endLine
               then posCol <= endCol
               else posLine > startLine && posLine < endLine

isValidSpan :: SourceSpan -> Bool
isValidSpan (SourceSpan start end) =
  sourcePosLine start <= sourcePosLine end &&
  (if sourcePosLine start == sourcePosLine end
   then sourcePosColumn start <= sourcePosColumn end
   else True)

-- | Test collection
tests :: TestTree
tests = testGroup "Compiler Error Handling Tests"
  [ testCompilerErrorHandling
  ]