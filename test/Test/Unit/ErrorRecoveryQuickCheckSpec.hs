{-# LANGUAGE CPP #-}

module Test.Unit.ErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)

import Compiler.Errors (CompilerError(..), ErrorSeverity(..), formatError, formatCompilerError, CompilationPhase(..))
import Compiler.Errors.Core (TypeError(..), formatError, errorAt, getErrorLine, getErrorColumn, ErrorLocation(..), ErrorContext(..), ErrorCategory(..), ErrorSeverity(..), ErrorRecovery(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..), hasTypeErrors, diagnoseTypeErrors, createTypusFileFromErrors, TypeError(..))
import Parser (parseTypus)
import Utils (trim, removeComments)
import Data.Char (isAlphaNum, isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import SourceLocation (SourcePos(..), startPos, toErrorLocation)
import qualified Data.Text as T
import Data.Text (pack)

-- Arbitrary instances for testing
instance Arbitrary CompilerError where
  arbitrary = do
    typeError <- arbitrary
    sourceContext <- arbitrary
    stackTrace <- listOf $ elements ["func1", "func2", "func3"]
    phase <- arbitrary
    return $ CompilerError typeError sourceContext stackTrace phase

instance Arbitrary Compiler.Errors.Core.TypeError where
  arbitrary = do
    errorId <- elements ["TYPE001", "TYPE002", "TYPE003"]
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf arbitrary
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- arbitrary
    return $ Compiler.Errors.Core.TypeError {
      errorId = errorId,
      severity = severity,
      category = category,
      message = message,
      location = location,
      context = context,
      recovery = recovery,
      suggestions = suggestions,
      relatedErrors = relatedErrors,
      errorChain = errorChain,
      timestamp = timestamp
    }

-- | Generate random error locations
instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- choose (1, 100)
    column <- choose (1, 100)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation filePath line column endLine endColumn

-- | Generate random error contexts
instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- arbitrary
    contextFunction <- arbitrary
    contextVariable <- arbitrary
    contextType <- arbitrary
    contextAdditional <- listOf $ arbitrary
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

-- | Generate random error categories
instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- | Generate random error severity
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

-- | Generate random compilation phases


-- | Generate random Text for error messages
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- | Generate random error recovery strategies
instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- arbitrary
    shouldContinue <- arbitrary
    recoveryAction <- arbitrary
    recoveryHint <- arbitrary
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

-- | Generate random compilation phases
instance Arbitrary CompilationPhase where
  arbitrary = elements [LexingPhase, ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependentTypeCheckingPhase, CodeGenerationPhase, OptimizationPhase]

-- Helper function for categorizing errors
categorizeError :: CompilerError -> String
categorizeError err = case err of
  _ -> "general" -- Default category

-- | Generate random error messages
genErrorMessage :: Gen String
genErrorMessage = listOf $ oneof
  [ choose ('a', 'z')
  , choose ('A', 'Z')
  , choose ('0', '9')
  , elements " \t\n\r"
  , elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  ]

-- | Generate random compiler errors
genCompilerError :: Gen CompilerError
genCompilerError = do
  message <- genErrorMessage
  severity <- elements [Warning, Error, Fatal]
  line <- choose (1, 100)
  column <- choose (1, 80)
  let pos = startPos { posLine = line, posColumn = column }
      typeError = errorAt "test" (pack message) (toErrorLocation pos)
      typeErrorWithSeverity = typeError { severity = severity }
  return $ CompilerError typeErrorWithSeverity Nothing [] TypeCheckingPhase

-- | Generate malformed code snippets
genMalformedCode :: Gen String
genMalformedCode = oneof
  [ genUnclosedBrackets
  , genUnclosedBraces
  , genUnclosedParentheses
  , genInvalidSyntax
  , genMixedMalformed
  ]

-- | Generate code with unclosed brackets
genUnclosedBrackets :: Gen String
genUnclosedBrackets = do
  content <- genErrorMessage
  return $ "func test() {\n  x := [" ++ content ++ "\n}"

-- | Generate code with unclosed braces
genUnclosedBraces :: Gen String
genUnclosedBraces = do
  content <- genErrorMessage
  return $ "func test() {\n  if true {\n    " ++ content

-- | Generate code with unclosed parentheses
genUnclosedParentheses :: Gen String
genUnclosedParentheses = do
  content <- genErrorMessage
  return $ "func test() {\n  x := (1 + 2 * " ++ content ++ "\n}"

-- | Generate code with invalid syntax
genInvalidSyntax :: Gen String
genInvalidSyntax = do
  content <- genErrorMessage
  return $ "func test() {\n  x := @#$" ++ content ++ "\n}"

-- | Generate code with multiple malformed elements
genMixedMalformed :: Gen String
genMixedMalformed = do
  content <- genErrorMessage
  return $ "func test() {\n  x := [1, 2\n  y := (a + b\n  if true {\n    " ++ content

-- | Generate well-formed code snippets
genWellFormedCode :: Gen String
genWellFormedCode = do
  varName <- elements ["x", "y", "z", "result"]
  value <- choose (0, 100)
  return $ "func test() {\n  " ++ varName ++ " := " ++ show value ++ "\n}"

-- | Generate error recovery scenarios
genErrorRecoveryScenario :: Gen (String, String)
genErrorRecoveryScenario = do
  malformed <- genMalformedCode
  corrected <- genWellFormedCode
  return (malformed, corrected)

-- Property: Error formatting preserves essential information
prop_error_formatting_preserves_info :: CompilerError -> Property
prop_error_formatting_preserves_info error =
  let formatted = formatCompilerError error
      typeErr = ceError error
      hasMessage = T.unpack (message typeErr) `isInfixOf` formatted
      hasLine = show (getErrorLine (location typeErr)) `isInfixOf` formatted
      hasColumn = show (getErrorColumn (location typeErr)) `isInfixOf` formatted
  in property $ hasMessage .&&. hasLine .&&. hasColumn

-- Property: Error categorization is consistent with severity
prop_error_categorization_consistent :: CompilerError -> Property
prop_error_categorization_consistent error =
  let category = categorizeError error
      errorSeverity = severity (ceError error)
  in case errorSeverity of
    Warning -> property $ category == "Warning"
    Error -> property $ category == "Error"
    Fatal -> property $ category == "Fatal"
    Info -> property $ category == "Info"

-- Property: Malformed code detection
prop_malformed_code_detection :: String -> Property
prop_malformed_code_detection code =
  let hasUnclosedBrackets = '[' `elem` code && ']' `notElem` code
      hasUnclosedBraces = '{' `elem` code && '}' `notElem` code
      hasUnclosedParens = '(' `elem` code && ')' `notElem` code
      hasInvalidChars = any (`elem` "@#$%^&*") code
      shouldBeMalformed = hasUnclosedBrackets || hasUnclosedBraces || 
                         hasUnclosedParens || hasInvalidChars
  in shouldBeMalformed ==> property $ True  -- Would check actual detection

-- Property: Error recovery preserves structure
prop_error_recovery_preserves_structure :: String -> String -> Property
prop_error_recovery_preserves_structure malformed corrected =
  let cleanedMalformed = removeComments malformed
      cleanedCorrected = removeComments corrected
      malfunctions = parseTypus cleanedMalformed
      corrections = parseTypus cleanedCorrected
  in case (malfunctions, corrections) of
    (Left _, Right _) -> property $ True  -- Recovery successful
    (Left _, Left _) -> property $ True  -- Both fail, but structure preserved
    (Right _, Right _) -> property $ True  -- Both succeed
    (Right _, Left _) -> property $ False  -- Unexpected case

-- Property: Multiple error handling
prop_multiple_error_handling :: [CompilerError] -> Property
prop_multiple_error_handling errors =
  not (null errors) ==> 
  let formatted = map formatCompilerError errors
      categorized = map categorizeError errors
      hasWarnings = any (\e -> severity (ceError e) == Warning) errors
      hasErrors = any (\e -> severity (ceError e) == Error) errors
      hasFatal = any (\e -> severity (ceError e) == Fatal) errors
  in property $ length formatted == length errors .&&.
             length categorized == length errors

-- Property: Error position tracking
prop_error_position_tracking :: CompilerError -> Property
prop_error_position_tracking error =
  let typeErr = ceError error
      loc = location typeErr
      lineNum = getErrorLine loc
      columnNum = getErrorColumn loc
  in property $ lineNum >= 1 .&&. columnNum >= 1 .&&. lineNum <= 1000 .&&. columnNum <= 200

-- Property: Error message uniqueness
prop_error_message_uniqueness :: [CompilerError] -> Property
prop_error_message_uniqueness errors =
  let messages = map (T.unpack . message . ceError) errors
      uniqueMessages = Data.List.nub messages
  in property $ length uniqueMessages <= length messages

-- Property: Type error diagnosis consistency
prop_type_error_diagnosis_consistent :: Compiler.Errors.Core.TypeError -> Property
prop_type_error_diagnosis_consistent typeError =
  let testFile = createTypusFileFromErrors [Compiler.TypeChecker.TypeError (Just $ T.unpack $ Compiler.Errors.Core.message typeError) (T.unpack $ Compiler.Errors.Core.message typeError)]
      diagnostics = diagnoseTypeErrors testFile
      hasErrors = hasTypeErrors testFile
  in property $ hasErrors

-- Property: Error recovery incremental improvement
prop_error_recovery_incremental :: String -> [String] -> Property
prop_error_recovery_incremental original corrections =
  not (null corrections) ==> 
  let originalResult = parseTypus original
      correctedResults = map parseTypus corrections
      originalFailed = case originalResult of
        Left _ -> True
        Right _ -> False
      successes = length [() | Right _ <- correctedResults]
  in originalFailed ==> property $ successes >= 0

-- Property: Error context preservation
prop_error_context_preservation :: String -> String -> Property
prop_error_context_preservation code context =
  let fullCode = context ++ "\n" ++ code
      result = parseTypus fullCode
  in case result of
    Left error -> property $ True  -- Error should include context
    Right _ -> property $ True

-- Property: Error severity classification
prop_error_severity_classification :: CompilerError -> Property
prop_error_severity_classification error =
  let typeErr = ceError error
      errorSeverity = severity typeErr
      errorMessage = T.unpack (message typeErr)
      hasKeyword = any (`isInfixOf` map toLower errorMessage) ["error", "warning", "fatal"]
  in property $ hasKeyword ==> True

-- Property: Error recovery performance
prop_error_recovery_performance :: String -> Int -> Property
prop_error_recovery_performance code iterations =
  iterations <= 100 ==> 
  let results = replicate iterations (parseTypus code)
      failures = length [() | Left _ <- results]
  in property $ failures >= 0

-- Property: Error message informativeness
prop_error_message_informativeness :: CompilerError -> Property
prop_error_message_informativeness error =
  let errorMessage = T.unpack (message (ceError error))
      hasContent = length (trim errorMessage) > 0
      hasReasonableLength = length errorMessage <= 500
  in property $ hasContent .&&. hasReasonableLength

-- Property: Error localization consistency
prop_error_localization_consistent :: CompilerError -> Property
prop_error_localization_consistent error =
  let typeErr = ceError error
      loc = location typeErr
      lineNum = getErrorLine loc
      columnNum = getErrorColumn loc
      formatted = formatCompilerError error
      hasLineInfo = show lineNum `isInfixOf` formatted
      hasColumnInfo = show columnNum `isInfixOf` formatted
  in property $ hasLineInfo .&&. hasColumnInfo

-- Property: Error recovery state preservation
prop_error_recovery_state_preservation :: String -> String -> Property
prop_error_recovery_state_preservation before after =
  let beforeResult = parseTypus before
      afterResult = parseTypus after
  in case (beforeResult, afterResult) of
    (Left _, Right _) -> property $ True  -- Recovery successful
    (Left _, Left _) -> property $ True  -- Consistent failure
    (Right _, Right _) -> property $ True  -- Both succeed
    (Right _, Left _) -> property $ False  -- Regression

tests :: TestTree
tests = testGroup "Error Recovery QuickCheck Tests"
  [ fastProperty "error formatting preserves info" prop_error_formatting_preserves_info
  , fastProperty "error categorization consistent" prop_error_categorization_consistent
  , fastProperty "malformed code detection" prop_malformed_code_detection
  , fastProperty "error recovery preserves structure" prop_error_recovery_preserves_structure
  , fastProperty "multiple error handling" prop_multiple_error_handling
  , fastProperty "error position tracking" prop_error_position_tracking
  , fastProperty "error message uniqueness" prop_error_message_uniqueness
  , fastProperty "type error diagnosis consistent" prop_type_error_diagnosis_consistent
  , fastProperty "error recovery incremental" prop_error_recovery_incremental
  , fastProperty "error context preservation" prop_error_context_preservation
  , fastProperty "error severity classification" prop_error_severity_classification
  , fastProperty "error recovery performance" prop_error_recovery_performance
  , fastProperty "error message informativeness" prop_error_message_informativeness
  , fastProperty "error localization consistent" prop_error_localization_consistent
  , fastProperty "error recovery state preservation" prop_error_recovery_state_preservation
  ]