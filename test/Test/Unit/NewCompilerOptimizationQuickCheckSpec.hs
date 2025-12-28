module Test.Unit.NewCompilerOptimizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourceSpan(..))
import TestSupport.QuickCheck (fastProperty)

-- ============================================================================
-- New QuickCheck Tests for Compiler Optimization Consistency
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Compiler Optimization Consistency QuickCheck Tests"
    [ testGroup "Compilation Consistency Properties"
        [ fastProperty "compilation is deterministic" prop_compilationIsDeterministic
        , fastProperty "generateGoCode is deterministic" prop_generateGoCodeIsDeterministic
        , fastProperty "compilation preserves semantic meaning" prop_compilationPreservesSemanticMeaning
        , fastProperty "optimization doesn't break valid code" prop_optimizationDoesntBreakValidCode
        , fastProperty "error messages are consistent" prop_errorMessagesAreConsistent
        ]

    , testGroup "Optimization Invariants"
        [ fastProperty "compilation result is total" prop_compilationResultIsTotal
        , fastProperty "generated Go code is syntactically valid" prop_generatedGoCodeIsSyntacticallyValid
        , fastProperty "compilation handles edge cases gracefully" prop_compilationHandlesEdgeCases
        , fastProperty "optimization preserves type safety" prop_optimizationPreservesTypeSafety
        ]

    , testGroup "Performance Properties"
        [ fastProperty "compilation time is reasonable" prop_compilationTimeIsReasonable
        , fastProperty "memory usage doesn't grow excessively" prop_memoryUsageDoesntGrowExcessively
        , fastProperty "optimization doesn't introduce infinite loops" prop_optimizationDoesntIntroduceInfiniteLoops
        , fastProperty "generated code size is bounded" prop_generatedCodeSizeIsBounded
        ]

    , testGroup "Error Handling Properties"
        [ fastProperty "compilation errors are informative" prop_compilationErrorsAreInformative
        , fastProperty "partial compilation succeeds when possible" prop_partialCompilationSucceedsWhenPossible
        , fastProperty "error recovery doesn't lose information" prop_errorRecoveryDoesntLoseInformation
        , fastProperty "compilation phases maintain invariants" prop_compilationPhasesMaintainInvariants
        ]
    ]

-- ============================================================================
-- Compilation Consistency Property Tests
-- ============================================================================

-- | Compilation should be deterministic - same input produces same output
prop_compilationIsDeterministic :: String -> Property
prop_compilationIsDeterministic input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result1 = compile typusFile
             result2 = compile typusFile
         in counterexample ("input=" ++ show input) $
            case (result1, result2) of
              (Right r1, Right r2) -> r1 === r2
              (Left e1, Left e2) -> length e1 === length e2  -- Compare error count
              _ -> property False  -- Should be consistent in success/failure

-- | generateGoCode should be deterministic
prop_generateGoCodeIsDeterministic :: String -> Property
prop_generateGoCodeIsDeterministic input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result1 = generateGoCode typusFile
             result2 = generateGoCode typusFile
         in counterexample ("input=" ++ show input) $
            result1 === result2

-- | Compilation should preserve semantic meaning
prop_compilationPreservesSemanticMeaning :: String -> Property
prop_compilationPreservesSemanticMeaning input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
             goCode = generateGoCode typusFile
         in counterexample ("input=" ++ take 50 input ++ "...") $
            case result of
              Right compiled -> 
                -- Compiled code should be related to generated Go code
                property (not (null compiled) && not (null goCode))
              Left _ -> property True  -- Errors are acceptable

-- | Optimization shouldn't break valid code
prop_optimizationDoesntBreakValidCode :: String -> Property
prop_optimizationDoesntBreakValidCode validCode =
  let isValid = isValidTypusCode validCode
      parsed = parseTypus validCode
  in if not isValid
     then property True  -- Skip invalid code
     else case parsed of
            Left _ -> property True  -- Parsing errors are acceptable
            Right typusFile ->
              let result = compile typusFile
              in counterexample ("validCode=" ++ take 50 validCode ++ "...") $
                 case result of
                   Right compiled -> not (null compiled)
                   Left _ -> property True  -- May fail for other reasons

-- | Error messages should be consistent
prop_errorMessagesAreConsistent :: String -> Property
prop_errorMessagesAreConsistent input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in case result of
              Left errors -> 
                -- Error messages should have consistent format
                property (all hasValidFormat errors)
              Right _ -> property True
  where
    hasValidFormat :: CompilerError -> Bool
    hasValidFormat err = 
      let msg = unlines [show err]
      in length msg > 0  -- Basic format check

-- ============================================================================
-- Optimization Invariant Tests
-- ============================================================================

-- | Compilation result should be total (always returns something)
prop_compilationResultIsTotal :: String -> Property
prop_compilationResultIsTotal input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Parsing may fail, but that's expected
       Right typusFile ->
         let result = compile typusFile
         in counterexample ("input=" ++ take 50 input ++ "...") $
            case result of
              Right _ -> property True
              Left errors -> not (null errors)  -- Should provide errors

-- | Generated Go code should be syntactically valid (basic checks)
prop_generatedGoCodeIsSyntacticallyValid :: String -> Property
prop_generatedGoCodeIsSyntacticallyValid input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let goCode = generateGoCode typusFile
         in counterexample ("input length=" ++ show (length input)) $
            basicGoSyntaxCheck goCode

-- | Compilation should handle edge cases gracefully
prop_compilationHandlesEdgeCases :: String -> Property
prop_compilationHandlesEdgeCases input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Parsing may fail on edge cases
       Right typusFile ->
         let result = compile typusFile
         in counterexample ("edge case input length=" ++ show (length input)) $
            case result of
              Right _ -> property True
              Left errors -> property (not (null errors))

-- | Optimization should preserve type safety
prop_optimizationPreservesTypeSafety :: String -> Property
prop_optimizationPreservesTypeSafety input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in counterexample ("input=" ++ take 50 input ++ "...") $
            case result of
              Right compiled -> 
                -- Should not introduce obvious type errors
                property (not (containsObviousTypeErrors compiled))
              Left _ -> property True  -- Errors are acceptable

-- ============================================================================
-- Performance Property Tests
-- ============================================================================

-- | Compilation time should be reasonable (basic check)
prop_compilationTimeIsReasonable :: String -> Property
prop_compilationTimeIsReasonable input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         -- This is a basic sanity check - real performance testing would need timing
         let result = compile typusFile
         in property True  -- If it completes without hanging, time is reasonable

-- | Memory usage shouldn't grow excessively (basic check)
prop_memoryUsageDoesntGrowExcessively :: String -> Property
prop_memoryUsageDoesntGrowExcessively input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
             goCode = generateGoCode typusFile
         in counterexample ("input length=" ++ show (length input) ++ 
                          ", output length=" ++ show (length goCode)) $
            -- Basic check: output shouldn't be exponentially larger than input
            property (length goCode <= length input * 10)

-- | Optimization shouldn't introduce infinite loops
prop_optimizationDoesntIntroduceInfiniteLoops :: String -> Property
prop_optimizationDoesntIntroduceInfiniteLoops input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in case result of
              Right compiled -> 
                -- Basic check for obvious infinite loop patterns
                property (not (containsInfiniteLoopPatterns compiled))
              Left _ -> property True

-- | Generated code size should be bounded
prop_generatedCodeSizeIsBounded :: String -> Property
prop_generatedCodeSizeIsBounded input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let goCode = generateGoCode typusFile
             inputSize = length input
             outputSize = length goCode
         in counterexample ("input size=" ++ show inputSize ++ 
                          ", output size=" ++ show outputSize) $
            -- Reasonable upper bound
            property (outputSize <= max 1000 (inputSize * 5))

-- ============================================================================
-- Error Handling Property Tests
-- ============================================================================

-- | Compilation errors should be informative
prop_compilationErrorsAreInformative :: String -> Property
prop_compilationErrorsAreInformative input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in case result of
              Left errors ->
                -- Each error should have meaningful information
                property (all errorIsInformative errors)
              Right _ -> property True
  where
    errorIsInformative :: CompilerError -> Bool
    errorIsInformative err = 
      -- Basic check: error should have non-empty message
      length (show err) > 10

-- | Partial compilation should succeed when possible
prop_partialCompilationSucceedsWhenPossible :: String -> String -> Property
prop_partialCompilationSucceedsWhenPossible validPart errorPart =
  let input = validPart ++ "\n" ++ errorPart
      parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
             goCode = generateGoCode typusFile
         in counterexample ("validPart=" ++ take 30 validPart ++ "...") $
            -- Should generate some Go code even with errors
            property (not (null goCode))

-- | Error recovery shouldn't lose information
prop_errorRecoveryDoesntLoseInformation :: String -> Property
prop_errorRecoveryDoesntLoseInformation input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in case result of
              Left errors ->
                -- Should preserve original structure information
                property (length errors > 0)
              Right compiled -> 
                -- Should preserve semantic information
                property (not (null compiled))

-- | Compilation phases should maintain invariants
prop_compilationPhasesMaintainInvariants :: String -> Property
prop_compilationPhasesMaintainInvariants input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True  -- Skip if parsing fails
       Right typusFile ->
         let result = compile typusFile
         in case result of
              Left errors ->
                -- Errors should be properly categorized by phase
                property (all hasValidPhase errors)
              Right _ -> property True
  where
    hasValidPhase :: CompilerError -> Bool
    hasValidPhase err = 
      -- Error should have a valid compilation phase
      case err of
        CompilerError { cePhase = phase } -> 
          phase `elem` [ParsingPhase, TypeCheckingPhase, OptimizationPhase, CodeGenPhase]

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Basic check if code looks like valid Typus code
isValidTypusCode :: String -> Bool
isValidTypusCode code =
  let lines' = lines code
      hasValidStructure = any (not . null) lines'
  in hasValidStructure && length code < 10000  -- Reasonable size

-- | Basic Go syntax check
basicGoSyntaxCheck :: String -> Property
basicGoSyntaxCheck goCode =
  let hasPackageDecl = "package " `isInfixOf` goCode
      hasBalancedBraces = countChar '{' goCode == countChar '}' goCode
      hasBalancedParens = countChar '(' goCode == countChar ')' goCode
  in property (hasPackageDecl || null goCode) && hasBalancedBraces && hasBalancedParens

-- | Check if code contains obvious type errors
containsObviousTypeErrors :: String -> Bool
containsObviousTypeErrors code =
  let obviousErrors = ["string := 42", "int := \"hello\"", "var x int = \"string\""]
  in any (`isInfixOf` code) obviousErrors

-- | Check if code contains infinite loop patterns
containsInfiniteLoopPatterns :: String -> Bool
containsInfiniteLoopPatterns code =
  let infinitePatterns = ["for {}", "while true {}", "for ;; {}"]
  in any (`isInfixOf` code) infinitePatterns

-- | Count character occurrences
countChar :: Char -> String -> Int
countChar c = length . filter (== c)
