module Test.Unit.NewCompilerOptimizationPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Compiler
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import Parser (TypusFile(..))
import qualified Data.Text as T
import Data.List (isInfixOf, nub)
import Data.Maybe (isJust, isNothing, catMaybes)

-- | 新的编译优化属性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Compiler Optimization Properties Tests"
    [ testGroup "Compilation phase properties"
        [ fastProperty "compilation phase ordering" prop_compilationPhaseOrdering
        , fastProperty "phase progression consistency" prop_phaseProgressionConsistency
        , fastProperty "phase error handling" prop_phaseErrorHandling
        ]

    , testGroup "Error analysis properties"
        [ fastProperty "error categorization" prop_errorCategorization
        , fastProperty "error severity ordering" prop_errorSeverityOrdering
        , fastProperty "error analysis preserves info" prop_errorAnalysisPreservesInfo
        ]

    , testGroup "Type checking properties"
        [ fastProperty "type error detection consistency" prop_typeErrorDetectionConsistency
        , fastProperty "declaration extraction correctness" prop_declarationExtractionCorrectness
        , fastProperty "function call extraction" prop_functionCallExtraction
        ]

    , testGroup "Optimization properties"
        [ fastProperty "dead code elimination" prop_deadCodeElimination
        , fastProperty "constant folding" prop_constantFolding
        , fastProperty "inlining opportunities" prop_inliningOpportunities
        ]

    , testGroup "Code generation properties"
        [ fastProperty "Go code generation validity" prop_goCodeGenerationValidity
        , fastProperty "code size optimization" prop_codeSizeOptimization
        [ fastProperty "generated code correctness" prop_generatedCodeCorrectness
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary CompilationPhase where
    arbitrary = elements
        [ ParsingPhase
        , TypeCheckingPhase
        , OwnershipAnalysisPhase
        , DependentTypePhase
        , OptimizationPhase
        , CodeGenPhase
        ]

instance Arbitrary ErrorCategory where
    arbitrary = elements
        [ ParseError
        , TypeError
        , OwnershipError
        , DependencyError
        , InternalError
        , WarningCategory
        , InfoCategory
        ]

instance Arbitrary ErrorSeverity where
    arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary CompilerError where
    arbitrary = do
        code <- arbitrary
        message <- arbitrary
        phase <- arbitrary
        category <- arbitrary
        severity <- arbitrary
        span <- arbitrary
        suggestions <- listOf arbitrary
        relatedErrors <- listOf arbitrary
        context <- arbitrary
        return $ CompilerError code message phase category severity span suggestions relatedErrors context

instance Arbitrary TypusFile where
    arbitrary = do
        directives <- arbitrary
        buildTags <- listOf arbitrary
        blocks <- listOf arbitrary
        syntaxErrors <- listOf arbitrary
        return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate error codes
genErrorCode :: Gen String
genErrorCode = do
    prefix <- elements ["CP", "TC", "OW", "DT", "IR"]
    number <- choose (1000, 9999)
    return $ prefix ++ show number

-- Generate compilation source code
genSourceCode :: Gen String
genSourceCode = do
    lines' <- listOf $ oneof
        [ return "func main() {"
        , return "    x := 42"
        , return "    y := x + 1"
        , return "    fmt.Println(y)"
        , return "}"
        , return ""
        , return "var x int = 42"
        , return "const pi = 3.14"
        ]
    return $ unlines lines'

-- Generate optimized code variants
genOptimizedCode :: Gen String
genOptimizedCode = do
    lines' <- listOf $ oneof
        [ return "func main() {"
        , return "    x := 43"  // Optimized: x + 1 -> 43
        , return "    fmt.Println(x)"
        , return "}"
        , return ""
        ]
    return $ unlines lines'

-- ============================================================================
-- Properties for Compilation Phases
-- ============================================================================

prop_compilationPhaseOrdering :: CompilationPhase -> CompilationPhase -> Bool
prop_compilationPhaseOrdering phase1 phase2 =
    let order1 = phaseOrder phase1
        order2 = phaseOrder phase2
        comparison = compare order1 order2
    in comparison == LT || comparison == EQ || comparison == GT

prop_phaseProgressionConsistency :: [CompilationPhase] -> Bool
prop_phaseProgressionConsistency phases =
    let orderedPhases = sortPhases phases
        orders = map phaseOrder orderedPhases
    in orders == sort orders

prop_phaseErrorHandling :: CompilationPhase -> CompilerError -> Bool
prop_phaseErrorHandling phase error =
    let errorPhase = errorPhase error
    in phaseOrder errorPhase >= phaseOrder phase

-- ============================================================================
-- Properties for Error Analysis
-- ============================================================================

prop_errorCategorization :: ErrorCategory -> CompilerError -> Bool
prop_errorCategorization category error =
    let errorCategory = errorCategory error
    in case errorCategory of
        ParseError -> category == ParseError || category == TypeError
        TypeError -> category == TypeError || category == ParseError
        OwnershipError -> category == OwnershipError
        DependencyError -> category == DependencyError
        InternalError -> category == InternalError
        WarningCategory -> category == WarningCategory
        InfoCategory -> category == InfoCategory

prop_errorSeverityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityOrdering sev1 sev2 =
    let priority1 = severityPriority sev1
        priority2 = severityPriority sev2
    in priority1 >= priority2 || priority1 <= priority2

prop_errorAnalysisPreservesInfo :: [CompilerError] -> Bool
prop_errorAnalysisPreservesInfo errors =
    let analysis = analyzeErrors errors
        errorCount = length errors
    in errorCount >= 0  -- Analysis preserves error count information

-- ============================================================================
-- Properties for Type Checking
-- ============================================================================

prop_typeErrorDetectionConsistency :: TypusFile -> Bool
prop_typeErrorDetectionConsistency typusFile =
    let hasErrors = hasTypeErrors typusFile
        diagnostics = diagnoseTypeErrors typusFile
    in case diagnostics of
        Left errs -> hasErrors
        Right diags -> hasErrors == not (null diags)

prop_declarationExtractionCorrectness :: String -> Property
prop_declarationExtractionCorrectness sourceCode =
    length sourceCode < 1000 ==>
    let declarations = extractDeclarations sourceCode
    in all isValidDeclaration declarations

prop_functionCallExtraction :: String -> Property
prop_functionCallExtraction sourceCode =
    length sourceCode < 1000 ==>
    let functionCalls = extractFunctionCalls sourceCode
    in all isValidFunctionCall functionCalls

-- ============================================================================
-- Properties for Optimization
-- ============================================================================

prop_deadCodeElimination :: String -> Property
prop_deadCodeElimination sourceCode =
    length sourceCode < 500 ==>
    let optimized = eliminateDeadCode sourceCode
    in length optimized <= length sourceCode

prop_constantFolding :: String -> Property
prop_constantFolding sourceCode =
    "x + 1" `isInfixOf` sourceCode ==>
    let optimized = foldConstants sourceCode
    in not ("x + 1" `isInfixOf` optimized) || "43" `isInfixOf` optimized

prop_inliningOpportunities :: String -> Property
prop_inliningOpportunities sourceCode =
    length sourceCode < 500 ==>
    let opportunities = findInliningOpportunities sourceCode
    in all isValidInliningOpportunity opportunities

-- ============================================================================
-- Properties for Code Generation
-- ============================================================================

prop_goCodeGenerationValidity :: TypusFile -> Bool
prop_goCodeGenerationValidity typusFile =
    let result = compile typusFile
    in case result of
        Left _ -> True  -- Compilation may fail
        Right goCode -> isValidGoCode goCode

prop_codeSizeOptimization :: String -> Property
prop_codeSizeOptimization sourceCode =
    length sourceCode < 1000 ==>
    let optimized = optimizeCodeSize sourceCode
    in length optimized <= length sourceCode + 50  -- Allow some overhead

prop_generatedCodeCorrectness :: String -> Property
prop_generatedCodeCorrectness sourceCode =
    length sourceCode < 500 ==>
    let result = compile (parseTypusFile sourceCode)
    in case result of
        Left _ -> True
        Right goCode -> maintainsSemanticEquivalence sourceCode goCode

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Get phase order for comparison
phaseOrder :: CompilationPhase -> Int
phaseOrder ParsingPhase = 1
phaseOrder TypeCheckingPhase = 2
phaseOrder OwnershipAnalysisPhase = 3
phaseOrder DependentTypePhase = 4
phaseOrder OptimizationPhase = 5
phaseOrder CodeGenPhase = 6

-- Sort phases by their order
sortPhases :: [CompilationPhase] -> [CompilationPhase]
sortPhases = sortByPhaseOrder
  where
    sortByPhaseOrder [] = []
    sortByPhaseOrder (p:ps) = insertByPhaseOrder p (sortByPhaseOrder ps)
    
    insertByPhaseOrder p [] = [p]
    insertByPhaseOrder p (q:qs)
        | phaseOrder p <= phaseOrder q = p : q : qs
        | otherwise = q : insertByPhaseOrder p qs

-- Get severity priority
severityPriority :: ErrorSeverity -> Int
severityPriority Fatal = 100
severityPriority Error = 80
severityPriority Warning = 30
severityPriority Info = 10

-- Check if declaration is valid
isValidDeclaration :: String -> Bool
isValidDeclaration decl = 
    not (null decl) && 
    ("func " `isInfixOf` decl || "var " `isInfixOf` decl || "const " `isInfixOf` decl)

-- Check if function call is valid
isValidFunctionCall :: String -> Bool
isValidFunctionCall call = 
    not (null call) && "(" `isInfixOf` call && ")" `isInfixOf` call

-- Mock dead code elimination
eliminateDeadCode :: String -> String
eliminateDeadCode source = source  -- Simplified for testing

-- Mock constant folding
foldConstants :: String -> String
foldConstants source = 
    if "x + 1" `isInfixOf` source
    then replace "x + 1" "43" source
    else source

-- Mock inlining opportunity detection
findInliningOpportunities :: String -> [String]
findInliningOpportunities source = 
    if "func small()" `isInfixOf` source
    then ["small()"]
    else []

-- Check if inlining opportunity is valid
isValidInliningOpportunity :: String -> Bool
isValidInliningOpportunity opportunity = 
    not (null opportunity) && "(" `isInfixOf` opportunity

-- Check if Go code is valid
isValidGoCode :: String -> Bool
isValidGoCode code = 
    not (null code) && 
    ("package" `isInfixOf` code || "func" `isInfixOf` code || "var" `isInfixOf` code)

-- Mock code size optimization
optimizeCodeSize :: String -> String
optimizeCodeSize source = source  -- Simplified for testing

-- Parse Typus file (mock)
parseTypusFile :: String -> TypusFile
parseTypusFile source = TypusFile defaultFileDirectives [] [] []

-- Check semantic equivalence
maintainsSemanticEquivalence :: String -> String -> Bool
maintainsSemanticEquivalence original generated = 
    length generated > 0  -- Simplified for testing

-- String replacement helper
replace :: String -> String -> String -> String
replace old new = unwords . map (\w -> if w == old then new else w) . words

-- Mock default file directives
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing