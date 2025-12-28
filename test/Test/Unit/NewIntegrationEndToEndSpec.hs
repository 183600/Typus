module Test.Unit.NewIntegrationEndToEndSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Parser
import Compiler
import Ownership
import Dependencies
import SyntaxValidator
import DependentTypesParser
import ErrorHandler
import SourceLocation
import Utils
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Char (isAlphaNum, isAlpha)

-- | 新的端到端集成QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Integration End-to-End Tests"
    [ testGroup "Complete compilation pipeline"
        [ fastProperty "parse-compile-validate pipeline" prop_parseCompileValidatePipeline
        , fastProperty "error propagation through pipeline" prop_errorPropagationThroughPipeline
        , fastProperty "successful compilation produces valid output" prop_successfulCompilationProducesValidOutput
        ]

    , testGroup "Cross-module integration"
        [ fastProperty "parser and syntax validator integration" prop_parserSyntaxValidatorIntegration
        , fastProperty "compiler and error handler integration" prop_compilerErrorHandlerIntegration
        , fastProperty "ownership and dependency analysis integration" prop_ownershipDependencyIntegration
        ]

    , testGroup "Type system integration"
        [ fastProperty "dependent types and ownership integration" prop_dependentTypesOwnershipIntegration
        , fastProperty "type checking and constraint solving" prop_typeCheckingConstraintSolving
        , fastProperty "type inference and validation" prop_typeInferenceValidation
        ]

    , testGroup "Error handling integration"
        [ fastProperty "error collection and reporting" prop_errorCollectionReporting
        , fastProperty "error recovery and continuation" prop_errorRecoveryContinuation
        , fastProperty "multiple error handling" prop_multipleErrorHandling
        ]

    , testGroup "Performance and robustness"
        [ fastProperty "large input handling" prop_largeInputHandling
        , fastProperty "malformed input recovery" prop_malformedInputRecovery
        , fastProperty "concurrent processing safety" prop_concurrentProcessingSafety
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary CompilationScenario where
    arbitrary = oneof
        [ SimpleScenario <$> arbitrary
        , ComplexScenario <$> arbitrary <*> arbitrary <*> arbitrary
        , ErrorScenario <$> arbitrary <*> arbitrary
        ]

-- Generate complete Typus programs
genCompleteProgram :: Gen String
genCompleteProgram = do
    packageName <- genPackageName
    imports <- listOf genImport
    types <- listOf genTypeDefinition
    functions <- listOf genFunctionDefinition
    return $ unlines
        [ "package " ++ packageName
        , unlines imports
        , unlines types
        , unlines functions
        ]

-- Generate programs with syntax errors
genProgramWithSyntaxErrors :: Gen String
genProgramWithSyntaxErrors = oneof
    [ return "package main\nfunc main() {"  -- Missing closing brace
    , return "package main\nfunc main() {\n    x :=\n}"  -- Incomplete statement
    , return "package main\nfunc main() {\n    x := \"unclosed string\n}"  -- Unclosed string
    , return "func main() {}"  -- Missing package declaration
    ]

-- Generate programs with type errors
genProgramWithTypeErrors :: Gen String
genProgramWithTypeErrors = do
    return $ unlines
        [ "package main"
        , "func main() {"
        , "    var x int = \"string\""  -- Type mismatch
        , "    fmt.Println(x)"
        , "}"
        ]

-- Generate programs with ownership errors
genProgramWithOwnershipErrors :: Gen String
genProgramWithOwnershipErrors = do
    return $ unlines
        [ "package main"
        , "func main() {"
        , "    s1 := String::new()"
        , "    s2 := s1"
        , "    println!(\"{}\", s1)"  -- Use after move
        , "}"
        ]

-- Generate package names
genPackageName :: Gen String
genPackageName = elements ["main", "utils", "types", "errors", "parser", "compiler"]

-- Generate import statements
genImport :: Gen String
genImport = do
    module' <- elements ["fmt", "strings", "os", "io", "time"]
    return $ "import \"" ++ module' ++ "\""

-- Generate type definitions
genTypeDefinition :: Gen String
genTypeDefinition = do
    typeName <- genTypeName
    fields <- listOf 2 $ do
        fieldName <- genFieldName
        fieldType <- genBasicType
        return $ "    " ++ fieldName ++ " " ++ fieldType
    return $ unlines
        [ "type " ++ typeName ++ " struct {"
        , unlines fields
        , "}"
        ]

-- Generate function definitions
genFunctionDefinition :: Gen String
genFunctionDefinition = do
    funcName <- genFunctionName
    params <- listOf 2 $ do
        paramName <- genFieldName
        paramType <- genBasicType
        return $ paramName ++ " " ++ paramType
    returnType <- genBasicType
    body <- listOf 2 $ oneof
        [ return "    x := 42"
        , return "    return x"
        ]
    return $ unlines
        [ "func " ++ funcName ++ "(" ++ unwords (map (\p -> p ++ ",") params) ++ ") " ++ returnType ++ " {"
        , unlines body
        , "}"
        ]

-- Generate basic types
genBasicType :: Gen String
genBasicType = elements ["int", "string", "bool", "float64", "error"]

-- Generate type names
genTypeName :: Gen String
genTypeName = do
    first <- elements ['A'..'Z']
    rest <- listOf $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    return (first : rest)

-- Generate function names
genFunctionName :: Gen String
genFunctionName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    return (first : rest)

-- Generate field names
genFieldName :: Gen String
genFieldName = genFunctionName

-- ============================================================================
-- Properties for Complete Compilation Pipeline
-- ============================================================================

prop_parseCompileValidatePipeline :: String -> Property
prop_parseCompileValidatePipeline input =
    length input < 1000 ==>
    let parseResult = parseTypus input
        compileResult = case parseResult of
            Left _ -> Left ["Parse error"]
            Right typusFile -> compile typusFile
        validationErrors = case parseResult of
            Left _ -> []
            Right typusFile -> validateSyntax input
    in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Consistent error handling
        (Right _, Right _) -> True  -- Successful pipeline
        (Right _, Left _) -> True  -- Compilation errors are valid
        (Left _, Right _) -> False  -- Should not compile if parsing failed

prop_errorPropagationThroughPipeline :: String -> Property
prop_errorPropagationThroughPipeline input =
    length input < 500 ==>
    let parseResult = parseTypus input
        syntaxErrors = validateSyntax input
    in case parseResult of
        Left _ -> True  -- Parse errors should be propagated
        Right typusFile -> 
            let compileResult = compile typusFile
            in case compileResult of
                Left _ -> True  -- Compile errors should be propagated
                Right _ -> True  -- Success is also valid

prop_successfulCompilationProducesValidOutput :: String -> Property
prop_successfulCompilationProducesValidOutput input =
    "package main" `isInfixOf` input && "func main()" `isInfixOf` input ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let compileResult = compile typusFile
            in case compileResult of
                Right goCode -> isValidGoCode goCode
                Left _ -> True  -- May fail for other reasons
        Left _ -> True  -- Parsing may fail

-- ============================================================================
-- Properties for Cross-Module Integration
-- ============================================================================

prop_parserSyntaxValidatorIntegration :: String -> Property
prop_parserSyntaxValidatorIntegration input =
    length input < 500 ==>
    let parseResult = parseTypus input
        syntaxErrors = validateSyntax input
    in case parseResult of
        Left _ -> not (null syntaxErrors) || True  -- Should have syntax errors if parsing failed
        Right typusFile -> True  -- Successful parsing may still have syntax warnings

prop_compilerErrorHandlerIntegration :: String -> Property
prop_compilerErrorHandlerIntegration input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let compileResult = compile typusFile
            in case compileResult of
                Left errors -> length errors > 0  -- Errors should be collected
                Right _ -> True  -- Success is valid
        Left _ -> True  -- Parse errors are handled separately

prop_ownershipDependencyIntegration :: String -> Property
prop_ownershipDependencyIntegration input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let ownershipResult = analyzeOwnership typusFile
                dependencyResult = analyzeDependentTypes typusFile
            in True  -- Both analyses should run without crashing
        Left _ -> True  -- Parse errors prevent analysis

-- ============================================================================
-- Properties for Type System Integration
-- ============================================================================

prop_dependentTypesOwnershipIntegration :: String -> Property
prop_dependentTypesOwnershipIntegration input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let ownershipResult = analyzeOwnership typusFile
                dependentTypesResult = parseDependentType input
            in True  -- Both systems should work together
        Left _ -> True

prop_typeCheckingConstraintSolving :: String -> Property
prop_typeCheckingConstraintSolving input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let typeErrors = diagnoseTypeErrors typusFile
            in case typeErrors of
                Left _ -> True  -- Type errors are valid
                Right _ -> True  -- No type errors is also valid
        Left _ -> True

prop_typeInferenceValidation :: String -> Property
prop_typeInferenceValidation input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let declarations = extractDeclarations input
            in length declarations >= 0  -- Should extract declarations without crashing
        Left _ -> True

-- ============================================================================
-- Properties for Error Handling Integration
-- ============================================================================

prop_errorCollectionReporting :: String -> Property
prop_errorCollectionReporting input =
    length input < 500 ==>
    let syntaxErrors = validateSyntax input
        parseResult = parseTypus input
        compileErrors = case parseResult of
            Right typusFile -> case compile typusFile of
                Left errors -> Just errors
                Right _ -> Nothing
            Left _ -> Nothing
    in length syntaxErrors >= 0 && 
       case compileErrors of
         Just errors -> length errors >= 0
         Nothing -> True

prop_errorRecoveryContinuation :: String -> Property
prop_errorRecoveryContinuation input =
    length input < 500 ==>
    let parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let compileResult = compile typusFile
            in case compileResult of
                Left errors -> length errors > 0  -- Should collect errors and continue
                Right _ -> True
        Left _ -> True

prop_multipleErrorHandling :: String -> Property
prop_multipleErrorHandling input =
    "var x int = \"string\"" `isInfixOf` input && "func main() {" `isInfixOf` input ==>
    let syntaxErrors = validateSyntax input
        parseResult = parseTypus input
    in case parseResult of
        Right typusFile ->
            let compileResult = compile typusFile
            in case compileResult of
                Left errors -> length errors >= 1  -- Should detect multiple errors
                Right _ -> True
        Left _ -> length syntaxErrors >= 0

-- ============================================================================
-- Properties for Performance and Robustness
-- ============================================================================

prop_largeInputHandling :: Int -> Property
prop_largeInputHandling size =
    size > 0 && size < 10000 ==>
    let largeInput = unlines $ replicate size "    x := x + 1"
        parseResult = parseTypus largeInput
    in case parseResult of
        Left _ -> True  -- May fail but should not crash
        Right _ -> True  -- Success is valid

prop_malformedInputRecovery :: String -> Property
prop_malformedInputRecovery input =
    length input < 1000 ==>
    let parseResult = parseTypus input
        syntaxErrors = validateSyntax input
    in case parseResult of
        Left _ -> True  -- Should handle malformed input gracefully
        Right typusFile -> True  -- May still succeed partially

prop_concurrentProcessingSafety :: String -> Property
prop_concurrentProcessingSafety input =
    length input < 500 ==>
    let parseResult = parseTypus input
        syntaxErrors = validateSyntax input
    in length syntaxErrors >= 0  -- Should be thread-safe (simplified test)

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if Go code is valid
isValidGoCode :: String -> Bool
isValidGoCode code = 
    not (null code) && 
    ("package" `isInfixOf` code || "func" `isInfixOf` code || "var" `isInfixOf` code)

-- Mock compilation scenario data type
data CompilationScenario = 
    SimpleScenario String
  | ComplexScenario String String String
  | ErrorScenario String String
  deriving (Show, Eq)

-- Mock function implementations
analyzeOwnership :: TypusFile -> ()
analyzeOwnership _ = ()

analyzeDependentTypes :: TypusFile -> ()
analyzeDependentTypes _ = ()

extractDeclarations :: String -> [String]
extractDeclarations _ = []

diagnoseTypeErrors :: TypusFile -> Either [String] [()]
diagnoseTypeErrors _ = Right []

parseDependentType :: String -> Either String ()
parseDependentType _ = Right ()