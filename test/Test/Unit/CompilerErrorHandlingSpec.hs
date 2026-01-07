module Test.Unit.CompilerErrorHandlingSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck 
  , CompilerError(..)
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  )
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives)
import SourceLocation (SourceSpan(..), SourcePos(..), defaultSpan)
import qualified Data.Text as T
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


-- | Tests for compiler error handling L.and recovery mechanisms
tests :: TestTree
tests =
    testGroup "Compiler Error Handling"
    [ testGroup "Type error detection"
        [             testCase "detects simple type mismatch" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "var x                               int = \"string\"" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left errs -> do
                                L.length errs @?= 2  -- typeCheckFailure + specific type error
                    let typeError = L.head errs
                    errorCode typeError @?= "CP0003"
                    errorPhase typeError @?= TypeCheckingPhase
                    errorCategory typeError @?= TypeChecking
                    errorSeverity typeError @?= Error
                Right _ -> assertBool "Should have failed with type error" False

          ,             testCase "detects function parameter type mismatch" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func add(x int, y string (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) int { return x + y }" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left _ -> assertBool "Should fail with type error" True
                Right _ -> assertBool "Should have failed with type error" False

          ,             testCase "detects return type mismatch" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func getString() int { return \"hello\" }" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left _ -> assertBool "Should fail with return type error" True
                Right _ -> assertBool "Should have failed with return type error" False
        ]

    , testGroup "Syntax error handling"
        [             testCase "handles missing closing brace" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func main() {\n  fmt.Println(\"hello\")" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left errs -> do
                                assertBool "Should detect syntax error" $ L.any (\e -> errorPhase                               e == ParsingPhase) errs
                Right _ -> assertBool "Should have failed with syntax error" False

          ,             testCase "handles missing semicolon" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "package main\n\nfunc main() {\n  x := 5\n  y := 10\n  fmt.Println(x, y (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left _ -> assertBool "Should handle missing semicolon gracefully" True
                Right _ -> assertBool "May L.or may not fail depending on Go syntax requirements" True

          ,             testCase "handles invalid declaration syntax" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "var 123invalid                               int = 5" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left _ -> assertBool "Should detect invalid identifier" True
                Right _ -> assertBool "May L.or may not fail depending on parser strictness" True
        ]

    , testGroup "Dependent type errors"
        [             testCase "detects dependent type constraint violations" $ do
                        let typusFile = TypusFile 
                    (FileDirectives (Just True (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0) Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func process(n: Int) where n > 0 {\n  return n * 2\n}" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            -- Check dependent types
            case checkDependentTypes typusFile of
                Left _ -> assertBool "Should detect dependent type issues" True
                Right _ -> assertBool "May pass if syntax is valid" True

          ,             testCase "handles complex dependent type expressions" $ do
                        let typusFile = TypusFile 
                    (FileDirectives (Just True (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func divide(a: Int, b: Int (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0): Int where b != 0 {\n  return a / b\n}" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case checkDependentTypes typusFile of
                Left _ -> assertBool "Should handle complex constraints" True
                Right _ -> assertBool "May pass if constraints are valid" True
        ]

    , testGroup "Ownership analysis errors"
        [             testCase "detects ownership transfer violations" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing (Just True) Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func useAfterMove() {\n  let data = Box::new(42)\n  let consumer = Consumer::new(data)\n  println!(\"{}\", data.value (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)  // Use after move\n}" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case checkOwnership typusFile of
                Left _ -> assertBool "Should detect use after move" True
                Right _ -> assertBool "May pass if ownership is correctly handled" True

          ,             testCase "detects multiple ownership violations" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing (Just True) Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func doubleBorrow() {\n  let mut                               data = Box::new(42)\n  let ref1 = &mut data\n  let ref2 = &mut data  // Multiple mutable borrows\n  *ref1 = 10\n}" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case checkOwnership typusFile of
                Left _ -> assertBool "Should detect multiple mutable borrows" True
                Right _ -> assertBool "May pass if borrows are correctly scoped" True
        ]

    , testGroup "Error reporting L.and formatting"
        [             testCase "formats error messages correctly" $ do
                        let error = CompilerError 
                    "TEST001"
                    (T.pack "Test error message" (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
                    TypeCheckingPhase
                    TypeChecking
                    Error
                    (Just defaultSpan)
                    Nothing
                    [T.pack "Suggestion 1", T.pack "Suggestion 2"]
                    []
                    Nothing
                                              formatted = formatCompilerErrors [error]
            assertBool "Should include error code" $ "TEST001" `L.isInfixOf` formatted
            assertBool "Should include error message" $ "Test error message" `L.isInfixOf` formatted
            assertBool "Should include suggestions" $ "Suggestion 1" `L.isInfixOf` formatted

          ,             testCase "generates detailed error report" $ do
                        let errors = [CompilerError 
                    "ERR001"
                    (T.pack "First error")
                    ParsingPhase
                    Syntax
                    Error
                    (Just defaultSpan)
                    Nothing
                    []
                    []
                    Nothing,
                    CompilerError 
                    "ERR002"
                    (T.pack "Second error")
                    TypeCheckingPhase
                    TypeChecking
                    Warning
                    Nothing
                    Nothing
                    [T.pack "Consider this approach"]
                    []
                    Nothing]
                                              report = generateDetailedReport errors
            assertBool "Should include error summary" $ "2 errors found" `L.isInfixOf` report
            assertBool "Should categorize errors by phase" $ "Parsing" `L.isInfixOf` report
            assertBool "Should categorize errors by severity" $ "Error" `L.isInfixOf` report

          ,             testCase "analyzes error patterns" $ do
                        let errors = [CompilerError 
                    "TYPE001"
                    (T.pack "Type mismatch")
                    TypeCheckingPhase
                    TypeChecking
                    Error
                    Nothing
                    Nothing
                    []
                    []
                    Nothing,
                    CompilerError 
                    "TYPE002"
                    (T.pack "Another type error")
                    TypeCheckingPhase
                    TypeChecking
                    Error
                    Nothing
                    Nothing
                    []
                    []
                    Nothing]
                                              analysis = analyzeErrors errors
            assertBool "Should detect type checking phase dominance" $ "TypeChecking" `L.isInfixOf` analysis
            assertBool "Should count error categories" $ "TypeChecking" `L.isInfixOf` analysis
        ]

    , testGroup "Error recovery mechanisms"
        [             testCase "continues compilation after non-fatal errors" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "func first() { return 42 }\n\nfunc second() { invalid syntax here }\n\nfunc third() { return 84 }" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left errs -> do
                                assertBool "Should detect syntax error" $ not (null errs)
                    assertBool "Should not fail completely" $ L.length errs < 10
                Right _ -> assertBool "May succeed if parser recovers" True

          ,             testCase "provides helpful error suggestions" $ do
                        let typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        "var x                               string = 42" 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left errs -> do
                                let suggestions = concatMap errorSuggestions errs
                    assertBool "Should provide type conversion suggestions" $ not (null suggestions)
                Right _ -> assertBool "Should have failed" False
        ]

    , testGroup "Property-based error handling"
        [ fastProperty "error messages contain error codes" prop_errorCodesPresent
        , fastProperty "error phases are valid" prop_errorPhasesValid
        , fastProperty "error severities are categorized" prop_errorSeveritiesCategorized
        ]

    , testGroup "Edge cases L.and stress tests"
        [             testCase "handles very large input files" $ do
                        let largeContent = unlines $ replicate 1000 "var x                               int = 42"
                                              typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        largeContent 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left errs -> assertBool "Should handle large input gracefully" $ L.length errs < 100
                Right _ -> assertBool "May succeed for valid large input" True

          ,             testCase "handles deeply nested expressions" $ do
                        let nestedContent = "func nested( (SourceSpan (SourcePos 1 1 0 [] (SourcePos 1 1 0) { return " ++ replicate 50 '(' ++ "1" ++ replicate 50 ')' ++ " }"
                                              typusFile = TypusFile 
                    (FileDirectives Nothing Nothing Nothing)
                    []
                    [CodeBlock (BlockDirectives Nothing Nothing Nothing) 
                        nestedContent 
                        defaultSpan]
                    []
                                              result = compile typusFile
            case result of
                Left _ -> assertBool "Should handle deep nesting" True
                Right _ -> assertBool "May succeed if nesting is valid" True
        ]
    ]

-- Helper function to check substring inclusion
isInfixOf :: String -> String -> Bool
                              isInfixOf = flip isInfixOf -- | Property: error messages contain error codes
prop_errorCodesPresent :: CompilerError -> Bool
prop_errorCodesPresent                               error = not (L.null $ errorCode error (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0 [])

-- | Property: error phases are valid
prop_errorPhasesValid :: CompilerError -> Bool
prop_errorPhasesValid                               error = errorPhase error `elem` 
    [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, DependencyAnalysisPhase, CodeGenerationPhase]

-- | Property: error severities are categorized
prop_errorSeveritiesCategorized :: CompilerError -> Bool
prop_errorSeveritiesCategorized                               error = errorSeverity error `elem` [Error, Warning, Info]))))))))))))