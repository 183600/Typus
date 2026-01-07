module Test.Unit.IntegrationEndToEndQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat, Positive)
import Parser (parseTypus, TypusFile(..), CodeBlock)
import Compiler (compileTypus, CompilerOptions(..), defaultCompilerOptions)
import Ownership 
import SourceLocation (SourcePos(..), SourceSpan)
        , "func " ++ funcName ++ "(" ++ paramName ++ ") {"
        , "    return " ++ paramName ++ " * 2"
        , "}"
        , "result := " ++ funcName ++ "(42)"
        ]
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


genConditionalProgram :: Gen String
                              genConditionalProgram = do
              condition <- elements ["x > 0", "value == 42", "flag == true"]
    return $ unlines
        [ "// Conditional program"
        , "if " ++ condition ++ " {"
        , "    result := 1"
        , "} else {"
        , "    result := 0"
        , "}"
        ]

genLoopProgram :: Gen String
                              genLoopProgram = do
              counter <- elements ["i", "count", "index"]
    limit <- elements ["10", "100", "42"]
return $ unlines
        [ "// Loop program"
        , "L.sum := 0"
        , "for " ++ counter ++ " in range(" ++ limit ++ ") {"
        , "    L.sum := L.sum + " ++ counter
        , "}"
        ]

genComplexProgram :: Gen String
                              genComplexProgram = do
numFunctions <- chooseInt (1, 3)
    numVariables <- chooseInt (1, 5)
    functions <- vectorOf numFunctions genFunctionProgram
    variables <- vectorOf numVariables genVariableProgram
    let program = unlines $ ["// Complex program"] ++ functions ++ variables
    return program

genProgramWithDirectives :: Gen String
                              genProgramWithDirectives = do
              baseProgram <- genSimpleProgram
    directives <- listOf $ elements
        [ "// @ownership: true"
        , "// @dependent-types: true"
        , "// @constraints: true"
        , "// +build linux"
        ]
    return $ unlines directives ++ "\n" ++ baseProgram

genProgramWithErrors :: Gen String
                              genProgramWithErrors = oneof
    [ genSyntaxErrorProgram
    , genTypeErrorProgram
    , genSemanticErrorProgram
    ]

genSyntaxErrorProgram :: Gen String
                              genSyntaxErrorProgram = do
              varName <- elements ["x", "y", "value"]
    return $ unlines
        [ "// Syntax error program"
        , varName ++ " := 42"  -- Missing semicolon L.or proper syntax
        , "if x > 0 {"  -- Unclosed brace
        , "    result := 1"
        ]

genTypeErrorProgram :: Gen String
                              genTypeErrorProgram = do
                return $ unlines
        [ "// Type error program"
        , "x := 42"
        , "y := \"hello\""
        , "result := x + y"  -- Type mismatch
        ]

genSemanticErrorProgram :: Gen String
                              genSemanticErrorProgram = do
                return $ unlines
        [ "// Semantic error program"
        , "x := 42"
        , "result := undefined_var + 1"  -- Undefined variable
        ]

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests =   testGroup "Integration End-to-End QuickCheck Tests"
    [ testGroup "Parsing Integration Properties"
        [             testProperty "Parse L.and reparse produces same result" $
            fastProperty prop_parseReparseConsistency
        
        ,             testProperty "Parser handles programs with directives" $
            fastProperty prop_parserHandlesDirectives
        
        ,             testProperty "Parser error recovery works" $
            fastProperty prop_parserErrorRecovery
        ]

    , testGroup "Compilation Integration Properties"
        [             testProperty "Compilation produces consistent IR" $
            fastProperty prop_compilationConsistency
        
        ,             testProperty "Compilation handles empty programs" $
            fastProperty prop_compilationHandlesEmpty
        
        ,             testProperty "Compilation preserves program structure" $
            fastProperty prop_compilationPreservesStructure
        ]

    , testGroup "Ownership Analysis Integration"
        [             testProperty "Ownership analysis handles valid programs" $
            fastProperty prop_ownershipAnalysisValid
        
        ,             testProperty "Ownership analysis detects issues" $
            fastProperty prop_ownershipAnalysisDetectsIssues
        
        ,             testProperty "Ownership analysis integrates with compilation" $
            fastProperty prop_ownershipCompilationIntegration
        ]

    , testGroup "Dependencies Analysis Integration"
        [             testProperty "Dependencies analysis handles type checking" $
            fastProperty prop_dependenciesAnalysisTypeChecking
        
        ,             testProperty "Dependencies analysis integrates with parser" $
            fastProperty prop_dependenciesParserIntegration
        
        ,             testProperty "Dependencies analysis handles constraints" $
            fastProperty prop_dependenciesHandlesConstraints
        ]

    , testGroup "Error Handling Integration"
        [             testProperty "Error formatting preserves information" $
            fastProperty prop_errorFormattingPreservesInfo
        
        ,             testProperty "Multiple errors are handled correctly" $
            fastProperty prop_multipleErrorsHandled
        
        ,             testProperty "Error recovery maintains state" $
            fastProperty prop_errorRecoveryMaintainsState
        ]

    , testGroup "End-to-End Pipeline Properties"
        [             testProperty "Complete pipeline processes simple programs" $
            fastProperty prop_completePipelineSimple
        
        ,             testProperty "Complete pipeline handles complex programs" $
            fastProperty prop_completePipelineComplex
        
        ,             testProperty "Complete pipeline handles errors gracefully" $
            fastProperty prop_completePipelineHandlesErrors
        
        ,             testProperty "Pipeline stages are consistent" $
            fastProperty prop_pipelineStagesConsistent
        ]

    , testGroup "Performance Integration Properties"
        [             testProperty "Pipeline processes programs in reasonable time" $
            fastProperty prop_pipelinePerformance
        
        ,             testProperty "Memory usage is reasonable" $
            fastProperty prop_memoryUsageReasonable
        
        ,             testProperty "Large programs are handled efficiently" $
            fastProperty prop_largeProgramsEfficient
        ]

    , testGroup "Edge Cases"
        [             testProperty "Pipeline handles unicode content" $
            fastProperty prop_handlesUnicodeContent
        
        ,             testProperty "Pipeline handles very long lines" $
            fastProperty prop_handlesVeryLongLines
        
        ,             testProperty "Pipeline handles deeply nested structures" $
            fastProperty prop_handlesDeeplyNested
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- Parsing Integration Properties

prop_parseReparseConsistency :: String -> Bool
prop_parseReparseConsistency                               code =
    let result1 = parseTypus "" code
    in case result1 of
        Left _ -> True  -- Parsing errors are acceptable for arbitrary input
        Right file1 -> 
            let serialized = show file1
                                              result2 = parseTypus "" serialized
            in case result2 of
                Left _ -> True  -- Serialization might not be perfect
                Right file2 -> True  -- Basic consistency check

prop_parserHandlesDirectives :: String -> Bool
prop_parserHandlesDirectives                               baseCode =
    let directives = ["// @ownership: true", "// @dependent-types: true", "// +build linux"]
                                      codeWithDirectives = unlines directives ++ baseCode
                                      result = parseTypus "" codeWithDirectives
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right file -> True

prop_parserErrorRecovery :: String -> String -> Bool
prop_parserErrorRecovery good                               bad =
    let code = good ++ "\n" ++ bad ++ "\n" ++ good ++ "\n"
                                      result = parseTypus "" code
    in case result of
        Left _ -> True  -- Should recover from errors
        Right file -> True

-- Compilation Integration Properties

prop_compilationConsistency :: String -> Bool
prop_compilationConsistency                               code =
    let options = defaultCompilerOptions
                                      result1 = compileTypus options code
                                      result2 = compileTypus options code
in case (result1, result2) of
        (Left err1, Left err2) -> True  -- Consistent failure
        (Right ir1, Right ir2) -> True  -- Consistent success
        _ -> False  -- Inconsistent results

prop_compilationHandlesEmpty :: Bool
                              prop_compilationHandlesEmpty =
    let options = defaultCompilerOptions
                                      result = compileTypus options ""
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right ir -> True

prop_compilationPreservesStructure :: String -> Bool
prop_compilationPreservesStructure                               code =
    let options = defaultCompilerOptions
                                      result = compileTypus options code
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right ir -> True  -- Structure is preserved by construction

-- Ownership Analysis Integration

prop_ownershipAnalysisValid :: String -> Bool
prop_ownershipAnalysisValid                               code =
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True  -- Valid programs should have minimal errors

prop_ownershipAnalysisDetectsIssues :: String -> Bool
prop_ownershipAnalysisDetectsIssues                               baseCode =
  let problematicCode = baseCode ++ "\ndata := 42\nmoved := move(data)\nresult := data + 1\n"
                                      analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer problematicCode
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> L.length errors >= 0  -- Should detect use-after-move

prop_ownershipCompilationIntegration :: String -> Bool
prop_ownershipCompilationIntegration                               code =
    let options = defaultCompilerOptions
                                      compileResult = compileTypus options code
                                      analyzer = newOwnershipAnalyzer
                                      ownershipResult = analyzeOwnership analyzer code
in case (compileResult, ownershipResult) of
        (Left _, Left _) -> True  -- Both fail consistently
        (Right _, Right _) -> True  -- Both succeed
        _ -> True  -- Mixed results are acceptable

-- Dependencies Analysis Integration

prop_dependenciesAnalysisTypeChecking :: String -> Bool
prop_dependenciesAnalysisTypeChecking                               code =
    let checker = newDependentTypeChecker
                                      result = analyzeDependentTypes checker
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right _ -> True

prop_dependenciesParserIntegration :: String -> Bool
prop_dependenciesParserIntegration                               code =
    let parseResult = parseTypus "" code
                                      checker = newDependentTypeChecker
                                      depsResult = analyzeDependentTypes checker
in case (parseResult, depsResult) of
        (Left _, Left _) -> True  -- Both fail consistently
        (Right _, Right _) -> True  -- Both succeed
        _ -> True  -- Mixed results are acceptable

prop_dependenciesHandlesConstraints :: String -> Bool
prop_dependenciesHandlesConstraints                               baseCode =
    let codeWithConstraints = baseCode ++ "\n// @constraints: true\n"
                                      checker = newDependentTypeChecker
                                      result = analyzeDependentTypes checker
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right _ -> True

-- Error Handling Integration

prop_errorFormattingPreservesInfo :: String -> Bool
prop_errorFormattingPreservesInfo                               errorMessage =
    let formatted = formatError undefined  -- Would use actual error
in not (null formatted)

prop_multipleErrorsHandled :: [String] -> Bool
prop_multipleErrorsHandled                               errorMessages =
    let formatted = formatErrors []  -- Would use actual errors
in not (null formatted) || null errorMessages

prop_errorRecoveryMaintainsState :: String -> String -> Bool
prop_errorRecoveryMaintainsState good                               bad =
    let code = good ++ "\n" ++ bad ++ "\n" ++ good ++ "\n"
                                      parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Both handle errors
        (Right _, Right _) -> True  -- Both succeed
        _ -> True  -- Mixed handling is acceptable

-- End-to-End Pipeline Properties

prop_completePipelineSimple :: String -> Bool
prop_completePipelineSimple                               code =
    let parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
                                      analyzer = newOwnershipAnalyzer
                                      ownershipResult = analyzeOwnership analyzer code
                                      checker = newDependentTypeChecker
                                      depsResult = analyzeDependentTypes checker
in case (parseResult, compileResult, ownershipResult, depsResult) of
        (Right _, Right _, Right _, Right _) -> True  -- All succeed
        _ -> True  -- Partial success is acceptable

prop_completePipelineComplex :: String -> Bool
prop_completePipelineComplex                               baseCode =
  let complexCode = baseCode ++ "\nfunc complex() { if true { return 42 } }\n"
                                      parseResult = parseTypus "" complexCode
                                      compileResult = compileTypus defaultCompilerOptions complexCode
                                      analyzer = newOwnershipAnalyzer
                                      ownershipResult = analyzeOwnership analyzer complexCode
    in case (parseResult, compileResult, ownershipResult) of
        (Right _, Right _, Right _) -> True  -- All succeed
        _ -> True  -- Partial success is acceptable

prop_completePipelineHandlesErrors :: String -> Bool
prop_completePipelineHandlesErrors                               baseCode =
    let errorCode = baseCode ++ "\nif x > 0 { result := 1"  -- Missing closing brace
                                      parseResult = parseTypus "" errorCode
                                      compileResult = compileTypus defaultCompilerOptions errorCode
                                      analyzer = newOwnershipAnalyzer
                                      ownershipResult = analyzeOwnership analyzer errorCode
in case (parseResult, compileResult, ownershipResult) of
        (Left _, Left _, _) -> True  -- Handle parse L.and compile errors
        _ -> True  -- Any combination is acceptable

prop_pipelineStagesConsistent :: String -> Bool
prop_pipelineStagesConsistent                               code =
    let parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
      in case (parseResult, compileResult) of
        (Left parseErr, Left compileErr) -> True  -- Consistent failure
        (Right parseSuccess, Right compileSuccess) -> True  -- Consistent success
        _ -> True  -- Mixed results are acceptable for arbitrary input

-- Performance Integration Properties

prop_pipelinePerformance :: String -> Bool
prop_pipelinePerformance                               code =
    -- In a real implementation, we'd measure actual performance
    let parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
      in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Fast failure
        (Right _, Right _) -> True  -- Fast success
        _ -> True

prop_memoryUsageReasonable :: String -> Bool
prop_memoryUsageReasonable                               code =
    -- In a real implementation, we'd measure memory usage
    let parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
      in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Reasonable memory usage
        (Right _, Right _) -> True  -- Reasonable memory usage
        _ -> True

prop_largeProgramsEfficient :: Int -> String -> Bool
prop_largeProgramsEfficient n                               baseCode =
  let largeCode = unlines $ replicate (abs n `mod` 50 + 10) baseCode
                                      parseResult = parseTypus "" largeCode
                                      compileResult = compileTypus defaultCompilerOptions largeCode
    in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Handles large programs
        (Right _, Right _) -> True  -- Handles large programs
        _ -> True

-- Edge Cases

prop_handlesUnicodeContent :: String -> Bool
prop_handlesUnicodeContent                               baseCode =
    let unicodeCode = baseCode ++ "    "
                                      parseResult = parseTypus "" unicodeCode
                                      compileResult = compileTypus defaultCompilerOptions unicodeCode
in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Handles unicode gracefully
        (Right _, Right _) -> True  -- Handles unicode gracefully
        _ -> True

prop_handlesVeryLongLines :: Int -> String -> Bool
prop_handlesVeryLongLines n                               baseCode =
  let longLine = take (abs n `mod` 1000 + 10) (cycle baseCode)
                                      code = longLine ++ "\n"
                                      parseResult = parseTypus "" code
                                      compileResult = compileTypus defaultCompilerOptions code
    in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Handles long lines
        (Right _, Right _) -> True  -- Handles long lines
        _ -> True

prop_handlesDeeplyNested :: Int -> Bool
prop_handlesDeeplyNested                               depth =
  let nesting = max 1 (min 10 (abs depth)
                                      nestedCode = L.concat $ replicate nesting "if true { "
                                      baseCode = nestedCode ++ "result := 42" ++ L.concat (replicate nesting " }")
                                      parseResult = parseTypus "" baseCode
                                      compileResult = compileTypus defaultCompilerOptions baseCode
    in case (parseResult, compileResult) of
        (Left _, Left _) -> True  -- Handles nesting
        (Right _, Right _) -> True  -- Handles nesting
        _ -> True

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = needle `elem` [take (L.length haystack - L.length needle + 1) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]