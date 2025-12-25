{-# LANGUAGE CPP #-}
module Test.Unit.AdditionalCabalTestsSpec (tests) where

import Data.List (isInfixOf, nub)
import Data.Either (isLeft, isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.QuickCheck 
    ( Arbitrary(..)
    , Gen
    , Property
    , Testable(..)
    , arbitrary
    , choose
    , elements
    , listOf
    , oneof
    , property
    , (===)
    , (==>)
    , QuickCheck.quickCheck
    )

import Parser (parseTypus)
import SourceLocation 
    ( SourcePos(..)
    , SourceSpan(..)
    , spanStart
    , spanEnd
    , posLine
    , posColumn
    )
import Compiler 
    ( compileTypus
    , CompilationResult(..)
    )
import ErrorHandler 
    ( handleError
    , ErrorSeverity(..)
    )
import Ownership 
    ( analyzeOwnership
    , OwnershipResult(..)
    )
import DependentTypesParser 
    ( parseDependentType
    , DependentType(..)
    )

-- | Test case 1: Compiler error handling for malformed syntax
testCompilerErrorHandling :: TestTree
testCompilerErrorHandling =
  testCase "Compiler handles malformed syntax gracefully" $ do
    let malformedSource = unlines
          [ "package main"
          , "func main() {"
          , "    if x > 0 {"
          , "        println(\"test\")"
          , "    // missing closing brace"
          , "}"
          ]
    result <- compileTypus malformedSource
    case result of
      Left err -> do
        assertBool "Error should contain syntax error information" 
                   ("syntax" `isInfixOf` err || "parse" `isInfixOf` err)
        assertBool "Error should mention line number" 
                   (any (`isInfixOf` err) ["line", "Line", ":", "2"])
      Right _ -> assertFailure "Expected compilation to fail with malformed syntax"

-- | Test case 2: Dependent type parsing and validation
testDependentTypeParsing :: TestTree
testDependentTypeParsing =
  testCase "Dependent type parsing works correctly" $ do
    let validDependentType = "Vector{n : Nat} where n > 0"
    case parseDependentType validDependentType of
      Left err -> assertFailure $ "Failed to parse valid dependent type: " ++ err
      Right depType -> do
        assertBool "Dependent type should have constraints" 
                   (not $ null $ dtConstraints depType)
        assertBool "Dependent type should have type parameters" 
                   (not $ null $ dtTypeParams depType)

-- | Test case 3: Ownership analysis for move operations
testOwnershipAnalysis :: TestTree
testOwnershipAnalysis =
  testCase "Ownership analysis correctly identifies moved values" $ do
    let sourceWithMove = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    moved := data  // move operation"
          , "    // data should no longer be usable here"
          , "    println(moved)"
          , "}"
          ]
    result <- analyzeOwnership sourceWithMove
    case result of
      Left err -> assertFailure $ "Ownership analysis failed: " ++ err
      Right ownershipResult -> do
        let movedVars = orMovedVariables ownershipResult
        assertBool "Should identify moved variables" 
                   (not $ null movedVars)
        assertBool "Should report 'data' as moved" 
                   ("data" `elem` movedVars)

-- | Test case 4: Source location tracking for errors
testSourceLocationTracking :: TestTree
testSourceLocationTracking =
  testCase "Source locations are correctly tracked for errors" $ do
    let sourceWithError = unlines
          [ "package main"
          , "func main() {"
          , "    var x int"
          , "    x = \"string\"  // type mismatch"
          , "}"
          ]
    result <- compileTypus sourceWithError
    case result of
      Left err -> do
        assertBool "Error should include line information" 
                   (any (`isInfixOf` err) ["3", "4", "line"])
        assertBool "Error should mention type mismatch" 
                   ("type" `isInfixOf` err && "mismatch" `isInfixOf` err)
      Right _ -> assertFailure "Expected compilation to fail with type error"

-- | Test case 5: Error severity classification
testErrorSeverityClassification :: TestTree
testErrorSeverityClassification =
  testCase "Error severity is correctly classified" $ do
    let syntaxError = "Unexpected token '{' at line 3"
        typeError = "Type mismatch: expected int, got string"
        warning = "Unused variable 'x' declared but never used"
    
    assertBool "Syntax errors should be classified as Error" 
               (handleError syntaxError == Error)
    assertBool "Type errors should be classified as Error" 
               (handleError typeError == Error)
    assertBool "Unused variable warnings should be classified as Warning" 
               (handleError warning == Warning)

-- | Test case 6: Complex dependent type constraints
testComplexDependentTypes :: TestTree
testComplexDependentTypes =
  testCase "Complex dependent type constraints are handled" $ do
    let complexType = "Matrix{m, n : Nat} where m > 0, n > 0, m * n < 1000"
    case parseDependentType complexType of
      Left err -> assertFailure $ "Failed to parse complex dependent type: " ++ err
      Right depType -> do
        let constraints = dtConstraints depType
        assertBool "Should have multiple constraints" (length constraints >= 3)
        assertBool "Should include size constraint" 
                   (any ("m * n < 1000" `isInfixOf`) constraints)

-- | Test case 7: Ownership transfer in function calls
testOwnershipTransfer :: TestTree
testOwnershipTransfer =
  testCase "Ownership transfer in function calls is tracked" $ do
    let sourceWithFunctionCall = unlines
          [ "//! ownership: on"
          , "package main"
          , "func consume(data []int) {"
          , "    // data is consumed here"
          , "}"
          , "func main() {"
          , "    data := make([]int, 10)"
          , "    consume(data)  // ownership transferred"
          , "    // data should no longer be usable"
          , "}"
          ]
    result <- analyzeOwnership sourceWithFunctionCall
    case result of
      Left err -> assertFailure $ "Ownership analysis failed: " ++ err
      Right ownershipResult -> do
        let transferredVars = orTransferredVariables ownershipResult
        assertBool "Should identify transferred variables" 
                   (not $ null transferredVars)
        assertBool "Should report 'data' as transferred" 
                   ("data" `elem` transferredVars)

-- | Test case 8: Error recovery and multiple error reporting
testErrorRecovery :: TestTree
testErrorRecovery =
  testCase "Compiler reports multiple errors when possible" $ do
    let sourceWithMultipleErrors = unlines
          [ "package main"
          , "func main() {"
          , "    var x int = \"string\"  // type error 1"
          , "    var y string = 123     // type error 2"
          , "    if x > 0 {             // use of incorrectly typed variable"
          , "        println(y)"
          , "    }"
          , "}"
          ]
    result <- compileTypus sourceWithMultipleErrors
    case result of
      Left err -> do
        assertBool "Should report multiple errors" 
                   (length (lines err) >= 2 || "and" `isInfixOf` err)
        assertBool "Should mention type mismatch" 
                   ("type" `isInfixOf` err)
      Right _ -> assertFailure "Expected compilation to fail with multiple errors"

-- | QuickCheck property: Round-trip parsing for valid syntax
roundTripParseProperty :: Property
roundTripParseProperty =
  forAll validGoSyntax $ \source ->
    isRight (parseTypus source) ==> 
    case parseTypus source of
      Right parsed -> isRight (parseTypus (show parsed))
      Left _ -> property True

-- | QuickCheck property: Ownership analysis preserves variable count
ownershipVariableCountProperty :: Property
ownershipVariableCountProperty =
  forAll validOwnershipCode $ \source ->
    case analyzeOwnership source of
      Right result -> length (orDeclaredVariables result) >= length (orMovedVariables result)
      Left _ -> property True

-- | QuickCheck test cases
quickCheckTests :: TestTree
quickCheckTests =
  testGroup "QuickCheck Property Tests"
    [ testCase "Round-trip parsing preserves structure" $ do
        quickCheck roundTripParseProperty
    , testCase "Ownership analysis preserves variable count" $ do
        quickCheck ownershipVariableCountProperty
    ]

-- | Helper generators for QuickCheck
validGoSyntax :: Gen String
validGoSyntax = oneof
  [ return $ unlines ["package main", "func main() {}"]
  , return $ unlines ["package main", "func add(x, y int) int { return x + y }", "func main() {}"]
  , return $ unlines ["package main", "type Point struct { X, Y int }", "func main() {}"]
  ]

validOwnershipCode :: Gen String
validOwnershipCode = oneof
  [ return $ unlines ["//! ownership: on", "package main", "func main() { x := 42 }"]
  , return $ unlines ["//! ownership: on", "package main", "func main() { data := make([]int, 10) }"]
  , return $ unlines ["//! ownership: on", "package main", "func test() []int { return nil }", "func main() {}"]
  ]

-- | Aggregate all tests
tests :: TestTree
tests =
  testGroup "Additional Cabal Tests"
    [ testCompilerErrorHandling
    , testDependentTypeParsing
    , testOwnershipAnalysis
    , testSourceLocationTracking
    , testErrorSeverityClassification
    , testComplexDependentTypes
    , testOwnershipTransfer
    , testErrorRecovery
    , quickCheckTests
    ]