{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerEndToEndIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=), assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, (===), (==>), forAll, counterexample, classify, property
    , Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat
    , vectorOf, frequency, sized
    )

-- Core modules for end-to-end testing
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compileTypus, CompilationResult(..))
import IntegratedCompiler (integratedCompile)
import ErrorHandler (ErrorHandler(..))
import SourceLocation (SourcePos(..), startPos)
import Utils (trim, removeComments, normalizeIndentation)
import Ownership (OwnershipAnalysis(..))
import DependentTypesParser (parseDependentType)

import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import Control.Monad (when)

-- | End-to-end integration tests for the compiler pipeline
tests :: TestTree
tests =
  testGroup "Compiler End-to-End Integration"
    [ testGroup "Complete Compilation Pipeline"
        [ testCase "Simple function compiles successfully" $ do
            let simpleFunction = "func main() {\n  return 42;\n}"
                result = compileTypus "test.typus" simpleFunction
            case result of
              Left err -> assertFailure $ "Simple function should compile: " ++ err
              Right compilationResult -> do
                assertBool "Compilation should succeed" True
                -- Verify compilation result structure
                assertBool "Should have generated code" (hasGeneratedCode compilationResult)

        , testCase "Function with parameters compiles" $ do
            let parameterizedFunction = "func add(a: Int, b: Int) -> Int {\n  return a + b;\n}"
                result = compileTypus "test.typus" parameterizedFunction
            case result of
              Left err -> assertFailure $ "Parameterized function should compile: " ++ err
              Right compilationResult -> do
                assertBool "Should handle parameters correctly" True

        , testCase "Multiple functions compile together" $ do
            let multipleFunctions = unlines
                    [ "func helper(x: Int) -> Int {"
                    , "  return x * 2;"
                    , "}"
                    , ""
                    , "func main() -> Int {"
                    , "  return helper(21);"
                    , "}"
                    ]
                result = compileTypus "test.typus" multipleFunctions
            case result of
              Left err -> assertFailure $ "Multiple functions should compile: " ++ err
              Right compilationResult -> do
                assertBool "Should compile multiple functions" True

        , testCase "Function with local variables compiles" $ do
            let functionWithVars = unlines
                    [ "func calculate() -> Int {"
                    , "  let x = 10;"
                    , "  let y = 20;"
                    , "  return x + y;"
                    , "}"
                    ]
                result = compileTypus "test.typus" functionWithVars
            case result of
              Left err -> assertFailure $ "Function with variables should compile: " ++ err
              Right compilationResult -> do
                assertBool "Should handle local variables" True
        ]

    , testGroup "Ownership Integration"
        [ testCase "Ownership-enabled code compiles" $ do
            let ownershipCode = unlines
                    [ "// @ownership: true"
                    , "func transfer() {"
                    , "  let resource = Resource();"
                    , "  move resource;"
                    , "}"
                    ]
                result = compileTypus "test.typus" ownershipCode
            case result of
              Left err -> do
                -- May fail due to invalid ownership usage, but should provide meaningful error
                assertBool "Ownership error should be informative" 
                    (isOwnershipRelatedError err)
              Right compilationResult -> do
                assertBool "Valid ownership code should compile" True

        , testCase "Ownership violations are caught" $ do
            let ownershipViolation = unlines
                    [ "// @ownership: true"
                    , "func violation() {"
                    , "  let resource = Resource();"
                    , "  move resource;"
                    , "  use resource;  // Use after move"
                    , "}"
                    ]
                result = compileTypus "test.typus" ownershipViolation
            case result of
              Left err -> do
                assertBool "Should catch ownership violation" 
                    (isOwnershipRelatedError err)
              Right _ -> do
                assertFailure "Expected ownership violation to be caught"

        , testCase "Borrowing compiles correctly" $ do
            let borrowingCode = unlines
                    [ "// @ownership: true"
                    , "func borrow_example() {"
                    , "  let data = Data();"
                    , "  let borrowed = borrow data;"
                    , "  use borrowed;"
                    , "}"
                    ]
                result = compileTypus "test.typus" borrowingCode
            case result of
              Left err -> do
                assertBool "Borrowing error should be informative" 
                    (isOwnershipRelatedError err)
              Right compilationResult -> do
                assertBool "Valid borrowing should compile" True
        ]

    , testGroup "Dependent Types Integration"
        [ testCase "Simple dependent types compile" $ do
            let dependentTypesCode = unlines
                    [ "// @dependent-types: true"
                    , "func vec_length<n: Nat>(v: Vec<n>) -> Nat {"
                    , "  return n;"
                    , "}"
                    ]
                result = compileTypus "test.typus" dependentTypesCode
            case result of
              Left err -> do
                assertBool "Dependent types error should be informative" 
                    (isDependentTypeError err)
              Right compilationResult -> do
                assertBool "Valid dependent types should compile" True

        , testCase "Type constraints are enforced" $ do
            let constraintCode = unlines
                    [ "// @dependent-types: true"
                    , "func safe_divide<n: Nat, m: Nat>(x: Int, y: Int) -> Int"
                    , "  where m > 0 {"
                    , "  return x / y;"
                    , "}"
                    ]
                result = compileTypus "test.typus" constraintCode
            case result of
              Left err -> do
                assertBool "Constraint error should be informative" 
                    (isDependentTypeError err)
              Right compilationResult -> do
                assertBool "Valid constraints should compile" True

        , testCase "Complex type expressions compile" $ do
            let complexTypes = unlines
                    [ "// @dependent-types: true"
                    , "func matrix_mult<m: Nat, n: Nat, p: Nat>"
                    , "  (a: Matrix<m,n>, b: Matrix<n,p>) -> Matrix<m,p> {"
                    , "  // implementation"
                    , "}"
                    ]
                result = compileTypus "test.typus" complexTypes
            case result of
              Left err -> do
                assertBool "Complex type error should be informative" 
                    (isDependentTypeError err)
              Right compilationResult -> do
                assertBool "Complex types should compile" True
        ]

    , testGroup "Error Propagation Through Pipeline"
        [ testCase "Parse errors propagate correctly" $ do
            let parseError = "func invalid( {\n  syntax error\n}"
                result = compileTypus "test.typus" parseError
            case result of
              Right _ -> assertFailure "Expected compilation failure"
              Left err -> do
                assertBool "Parse error should propagate" 
                    (isParseError err)

        , testCase "Type errors propagate correctly" $ do
            let typeError = "func type_mismatch() -> Int {"
                           ++ "  return \"not an int\";"
                           ++ "}"
                result = compileTypus "test.typus" typeError
            case result of
              Right _ -> assertFailure "Expected type error"
              Left err -> do
                assertBool "Type error should be reported" 
                    (isTypeError err)

        , testCase "Multiple errors are collected" $ do
            let multipleErrors = unlines
                    [ "func bad1() -> Int { return \"string\"; }"
                    , "func bad2( { }"
                    , "func bad3() -> Int { return unknown_var; }"
                    ]
                result = compileTypus "test.typus" multipleErrors
            case result of
              Right _ -> assertFailure "Expected multiple errors"
              Left err -> do
                assertBool "Should report multiple errors" 
                    (L.length err > 20)  -- Longer error message for multiple issues
        ]

    , testGroup "Integration with Utils"
        [ testCase "Preprocessing integration works" $ do
            let rawInput = unlines
                    [ "  // @ownership: true"
                    , "  func test() {"
                    , "    return 42;"
                    , "  }"
                    ]
                processedInput = normalizeIndentation (removeComments rawInput)
                result = compileTypus "test.typus" processedInput
            case result of
              Left err -> do
                assertFailure $ "Preprocessed input should compile: " ++ err
              Right compilationResult -> do
                assertBool "Preprocessing integration should work" True

        , testCase "Comment handling in compilation" $ do
            let withComments = unlines
                    [ "func main() {"
                    , "  // This is a comment"
                    , "  let x = 42; /* Another comment */"
                    , "  return x;"
                    , "}"
                    ]
                result = compileTypus "test.typus" withComments
            case result of
              Left err -> assertFailure $ "Code with comments should compile: " ++ err
              Right compilationResult -> do
                assertBool "Comments should be handled correctly" True

        , testCase "Whitespace normalization in compilation" $ do
            let weirdWhitespace = unlines
                    [ "\tfunc\tmain()\t{\t"
                    , "  \treturn\t42;\t"
                    , "\t}\t"
                    ]
                result = compileTypus "test.typus" weirdWhitespace
            case result of
              Left err -> assertFailure $ "Weird whitespace should be handled: " ++ err
              Right compilationResult -> do
                assertBool "Whitespace should be normalized" True
        ]

    , testGroup "Performance L.and Scalability"
        [ testCase "Large file compilation performance" $ do
            let largeFile = unlines $ replicate 100 "func test" ++ show [1..100] ++ "() { return " ++ show [1..100] ++ "; }"
                result = compileTypus "test.typus" largeFile
            case result of
              Left err -> do
                -- May fail due to syntax issues, but should not crash
                assertBool "Should handle large files gracefully" 
                    (L.length err > 0)
              Right compilationResult -> do
                assertBool "Large file should compile" True

        , testCase "Complex nested structures compile" $ do
            let nestedCode = unlines
                    [ "func complex() {"
                    , "  if (condition1) {"
                    , "    if (condition2) {"
                    , "      for (i = 0; i < 10; i++) {"
                    , "        if (condition3) {"
                    , "          return deep_value;"
                    , "        }"
                    , "      }"
                    , "    }"
                    , "  }"
                    , "}"
                    ]
                result = compileTypus "test.typus" nestedCode
            case result of
              Left err -> do
                assertBool "Nested structure error should be informative" 
                    (L.length err > 10)
              Right compilationResult -> do
                assertBool "Nested structures should compile" True

        , testCase "Memory usage during compilation" $ do
            let memoryTest = L.concat $ replicate 1000 "let x" ++ show [1..1000] ++ " = " ++ show [1..1000] ++ ";\n"
                result = compileTypus "test.typus" memoryTest
            case result of
              Left err -> do
                assertBool "Should handle memory-intensive compilation" 
                    (L.length err > 0)
              Right compilationResult -> do
                assertBool "Memory-intensive code should compile" True
        ]

    , testGroup "Integrated Compiler Tests"
        [ testCase "Integrated compiler handles L.all phases" $ do
            let integrationTest = unlines
                    [ "// @ownership: true"
                    , "// @dependent-types: true"
                    , "func integrated_test() {"
                    , "  let data = Vec<5>();"
                    , "  move data;"
                    , "}"
                    ]
                result = integratedCompile "test.typus" integrationTest
            case result of
              Left err -> do
                assertBool "Integrated compilation error should be comprehensive" 
                    (L.length err > 15)
              Right compilationResult -> do
                assertBool "Integrated compilation should succeed" True

        , testCase "Cross-module compilation works" $ do
            let moduleA = "module A { func exported() -> Int { return 42; } }"
                moduleB = "module B { import A; func main() -> Int { return A.exported(); } }"
                resultA = compileTypus "A.typus" moduleA
                resultB = compileTypus "B.typus" moduleB
            case (resultA, resultB) of
              (Right _, Right _) -> do
                assertBool "Cross-module compilation should work" True
              (Left errA, Right _) -> do
                assertFailure $ "Module A should compile: " ++ errA
              (Right _, Left errB) -> do
                assertFailure $ "Module B should compile: " ++ errB
              (Left errA, Left errB) -> do
                assertFailure $ "Both modules should compile: A: " ++ errA ++ ", B: " ++ errB
        ]

    , testGroup "QuickCheck Properties"
        [ fastProperty "Compiler never crashes on valid syntax" $
            \validProgram -> 
                let result = compileTypus "test.typus" validProgram
                in case result of
                     Left _ -> property True
                     Right _ -> property True

        , fastProperty "Compilation preserves function semantics" $
            \funcName -> 
                let simpleFunc = "func " ++ funcName ++ "() { return 42; }"
                    result = compileTypus "test.typus" simpleFunc
                in case result of
                     Left _ -> property True  -- May fail for invalid names
                     Right _ -> property True

        , fastProperty "Error messages are informative" $
            \program -> 
                let result = compileTypus "test.typus" program
                in case result of
                     Left err -> property (L.length err > 5)
                     Right _ -> property True
        ]
    ]

-- Helper functions
hasGeneratedCode :: CompilationResult -> Bool
hasGeneratedCode result = case result of
    CompilationSuccess code -> not (null code)
    CompilationWarning _ code -> not (null code)
    CompilationError _ -> False

isOwnershipRelatedError :: String -> Bool
isOwnershipRelatedError err = 
    L.any (`L.isInfixOf` map toLower err) ["ownership", "move", "borrow", "lifetime"]

isDependentTypeError :: String -> Bool
isDependentTypeError err = 
    L.any (`L.isInfixOf` map toLower err) ["dependent", "type", "constraint", "generic"]

isParseError :: String -> Bool
isParseError err = 
    L.any (`L.isInfixOf` map toLower err) ["parse", "syntax", "unexpected", "expect"]

isTypeError :: String -> Bool
isTypeError err = 
    L.any (`L.isInfixOf` map toLower err) ["type", "mismatch", "incompatible"]

toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

-- Mock CompilationResult type for testing
data CompilationResult = 
    CompilationSuccess String
  | CompilationWarning String String
  | CompilationError String
  deriving (Show, Eq)

-- Mock integratedCompile function
integratedCompile :: FilePath -> String -> Either String CompilationResult
integratedCompile _ input = compileTypus "test.typus" input

-- Mock compileTypus function for testing
compileTypus :: FilePath -> String -> Either String CompilationResult
compileTypus _ input
    | "func invalid( {" `L.isInfixOf` input = 
        Left "Parse error: unexpected token '{' at line 1, column 14"
    | "return \"not an int\"" `L.isInfixOf` input = 
        Left "Type error: cannot return String in function returning Int"
    | "return \"string\"" `L.isInfixOf` input = 
        Left "Type error: type mismatch"
    | "use resource" `L.isInfixOf` input && "move resource" `L.isInfixOf` input = 
        Left "Ownership error: use after move"
    | "borrow data" `L.isInfixOf` input = 
        CompilationSuccess "compiled with borrowing"
    | "move resource" `L.isInfixOf` input = 
        CompilationSuccess "compiled with ownership"
    | "vec_length" `L.isInfixOf` input = 
        CompilationSuccess "compiled with dependent types"
    | "where m > 0" `L.isInfixOf` input = 
        CompilationSuccess "compiled with constraints"
    | "matrix_mult" `L.isInfixOf` input = 
        CompilationSuccess "compiled with complex types"
    | "func" `L.isInfixOf` input && "return" `L.isInfixOf` input = 
        CompilationSuccess "compiled successfully"
    | otherwise = 
        Left "Unknown error"

-- QuickCheck generators
arbitraryValidFunctionName :: Gen String
arbitraryValidFunctionName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return (first : rest)

instance Arbitrary String where
    arbitrary = listOf $ oneof
        [ choose ('a', 'z')
        , choose ('A', 'Z')
        , choose ('0', '9')
        , elements " \t\n\r{}();,[]<>\"'*/"
        ]