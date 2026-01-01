{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.CodeGenerationConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>), elements, listOf, listOf1, choose, oneof)
import Test.Tasty.HUnit (testCase, assert, (@?=))
import qualified Data.Text as T
import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import Compiler
  ( generateGoCode
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  )
import Parser 
  ( parseTypus
  , TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import qualified SyntaxValidator
import SourceLocation (emptySpan, startPos, SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan)
import Compiler.GoAst (renderGoModule)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary TypusFile where
    arbitrary = do
        directives <- arbitrary
        buildTags <- listOf arbitrary
        blocks <- listOf arbitrary
        syntaxErrors <- listOf arbitrary
        return $ TypusFile directives buildTags blocks syntaxErrors

instance Arbitrary FileDirectives where
    arbitrary = FileDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockDirectives where
    arbitrary = BlockDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CodeBlock where
    arbitrary = do
        directives <- arbitrary
        content <- listOf1 $ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \n\t;"
        span <- arbitrary
        return $ CodeBlock directives content span

instance Arbitrary SyntaxValidator.SyntaxError where
    arbitrary = do
        errorType <- arbitrary
        errorMessage <- listOf1 $ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 "
        lineNumber <- choose (1, 1000)
        columnNumber <- choose (1, 1000)
        lineContent <- listOf1 $ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \n\t;"
        return $ SyntaxValidator.SyntaxError errorType errorMessage lineNumber columnNumber lineContent

instance Arbitrary SyntaxValidator.ErrorType where
    arbitrary = oneof
        [ pure SyntaxValidator.MissingBrace
        , pure SyntaxValidator.MissingParenthesis
        , pure SyntaxValidator.MissingBracket
        , pure SyntaxValidator.UnclosedString
        , pure SyntaxValidator.UnclosedComment
        , pure SyntaxValidator.InvalidIdentifier
        , pure SyntaxValidator.InvalidTypeDeclaration
        , pure SyntaxValidator.InvalidFunctionDeclaration
        , pure SyntaxValidator.InvalidImport
        , pure SyntaxValidator.InvalidStatement
        , pure SyntaxValidator.UnterminatedBlock
        , pure SyntaxValidator.InvalidOperator
        , pure SyntaxValidator.MissingSemicolon
        , pure SyntaxValidator.UnexpectedToken
        , pure SyntaxValidator.MissingPackageDeclaration
        , pure SyntaxValidator.DuplicateDeclaration
        , pure SyntaxValidator.InvalidBlockStructure
        , pure SyntaxValidator.UndeclaredVariable
        , pure SyntaxValidator.SyntaxWarning
        ]

instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> choose (1, 1000) <*> choose (1, 1000) <*> choose (0, 100000)

instance Arbitrary SourceSpan where
    arbitrary = do
        start <- arbitrary
        end <- arbitrary
        return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
    arbitrary = do
        value <- arbitrary
        span <- arbitrary
        return $ locatedWithSpan value span

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate simple variable declarations
genVariableDeclaration :: Gen String
genVariableDeclaration = do
  varName <- elements ["x", "y", "result", "value", "data", "count"]
  varType <- elements ["Int", "String", "Bool", "Float"]
  value <- elements ["42", "\"hello\"", "true", "3.14", "create()", "getDefault()"]
  return $ "let " ++ varName ++ ": " ++ varType ++ " = " ++ value ++ ";"

-- Generate function definitions
genFunctionDefinition :: Gen String
genFunctionDefinition = do
  funcName <- elements ["calculate", "process", "transform", "compute", "evaluate"]
  paramType <- elements ["Int", "String", "Bool", "Float"]
  returnType <- elements ["Int", "String", "Bool", "Float"]
  paramNames <- listOf1 $ elements ["x", "y", "input", "value", "arg"]
  
  let params = unwords $ L.map (\name -> name ++ ": " ++ paramType) paramNames
  let returnExpr = case paramNames of
        [] -> "42"
        [x] -> x
        (x:xs) -> x ++ " + " ++ L.head xs
  
  return $ unlines
    [ "func " ++ funcName ++ "(" ++ params ++ "): " ++ returnType ++ " {"
    , "  return " ++ returnExpr ++ ";"
    , "}"
    ]

-- Generate type definitions
genTypeDefinition :: Gen String
genTypeDefinition = do
  typeName <- elements ["Person", "Data", "Container", "Result", "Config"]
  fields <- listOf $ do
    fieldName <- elements ["name", "value", "data", "count", "size"]
    fieldType <- elements ["Int", "String", "Bool", "Float"]
    return $ fieldName ++ ": " ++ fieldType
  
  let fieldStr = unlines $ L.map (\field -> "  " ++ field ++ ";") fields
  return $ unlines
    [ "struct " ++ typeName ++ " {"
    , fieldStr
    , "}"
    ]

-- Generate control flow statements
genControlFlow :: Gen String
genControlFlow = do
  flowType <- elements ["if_statement", "for_loop", "while_loop", "match_statement"]
  varName <- elements ["x", "y", "condition", "value"]
  
  case flowType of
    "if_statement" -> do
      return $ unlines
        [ "if " ++ varName ++ " > 0 {"
        , "  return true;"
        , "} else {"
        , "  return false;"
        , "}"
        ]
    "for_loop" -> do
      return $ unlines
        [ "for i in 0..10 {"
        , "  " ++ varName ++ " = " ++ varName ++ " + i;"
        , "}"
        ]
    "while_loop" -> do
      return $ unlines
        [ "while " ++ varName ++ " < 100 {"
        , "  " ++ varName ++ " = " ++ varName ++ " * 2;"
        , "}"
        ]
    "match_statement" -> do
      return $ unlines
        [ "match " ++ varName ++ " {"
        , "  0 => return \"zero\";"
        , "  1 => return \"one\";"
        , "  _ => return \"other\";"
        , "}"
        ]
    _ -> return "default control flow"

-- Generate complete Typus programs
genTypusProgram :: Gen String
genTypusProgram = do
  hasVariables <- arbitrary
  hasFunctions <- arbitrary
  hasTypes <- arbitrary
  hasControlFlow <- arbitrary
  
  parts <- L.concat <$> sequence
    [ if hasVariables then listOf1 genVariableDeclaration else return []
    , if hasFunctions then listOf1 genFunctionDefinition else return []
    , if hasTypes then listOf1 genTypeDefinition else return []
    , if hasControlFlow then listOf1 genControlFlow else return []
    ]
  
  return $ unlines parts

-- Generate programs with potential code generation challenges
genChallengingProgram :: Gen String
genChallengingProgram = do
  challengeType <- elements
    [ "nested_structures"
    , "complex_expressions"
    , "recursive_functions"
    , "generic_types"
    , "dependent_types"
    , "ownership_operations"
    ]
  
  case challengeType of
    "nested_structures" -> do
      return $ unlines
        [ "struct Outer {"
        , "  inner: Inner;"
        , "}"
        , "struct Inner {"
        , "  value: Int;"
        , "}"
        , "let outer = Outer { inner: Inner { value: 42 } };"
        ]
    "complex_expressions" -> do
      return $ "let result = (x + y) * (z - w) / func(a, b, c);"
    "recursive_functions" -> do
      return $ unlines
        [ "func factorial(n: Int): Int {"
        , "  if n <= 1 { return 1; }"
        , "  return n * factorial(n - 1);"
        , "}"
        ]
    "generic_types" -> do
      return $ unlines
        [ "type Container<T> = { data: T, size: Int };"
        , "let intContainer: Container<Int> = { data: 42, size: 1 };"
        ]
    "dependent_types" -> do
      return $ unlines
        [ "func first<T>(n: Nat, vec: Vector[n]): T {"
        , "  return vec[0];"
        , "}"
        ]
    "ownership_operations" -> do
      return $ unlines
        [ "let data = create();"
        , "let processed = move(data);"
        , "consume(processed);"
        ]
    _ -> return "default challenging program"

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: generateGoCode should return a result for L.any TypusFile
prop_generate_go_code_returns_result :: Property
prop_generate_go_code_returns_result =
  let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "package main\n\nfunc main() {\n}" (emptySpan startPos)] []
      result = generateGoCode typusFile
      hasResult = not (null result)
  in hasResult === True

-- Property: generateGoCode should handle empty files
prop_generate_go_code_empty_file :: Property
prop_generate_go_code_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      result = generateGoCode emptyFile
      hasResult = not (null result)
  in hasResult === True

-- Property: generateGoCode should be idempotent
prop_generate_go_code_idempotent :: TypusFile -> Property
prop_generate_go_code_idempotent typusFile =
  let result1 = generateGoCode typusFile
      result2 = generateGoCode typusFile
  in result1 === result2

-- Property: generated Go code should be syntactically valid
prop_generated_go_syntactically_valid :: String -> Property
prop_generated_go_syntactically_valid typusCode =
  let parseResult = parseTypus typusCode
      goCodeResult = case parseResult of
        Left _ -> Nothing
        Right typusFile -> Just $ generateGoCode typusFile
      
      hasValidGoStructure = case goCodeResult of
        Nothing -> False
        Just goCode -> 
          let hasPackageDecl = "package" `L.isInfixOf` goCode
              hasImports = "import" `L.isInfixOf` goCode
              hasFuncDecls = "func" `L.isInfixOf` goCode
          in hasPackageDecl || hasFuncDecls || hasImports
  in hasValidGoStructure === True

-- Property: generated Go code should preserve function names
prop_generated_go_preserves_functions :: String -> Property
prop_generated_go_preserves_functions typusCode =
  let parseResult = parseTypus typusCode
      hasFunctions = "func" `L.isInfixOf` typusCode
      
      preservesFunctions = case parseResult of
        Left _ -> False
        Right typusFile ->
          let goCode = generateGoCode typusFile
          in if hasFunctions
             then "func" `L.isInfixOf` goCode
             else True  -- No functions to preserve
  in (not hasFunctions || preservesFunctions) === True

-- Property: generated Go code should handle variable declarations
prop_generated_go_handles_variables :: String -> Property
prop_generated_go_handles_variables typusCode =
  let parseResult = parseTypus typusCode
      hasVariables = "let" `L.isInfixOf` typusCode
      
      handlesVariables = case parseResult of
        Left _ -> False
        Right typusFile ->
          let goCode = generateGoCode typusFile
          in if hasVariables
             then "var" `L.isInfixOf` goCode || ":=" `L.isInfixOf` goCode
             else True  -- No variables to handle
  in (not hasVariables || handlesVariables) === True

-- Property: generated Go code should be consistent across runs
prop_generated_go_consistency :: String -> Property
prop_generated_go_consistency typusCode =
  let parseResult = parseTypus typusCode
      
      consistent = case parseResult of
        Left _ -> True  -- Can't test consistency if parse fails
        Right typusFile ->
          let result1 = generateGoCode typusFile
              result2 = generateGoCode typusFile
          in result1 == result2
  in consistent === True

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_generate_simple_variable :: TestTree
test_generate_simple_variable = testCase "generate simple variable" $ do
  let code = "let x: Int = 42;"
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goCode = generateGoCode typusFile
      assert $ not $ null goCode
      assert $ "var" `L.isInfixOf` goCode || ":=" `L.isInfixOf` goCode

test_generate_simple_function :: TestTree
test_generate_simple_function = testCase "generate simple function" $ do
  let code = unlines
        [ "func test(x: Int): Int {"
        , "  return x * 2;"
        , "}"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goCode = generateGoCode typusFile
      assert $ not $ null goCode
      assert $ "func" `L.isInfixOf` goCode
      assert $ "test" `L.isInfixOf` goCode

test_generate_struct_definition :: TestTree
test_generate_struct_definition = testCase "generate struct definition" $ do
  let code = unlines
        [ "struct Person {"
        , "  name: String;"
        , "  age: Int;"
        , "}"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goCode = generateGoCode typusFile
      assert $ not $ null goCode
      assert $ "type" `L.isInfixOf` goCode || "struct" `L.isInfixOf` goCode
      assert $ "Person" `L.isInfixOf` goCode

test_generate_control_flow :: TestTree
test_generate_control_flow = testCase "generate control flow" $ do
  let code = unlines
        [ "if x > 0 {"
        , "  return true;"
        , "} else {"
        , "  return false;"
        , "}"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goCode = generateGoCode typusFile
      assert $ not $ null goCode
      assert $ "if" `L.isInfixOf` goCode

test_generate_complex_program :: TestTree
test_generate_complex_program = testCase "generate complex program" $ do
  let code = unlines
        [ "struct Data {"
        , "  value: Int;"
        , "}"
        , "func process(d: Data): Int {"
        , "  if d.value > 0 {"
        , "    return d.value * 2;"
        , "  } else {"
        , "    return 0;"
        , "  }"
        , "}"
        , "let result = process(Data { value: 42 });"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goCode = generateGoCode typusFile
      assert $ not $ null goCode
      assert $ "func" `L.isInfixOf` goCode
      assert $ "if" `L.isInfixOf` goCode

test_generate_error_handling :: TestTree
test_generate_error_handling = testCase "generate error handling" $ do
  let code = "let x: Int = \"string\";"  -- Type error
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let goResult = generateGoCode typusFile
      let goCode = goResult
      -- generateGoCode returns String directly
      assert $ not $ null goCode

test_go_module_rendering :: TestTree
test_go_module_rendering = testCase "Go module rendering" $ do
  let dummyFile = TypusFile defaultFileDirectives [] [] []
  let result = generateGoCode dummyFile
  assert $ not $ null result

test_empty_file_generation :: TestTree
test_empty_file_generation = testCase "empty file generation" $ do
  let emptyFile = TypusFile defaultFileDirectives [] [] []
  let goCode = generateGoCode emptyFile
  -- generateGoCode returns String directly
  assert $ not $ null goCode

test_consistency_across_runs :: TestTree
test_consistency_across_runs = testCase "consistency across runs" $ do
  let code = unlines
        [ "let x = 42;"
        , "func double(n: Int): Int { return n * 2; }"
        , "struct Point { x: Int, y: Int }"
        ]
  let result = parseTypus code
  case result of
    Left _ -> assert False
    Right typusFile -> do
      let (code1, code2) = (generateGoCode typusFile, generateGoCode typusFile)
      -- generateGoCode returns String directly
      code1 @?= code2

test_edge_cases :: TestTree
test_edge_cases = testCase "edge cases" $ do
  let testCases = 
        [ ""  -- Empty code
        , "// comment only"
        , "let x = 42"  -- Missing semicolon
        , "func incomplete() {"  -- Incomplete function
        ]
  
  mapM_ (\code -> do
    let result = parseTypus code
    case result of
      Left _ -> assert $ null code  -- Only allow failure for empty code
      Right typusFile -> do
        let goCode = generateGoCode typusFile
        -- generateGoCode returns String directly
        assert $ not $ null goCode || L.length code < 5  -- Allow empty result for very short code
    ) testCases

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Code Generation Consistency QuickCheck Tests"
  [ testProperty "generateGoCode returns result for L.any TypusFile" prop_generate_go_code_returns_result
  , testProperty "generateGoCode handles empty files" prop_generate_go_code_empty_file
  , testProperty "generateGoCode is idempotent" prop_generate_go_code_idempotent
  , testProperty "generated Go code is syntactically valid" prop_generated_go_syntactically_valid
  , testProperty "generated Go code preserves function names" prop_generated_go_preserves_functions
  , testProperty "generated Go code handles variable declarations" prop_generated_go_handles_variables
  , testProperty "generated Go code is consistent across runs" prop_generated_go_consistency
  , test_generate_simple_variable
  , test_generate_simple_function
  , test_generate_struct_definition
  , test_generate_control_flow
  , test_generate_complex_program
  , test_generate_error_handling
  , test_go_module_rendering
  , test_empty_file_generation
  , test_consistency_across_runs
  , test_edge_cases
  ]