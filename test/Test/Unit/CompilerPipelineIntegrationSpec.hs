module Test.Unit.CompilerPipelineIntegrationSpec where


import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), (==>), forAll, counterexample, classify)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List 
import Compiler (compile, CompilerResult, CompilerError(..), CompilationPhase)
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement)
import Compiler.GoAst (GoModule(..), renderGoModule)
import ErrorHandler 
import SourceLocation (SourcePos(..), SourceSpan)
                      let simpleInput = "func add(a: int, b: int) -> int { return a + b; }"
                                            parseResult = parseTypus simpleInput
          case parseResult of
            Left parseErr -> assertBool "Should parse successfully" False
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Left compileErr -> assertBool $ "Should compile successfully: " ++ show compileErr
                Right compiled -> do
                              assertBool "Should have IR module" (isJust $ irModule compiled)
                  assertBool "Should have generated Go code" (not $ T.L.null $ goCode compiled)

        ,             testCase "dependent types compilation pipeline" $ do
                      let dependentTypesInput = unlines
                [ "func safeDivide(numerator: int, denominator: NonZero int) -> int {"
                , "  return numerator / denominator"
                , "}"
                ]
                                            parseResult = parseTypus dependentTypesInput
          case parseResult of
            Left parseErr -> assertBool "Should parse successfully" False
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Left compileErr -> 
                  let hasDependentTypeError = L.any (\e -> "dependent type" `L.isInfixOf` errorMessage e) compileErr
                  in assertBool "Should handle dependent type errors gracefully" hasDependentTypeError
                Right compiled -> do
                              assertBool "Should compile dependent types" (isJust $ irModule compiled)

        ,             testCase "ownership compilation pipeline" $ do
                      let ownershipInput = unlines
                [ "func transferOwnership() {"
                , "  data := createData()"
                , "  receiver := move(data)"
                , "  return receiver"
                , "}"
                ]
                                            parseResult = parseTypus ownershipInput
          case parseResult of
            Left parseErr -> assertBool "Should parse successfully" False
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Left compileErr -> 
                  let hasOwnershipError = L.any (\e -> errorType                               e == OwnershipError) compileErr
                  in assertBool "Should detect ownership errors" hasOwnershipError
                Right compiled -> do
                              assertBool "Should compile ownership code" (isJust $ irModule compiled)
      ]

  , testGroup "IR Generation L.and Optimization"
      [             testCase "IR generation preserves semantics" $ do
                      let semanticInput = unlines
                [ "func calculate(x: int, y: int) -> int {"
                , "  temp := x * 2"
                , "  result := temp + y"
                , "  return result"
                , "}"
                ]
                                            parseResult = parseTypus semanticInput
          case parseResult of
            Right parsedFile -> do
                          let irResult = compileToIntegratedIR parsedFile
              case irResult of
                Left irErr -> assertBool $ "Should generate IR: " ++ show irErr
                Right irModule -> do
                              let functions = irFunctions irModule
                  assertBool "Should have at least one function" (not $ null functions)
                  let mainFunc = L.head functions
                  assertBool "Should have IR statements" (not $ L.null $ irStmts mainFunc)
                Left _ -> assertBool "Should parse successfully" False

        ,             testCase "IR optimization improves code" $ do
                      let optimizableInput = unlines
                [ "func redundant() {"
                , "  x := 5"
                , "  y := x"
                , "  z := y"
                , "  return z"
                , "}"
                ]
                                            parseResult = parseTypus optimizableInput
          case parseResult of
            Right parsedFile -> do
                          let irResult = compileToIntegratedIR parsedFile
              case irResult of
                Right originalIR -> do
                              let optimizedIR = optimizeIR originalIR
                                                    originalStmts = L.sum $ L.map (L.length . irStmts) $ irFunctions originalIR
                                                    optimizedStmts = L.sum $ L.map (L.length . irStmts) $ irFunctions optimizedIR
                  assertBool "Optimization should not increase statement count" (optimizedStmts <= originalStmts)
                Left _ -> assertBool "Should parse successfully" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Code Generation Pipeline"
      [             testCase "Go code generation preserves functionality" $ do
                      let codeGenInput = unlines
                [ "func greet(name: string) -> string {"
                , "  message := \"Hello, \" + name"
                , "  return message"
                , "}"
                ]
                                            parseResult = parseTypus codeGenInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                              let goCode = goCode compiled
                                                    hasFuncDecl = "func greet" `L.isInfixOf` goCode
                                                    hasReturn = "return" `L.isInfixOf` goCode
                                                    hasStringConcat = "+" `L.isInfixOf` goCode
                  assertBool "Should contain function declaration" hasFuncDecl
                  assertBool "Should contain return statement" hasReturn
                  assertBool "Should contain string concatenation" hasStringConcat
                Left _ -> assertBool "Should compile successfully" False
            Left _ -> assertBool "Should parse successfully" False

        ,             testCase "complex type mapping to Go" $ do
                      let complexTypeInput = unlines
                [ "func complexTypes() {"
                , "  numbers: [int] = [1, 2, 3]"
                , "  mapping: map[string]int = {\"key\": 42}"
                , "  pair: (int, string) = (42, \"answer\")"
                , "}"
                ]
                                            parseResult = parseTypus complexTypeInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                              let goCode = goCode compiled
                                                    hasSlice = "[]int" `L.isInfixOf` goCode
                                                    hasMap = "map[string]int" `L.isInfixOf` goCode
                  assertBool "Should handle array/slice types" hasSlice
                  assertBool "Should handle map types" hasMap
                Left _ -> assertBool "Should compile complex types" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Error Propagation Through Pipeline"
      [             testCase "parse errors propagate correctly" $ do
                      let parseErrorInput = "func broken { missing parameters }"
                                            parseResult = parseTypus parseErrorInput
          case parseResult of
            Left parseErr -> assertBool "Should fail at parse stage" True
            Right _ -> assertBool "Should not parse invalid input" False

        ,             testCase "type errors propagate through compilation" $ do
                      let typeErrorInput = unlines
                [ "func typeError() {"
                , "  text := \"hello\""
                , "  number := 42"
                , "  result := text + number  // Type error"
                , "}"
                ]
                                            parseResult = parseTypus typeErrorInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Left compileErr -> 
                  let hasTypeError = L.any (\e -> errorPhase                               e == TypeChecking) compileErr
                  in assertBool "Should fail at type checking phase" hasTypeError
                Right _ -> assertBool "Should fail with type error" False
            Left _ -> assertBool "Should parse successfully" False

        ,             testCase "ownership errors prevent code generation" $ do
                      let ownershipErrorInput = unlines
                [ "func ownershipError() {"
                , "  resource := acquireResource()"
                , "  moved := move(resource)"
                , "  use(resource)  // Error: use after move"
                , "}"
                ]
                                            parseResult = parseTypus ownershipErrorInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Left compileErr -> 
                  let hasOwnershipError = L.any (\e -> errorType                               e == OwnershipError) compileErr
                  in assertBool "Should fail with ownership error" hasOwnershipError
                Right compiled -> 
                  let goCode = goCode compiled
                  in assertBool "Should not generate Go code on ownership error" (T.null goCode)
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Pipeline Performance L.and Resource Management"
      [             testCase "pipeline handles large inputs efficiently" $ do
                      let largeInput = unlines $ replicate 100 "func test" ++ ["func main() { return 42; }"]
                                            parseResult = parseTypus largeInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> assertBool "Should handle large inputs" True
                Left _ -> assertBool "Should compile large inputs" False
            Left _ -> assertBool "Should parse large inputs" False

        ,             testCase "pipeline preserves memory safety" $ do
                      let memorySafeInput = unlines
                [ "func memoryTest() {"
                , "  data := createLargeData()"
                , "  processed := processData(data)"
                , "  cleanup(data)"
                , "  return processed"
                , "}"
                ]
                                            parseResult = parseTypus memorySafeInput
          case parseResult of
            Right parsedFile -> do
                          let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                              let goCode = goCode compiled
                                                    hasCleanup = "cleanup" `L.isInfixOf` goCode
                  assertBool "Should include cleanup calls" hasCleanup
                Left _ -> assertBool "Should compile memory-safe code" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "QuickCheck Properties for Pipeline Integration"
      [             testProperty "pipeline preserves function count" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile -> 
                case compile parsedFile of
                  Right compiled -> 
                    let inputFuncs = L.length $ L.filter ("func" `L.isPrefixOf`) $ lines input
                                                      goCode = goCode compiled
                                                      outputFuncs = L.length $ L.filter ("func" `L.isInfixOf`) $ T.lines goCode
                    in inputFuncs <= outputFuncs
                  Left _ -> property True
              Left _ -> property True

      ,             testProperty "successful compilation produces non-empty Go code" $ fastProperty $
          \input ->
            let parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                case compile parsedFile of
                  Right compiled -> not $ T.L.null $ goCode compiled
                  Left _ -> property True
              Left _ -> property True
      ]
  ]

-- Helper data types for compiler pipeline testing
data                               CompiledModule = CompiledModule
  { irModule :: Maybe IRModule
  , goCode :: T.Text
  , errors :: [CompilerError]
  } deriving (Show, Eq)

errorType :: CompilerError -> ErrorType
errorType (CompilerError et _ _ _ _) = et

data                               ErrorType = SyntaxError | TypeError | OwnershipError | UndefinedVariable | RuntimeError
  deriving (Show, Eq)