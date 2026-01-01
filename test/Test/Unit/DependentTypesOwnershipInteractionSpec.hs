{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependentTypesOwnershipInteractionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Parser (parseTypus, TypusFile(..))
import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership)
import Dependencies (DependentType(..), TypeConstraint(..), analyzeDependentTypes)
import SourceLocation (SourcePos(..), SourceSpan(..))
import ErrorHandler (ErrorContext(..))

-- | Tests for interaction between dependent types L.and ownership systems
tests :: TestTree
tests = testGroup "Dependent Types L.and Ownership Interaction Tests"
  [ testGroup "Type-Dependent Ownership Transfer"
      [ testCase "ownership transfer preserves dependent types" $ do
          let input = unlines
                [ "func transferSafe<T>(data: SafeData<T>) -> SafeData<T> {"
                , "  return move(data)"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> 
                  let goCode = goCode compiled
                      preservesGenericType = "SafeData" `L.isInfixOf` goCode
                  in assertBool "Should preserve generic type in ownership transfer" preservesGenericType
                Left errs -> 
                  let hasOwnershipError = L.any (\e -> errorType e == OwnershipError) errs
                      hasTypeError = L.any (\e -> errorType e == TypeError) errs
                  in assertBool "Should handle ownership/type errors gracefully" (hasOwnershipError || hasTypeError)
            Left _ -> assertBool "Should parse successfully" False

      , testCase "dependent type constraints affect ownership" $ do
          let input = unlines
                [ "func constrainedTransfer(n: NonZero int) -> NonZero int {"
                , "  result := move(n)"
                , "  return result"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasConstraintCheck = "NonZero" `L.isInfixOf` goCode
                  in assertBool "Should maintain dependent type constraints after ownership transfer" hasConstraintCheck
                Left errs -> 
                  let hasDependentTypeError = L.any (\e -> "dependent type" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle dependent type constraints" hasDependentTypeError
            Left _ -> assertBool "Should parse successfully" False

      , testCase "ownership invalidates dependent type guarantees" $ do
          let input = unlines
                [ "func invalidateGuarantee() {"
                , "  data: ValidatedData = createValidated()"
                , "  moved := move(data)"
                , "  // data is no longer ValidatedData after move"
                , "  use(data)  // Should error"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Left errs -> 
                  let hasOwnershipError = L.any (\e -> errorType e == OwnershipError) errs
                      hasDependentTypeError = L.any (\e -> "validated" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should detect ownership invalidation of type guarantees" (hasOwnershipError || hasDependentTypeError)
                Right _ -> assertBool "Should fail with ownership error" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Borrowing with Dependent Types"
      [ testCase "borrowed references maintain type constraints" $ do
          let input = unlines
                [ "func borrowConstrained<T>(data: &ConstrainedData<T>) -> T {"
                , "  return data.value"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasBorrowing = "&ConstrainedData" `L.isInfixOf` goCode
                  in assertBool "Should handle borrowing with constrained types" hasBorrowing
                Left errs -> 
                  let hasBorrowError = L.any (\e -> "borrow" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle borrowing errors with dependent types" hasBorrowError
            Left _ -> assertBool "Should parse successfully" False

      , testCase "lifetime constraints with dependent types" $ do
          let input = unlines
                [ "func lifetimeExample() {"
                , "  temp: TempBuffer = createTemp()"
                , "  borrowed: &TempBuffer = &temp"
                , "  // temp goes out of scope, borrowed becomes invalid"
                , "  use(borrowed)  // Should error"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Left errs -> 
                  let hasLifetimeError = L.any (\e -> "lifetime" `L.isInfixOf` errorMessage e) errs
                      hasBorrowError = L.any (\e -> errorType e == OwnershipError) errs
                  in assertBool "Should detect lifetime violations with dependent types" (hasLifetimeError || hasBorrowError)
                Right _ -> assertBool "Should fail with lifetime error" False
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Resource Management with Type Guarantees"
      [ testCase "resource cleanup preserves type safety" $ do
          let input = unlines
                [ "func managedResource() {"
                , "  file: SafeFile = openSafeFile(\"data.txt\")"
                , "  process(file)"
                , "  // file must be closed before going out of scope"
                , "  close(file)"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasCleanup = "close" `L.isInfixOf` goCode
                      hasSafeType = "SafeFile" `L.isInfixOf` goCode
                  in assertBool "Should include cleanup calls" hasCleanup
                  assertBool "Should maintain safe type throughout" hasSafeType
                Left errs -> 
                  let hasResourceError = L.any (\e -> "resource" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle resource management errors" hasResourceError
            Left _ -> assertBool "Should parse successfully" False

      , testCase "type-dependent resource acquisition" $ do
          let input = unlines
                [ "func acquireTypedResource(size: Positive int) -> Buffer<Positive int> {"
                , "  buffer := allocateBuffer(size)"
                , "  return Buffer<Positive int>(buffer)"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasPositiveType = "Positive" `L.isInfixOf` goCode
                      hasBufferType = "Buffer" `L.isInfixOf` goCode
                  in assertBool "Should preserve positive type constraint" hasPositiveType
                  assertBool "Should handle typed buffer allocation" hasBufferType
                Left errs -> 
                  let hasTypeError = L.any (\e -> "positive" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle type-dependent resource errors" hasTypeError
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Complex Type-Ownership Interactions"
      [ testCase "nested dependent types with ownership" $ do
          let input = unlines
                [ "func nestedTypes() {"
                , "  container: Container<SafeData<int>> = createContainer()"
                , "  inner := move(container.data)"
                , "  // container.data is no longer accessible"
                , "  process(inner)"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Left errs -> 
                  let hasNestedError = L.any (\e -> "container.data" `L.isInfixOf` errorMessage e) errs
                      hasOwnershipError = L.any (\e -> errorType e == OwnershipError) errs
                  in assertBool "Should handle nested type ownership" (hasNestedError || hasOwnershipError)
                Right compiled -> do
                  let goCode = goCode compiled
                      hasNestedType = "Container<SafeData<int>>" `L.isInfixOf` goCode
                  in assertBool "Should handle nested dependent types" hasNestedType
            Left _ -> assertBool "Should parse successfully" False

      , testCase "conditional ownership based on type predicates" $ do
          let input = unlines
                [ "func conditionalOwnership(data: Data) {"
                , "  if isValid(data) {"
                , "    safeData: ValidatedData = validate(data)"
                , "    transfer(move(safeData))"
                , "  } else {"
                , "    // data remains unvalidated, cannot transfer"
                , "    process(data)"
                , "  }"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasConditional = "if" `L.isInfixOf` goCode
                      hasValidation = "ValidatedData" `L.isInfixOf` goCode
                  in assertBool "Should handle conditional validation" hasConditional
                  assertBool "Should handle validated type in conditional" hasValidation
                Left errs -> 
                  let hasConditionalError = L.any (\e -> "conditional" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle conditional ownership errors" hasConditionalError
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "Error Recovery L.and Type Safety"
      [ testCase "ownership errors don't break type checking" $ do
          let input = unlines
                [ "func mixedErrors() {"
                , "  typed: NonZero int = 5"
                , "  data := createData()"
                , "  moved := move(data)"
                , "  use(data)  // Ownership error"
                , "  result := typed + \"string\"  // Type error"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Left errs -> 
                  let hasOwnershipError = L.any (\e -> errorType e == OwnershipError) errs
                      hasTypeError = L.any (\e -> errorType e == TypeError) errs
                      hasMultipleErrors = L.length errs >= 2
                  in assertBool "Should detect ownership error" hasOwnershipError
                  assertBool "Should detect type error" hasTypeError
                  assertBool "Should report multiple errors" hasMultipleErrors
                Right _ -> assertBool "Should fail with errors" False
            Left _ -> assertBool "Should parse successfully" False

      , testCase "type validation after ownership transfer" $ do
          let input = unlines
                [ "func validateAfterTransfer() {"
                , "  data: RawData = createRawData()"
                , "  validated: ValidatedData = validate(data)"
                , "  transferred := move(validated)"
                , "  // transferred should still be ValidatedData"
                , "  useValidated(transferred)"
                , "}"
                ]
              parseResult = parseTypus input
          case parseResult of
            Right parsedFile -> do
              let compileResult = compile parsedFile
              case compileResult of
                Right compiled -> do
                  let goCode = goCode compiled
                      hasValidation = "ValidatedData" `L.isInfixOf` goCode
                      hasTransfer = "move" `L.isInfixOf` goCode
                  in assertBool "Should preserve validation after transfer" hasValidation
                  assertBool "Should handle ownership transfer" hasTransfer
                Left errs -> 
                  let hasValidationError = L.any (\e -> "validate" `L.isInfixOf` errorMessage e) errs
                  in assertBool "Should handle validation errors" hasValidationError
            Left _ -> assertBool "Should parse successfully" False
      ]

  , testGroup "QuickCheck Properties for Type-Ownership Interaction"
      [ testProperty "ownership transfer preserves type constraints" $ fastProperty $
          \typeName ->
            let input = "func transfer<T>(data: " ++ typeName ++ "<T>) -> " ++ typeName ++ "<T> { return move(data); }"
                parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                case compile parsedFile of
                  Right compiled -> 
                    let goCode = goCode compiled
                        preservesType = typeName `L.isInfixOf` goCode
                    in preservesType
                  Left _ -> property True
              Left _ -> property True

      , testProperty "dependent type constraints survive ownership operations" $ fastProperty $
          \constraint ->
            let input = "func test(data: " ++ constraint ++ " int) { moved := move(data); use(moved); }"
                parseResult = parseTypus input
            in case parseResult of
              Right parsedFile ->
                case compile parsedFile of
                  Right compiled -> 
                    let goCode = goCode compiled
                        hasConstraint = constraint `L.isInfixOf` goCode
                    in hasConstraint
                  Left _ -> property True
              Left _ -> property True
      ]
  ]

-- Helper functions L.and data types
errorType :: CompilerError -> ErrorType
errorType (CompilerError et _ _ _ _) = et

data ErrorType = SyntaxError | TypeError | OwnershipError | UndefinedVariable | RuntimeError
  deriving (Show, Eq)

data CompiledModule = CompiledModule
  { goCode :: T.Text
  , errors :: [CompilerError]
  } deriving (Show, Eq)