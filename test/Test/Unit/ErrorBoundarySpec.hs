module Test.Unit.ErrorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, listOf1, resize)
import qualified Data.Text as T
import Data.List (isInfixOf)

import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Parser (parseTypus, TypusFile(..))
import ErrorHandler (formatError)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- | Generate malformed code snippets for error boundary testing
genMalformedCode :: Gen String
genMalformedCode = elements
  [ "func { }"  -- Missing function name
  , "func main" -- Missing parentheses and body
  , "func main() {" -- Unclosed brace
  , "var x int =" -- Incomplete assignment
  , "return" -- Return without value in non-void function
  , "if x {" -- Missing condition parentheses
  , "for {" -- Missing loop condition
  , "package" -- Missing package name
  , "import (" -- Unclosed import block
  , "func main() { var x int; x = }" -- Invalid assignment
  , "func main() { return x }" -- Undefined variable
  , "func main() { var x string = 42 }" -- Type mismatch
  , "func main() { var x int; x = \"hello\" }" -- Type mismatch in assignment
  ]

-- | Test error handling for malformed syntax
tests :: TestTree
tests =
  testGroup "Error Boundary Tests"
    [ testGroup "Syntax Error Recovery"
        [ testCase "handles missing function name gracefully" $ do
            let malformedCode = "func { }"
            result <- compile malformedCode
            case result of
              Left errs -> do
                assertBool "should have syntax errors" $ not $ null errs
                let hasSyntaxError = any (\e -> compilationPhase e == SyntaxPhase) errs
                assertBool "should contain syntax phase error" hasSyntaxError
              Right _ -> assertFailure "expected compilation failure"

        , testCase "handles incomplete function declarations" $ do
            let incompleteCode = "func main"
            result <- compile incompleteCode
            case result of
              Left errs -> do
                assertBool "should have errors" $ not $ null errs
                let hasExpectedError = any (\e -> "expected" `T.isInfixOf` formatError e) errs
                assertBool "error message should be informative" hasExpectedError
              Right _ -> assertFailure "expected compilation failure"

        , testCase "handles unclosed braces" $ do
            let unclosedBraces = "func main() {"
            result <- compile unclosedBraces
            case result of
              Left errs -> do
                assertBool "should have parse errors" $ not $ null errs
                let hasParseError = any (\e -> compilationPhase e == SyntaxPhase) errs
                assertBool "should contain parse error" hasParseError
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Type Error Boundaries"
        [ testCase "handles undefined variables gracefully" $ do
            let undefinedVarCode = "func main() { return x }"
            result <- compile undefinedVarCode
            case result of
              Left errs -> do
                assertBool "should have type errors" $ not $ null errs
                let hasTypeError = any (\e -> compilationPhase e == TypeCheckPhase) errs
                assertBool "should contain type checking error" hasTypeError
              Right _ -> assertFailure "expected compilation failure"

        , testCase "handles type mismatches with clear messages" $ do
            let typeMismatchCode = "func main() { var x string = 42 }"
            result <- compile typeMismatchCode
            case result of
              Left errs -> do
                assertBool "should have type errors" $ not $ null errs
                let hasClearMessage = any (\e -> "type" `T.isInfixOf` formatError e) errs
                assertBool "error message should mention type" hasClearMessage
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Parser Error Recovery"
        [ testCase "continues parsing after first error" $ do
            let multiErrorCode = unlines
                  [ "func { }"
                  , "func main() {"
                  , "  var x int = \"hello\""
                  , "  return y"
                  , "}"
                  ]
            result <- compile multiErrorCode
            case result of
              Left errs -> do
                assertBool "should detect multiple errors" $ length errs >= 2
                let hasSyntaxError = any (\e -> compilationPhase e == SyntaxPhase) errs
                let hasTypeError = any (\e -> compilationPhase e == TypeCheckPhase) errs
                assertBool "should detect syntax errors" hasSyntaxError
                assertBool "should detect type errors" hasTypeError
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Ownership Error Boundaries"
        [ testCase "handles ownership violations with clear messages" $ do
            let ownershipCode = unlines
                  [ "//! ownership: on"
                  , "func main() {"
                  , "  var x = make([]int, 10)"
                  , "  var y = x"
                  , "  x[0] = 1"
                  , "  y[0] = 2"
                  , "}"
                  ]
            result <- compile ownershipCode
            case result of
              Left errs -> do
                assertBool "should have ownership errors" $ not $ null errs
                let hasOwnershipError = any (\e -> compilationPhase e == OwnershipPhase) errs
                assertBool "should contain ownership analysis error" hasOwnershipError
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "Dependent Types Error Boundaries"
        [ testCase "handles dependent type constraint violations" $ do
            let dependentTypesCode = unlines
                  [ "//! dependent_types: on"
                  , "func main() {"
                  , "  var x Vector(n:5)"
                  , "  var y = x.get(10)"  -- Out of bounds
                  , "}"
                  ]
            result <- compile dependentTypesCode
            case result of
              Left errs -> do
                assertBool "should have dependent type errors" $ not $ null errs
                let hasDependentTypeError = any (\e -> compilationPhase e == DependentTypesPhase) errs
                assertBool "should contain dependent type checking error" hasDependentTypeError
              Right _ -> assertFailure "expected compilation failure"
        ]

    , testGroup "QuickCheck Property Tests"
        [ testProperty "malformed code always produces errors" $ forAll genMalformedCode $ \code -> do
            result <- compile code
            case result of
              Left errs -> not $ null errs
              Right _ -> False

        , testProperty "error messages are non-empty" $ forAll genMalformedCode $ \code -> do
            result <- compile code
            case result of
              Left errs -> all (\e -> not $ T.null $ formatError e) errs
              Right _ -> False
        ]
    ]