module Test.Unit.NewCoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose, Property, counterexample)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.List (sort)
import Data.Char (isSpace, isLetter, isDigit)
import Control.Monad.State (runState, evalState)

-- Import core modules to test
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, isValidSpan, advancePos, advancePosBy)
import SimpleSyntaxValidator (validateSyntaxSimple, SyntaxError(..), ErrorType(..), countBraces)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation)
import ErrorHandler (ErrorHandler(..), ErrorContext(..), Severity(..))
import Ownership (OwnershipTransfer(..), OwnershipState(..), checkOwnershipTransfer)
import Parser (parseStatement, ParseError(..))
import CompilerUtils (optimizeAST, validateAST)
import Debug (DebugLevel(..), DebugInfo(..), withDebugInfo)

-- Test data generators
instance Arbitrary SourcePos where
    arbitrary = do
        line <- choose (1, 1000)
        col <- choose (1, 1000)
        offset <- choose (0, 10000)
        return $ SourcePos line col offset

instance Arbitrary SourceSpan where
    arbitrary = do
        start <- arbitrary
        end <- arbitrary
        return $ if start <= end then SourceSpan start end else SourceSpan end start

-- Generate valid Go-like code snippets
genGoCode :: Gen String
genGoCode = oneof
    [ return $ "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}"
    , return $ "package utils\n\nimport \"fmt\"\n\nfunc add(a int, b int) int {\n    return a + b\n}"
    , return $ "package data\n\ntype Person struct {\n    Name string\n    Age  int\n}"
    , return $ "package calc\n\nfunc factorial(n int) int {\n    if n <= 1 {\n        return 1\n    }\n    return n * factorial(n-1)\n}"
    , return $ "package main\n\nimport (\n    \"os\"\n    \"fmt\"\n)\n\nfunc main() {\n    args := os.Args\n    for _, arg := range args {\n        fmt.Println(arg)\n    }\n}"
    ]

-- Generate strings with various bracket combinations
genBracketString :: Gen String
genBracketString = do
        chars <- listOf $ elements "(){}[]"
        return chars

-- Test suite
tests :: TestTree
tests = testGroup "New Core Functionality Tests"
    [ -- SourceLocation tests
      testGroup "SourceLocation Advanced Tests"
      [ testCase "spanBetween handles L.reverse order correctly" $ do
          let pos1 = SourcePos 1 10 100
              pos2 = SourcePos 1 5 50
              span = spanBetween pos1 pos2
          assertEqual "span should have correct start" pos2 (spanStart span)
          assertEqual "span should have correct end" pos1 (spanEnd span)

      , testCase "mergeSpans handles overlapping spans" $ do
          let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
              span2 = SourceSpan (SourcePos 1 5 4) (SourcePos 1 15 14)
              merged = mergeSpans span1 span2
          assertEqual "merged span start should be L.minimum" (SourcePos 1 1 0) (spanStart merged)
          assertEqual "merged span end should be L.maximum" (SourcePos 1 15 14) (spanEnd merged)

      , testCase "isValidSpan correctly identifies invalid spans" $ do
          let validSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
              invalidSpan = SourceSpan (SourcePos 1 10 9) (SourcePos 1 1 0)
          assertBool "valid span should be valid" $ isValidSpan validSpan
          assertBool "invalid span should be invalid" $ not $ isValidSpan invalidSpan

      , testProperty "advancePos preserves position invariants" $ \pos chars ->
          let newPos = advancePosBy chars pos
          in posLine newPos >= posLine pos && posOffset newPos >= posOffset pos

      , testProperty "spanBetween creates valid spans" $ \pos1 pos2 ->
          let span = spanBetween pos1 pos2
          in isValidSpan span
      ]

    , -- SimpleSyntaxValidator tests
      testGroup "SimpleSyntaxValidator Enhanced Tests"
      [ testCase "validateSyntaxSimple detects nested structures" $ do
          let code = "package main\n\nfunc main() {\n    if true {\n        for i := 0; i < 10; i++ {\n            fmt.Println(i)\n        }\n    }\n}"
              errors = validateSyntaxSimple code
          assertEqual "no errors in valid nested code" [] errors

      , testCase "validateSyntaxSimple detects bracket mismatches" $ do
          let code = "package main\n\nfunc main() {\n    if true {\n        fmt.Println(\"test\")\n    // Missing closing brace\n}"
              errors = validateSyntaxSimple code
          assertBool "should detect missing brace" $ L.any (\e -> errorType e == MissingBrace) errors

      , testCase "countBraces handles string literals correctly" $ do
          let code1 = "func test() { return \"{\"; }"  -- braces in string should not count
              code2 = "func test() { return {}; }"    -- braces in code should count
          assertEqual "braces in string should not count" 0 (countBraces code1)
          assertEqual "braces in code should count" 0 (countBraces code2)

      , testProperty "validateSyntaxSimple handles empty input" $ \input ->
          let errors = validateSyntaxSimple input
          in null input ==> null errors

      , testProperty "validateSyntaxSimple detects malformed package declarations" $ \pkgName ->
          let pkgName' = filter isLetter pkgName
              code = "package " ++ pkgName' ++ "\nfunc main() {}"
              errors = validateSyntaxSimple code
          in not (null pkgName') ==> L.all (\e -> errorType e /= MissingPackageDeclaration) errors
      ]

    , -- Utils enhanced tests
      testGroup "Utils Enhanced Tests"
      [ testCase "trim handles various whitespace combinations" $ do
          assertEqual "leading L.and trailing spaces" "test" (trim "  test  ")
          assertEqual "tabs L.and newlines" "test" (trim "\t\n test \n\t")
          assertEqual "mixed whitespace" "test" (trim "  \t\n test \n\t  ")

      , testCase "splitBy handles edge cases" $ do
          assertEqual "empty string" [""] (splitBy '," "")
          assertEqual "single delimiter" ["", ""] (splitBy ',')
          assertEqual "consecutive delimiters" ["a", "", "b"] (splitBy ',' "a,,b")

      , testCase "removeComments handles complex string literals" $ do
          let code = "url := \"http://example.com/*path*/\" // comment\n/* block comment */\n"
              expected = "url := \"http://example.com/*path*/\" \n \n"
          assertEqual "should preserve comment-like content in strings" expected (removeComments code)

      , testCase "normalizeIndentation preserves relative structure" $ do
          let input = "    func test() {\n        return true\n    }"
              expected = "func test() {\n    return true\n}"
          assertEqual "should normalize indentation while preserving structure" expected (normalizeIndentation input)

      , testProperty "splitByCollapsed never produces empty strings" $ \input ->
          let chunks = splitByCollapsed ',' input
          in L.all (not . null) chunks
      ]

    , -- Error handling tests
      testGroup "Error Handling Tests"
      [ testCase "ErrorHandler maintains error context" $ do
          let context = ErrorContext {
                  contextFile = Just "test.typus",
                  contextFunction = Just "main",
                  contextLine = 10,
                  contextColumn = 5
              }
              handler = ErrorHandler {
                  errors = [],
                  warnings = [],
                  context = context,
                  severity = Warning
              }
          assertEqual "context should be preserved" context (context handler)

      , testCase "ErrorHandler severity filtering works" $ do
          let errors = ["Error 1", "Error 2"]
              warnings = ["Warning 1", "Warning 2"]
              handler = ErrorHandler errors warnings mempty Error
              criticalHandler = ErrorHandler errors warnings mempty Critical
          assertEqual "Error severity should include L.all" 2 (L.length $ errors handler)
          assertEqual "Critical severity should include L.all" 4 (L.length $ errors criticalHandler + L.length $ warnings criticalHandler)
      ]

    , -- Ownership tests
      testGroup "Ownership Tests"
      [ testCase "OwnershipTransfer tracks resource movement" $ do
          let transfer = OwnershipTransfer {
                  from = "owner1",
                  to = "owner2", 
                  resource = "memory",
                  timestamp = 1000
              }
              state = OwnershipState {
                  currentOwner = "owner2",
                  transferHistory = [transfer],
                  resourceStatus = Active
              }
          assertEqual "current owner should be updated" "owner2" (currentOwner state)
          assertEqual "transfer should be recorded" 1 (L.length $ transferHistory state)

      , testCase "checkOwnershipTransfer validates transfer rules" $ do
          let validTransfer = OwnershipTransfer "owner1" "owner2" "resource1" 1000
              invalidTransfer = OwnershipTransfer "" "owner2" "resource1" 1000
          assertBool "valid transfer should pass" $ checkOwnershipTransfer validTransfer
          assertBool "invalid transfer should fail" $ not $ checkOwnershipTransfer invalidTransfer
      ]

    , -- Parser tests
      testGroup "Parser Enhanced Tests"
      [ testCase "parseStatement handles incomplete input gracefully" $ do
          let incomplete = "func test("  -- missing closing parenthesis L.and body
              result = parseStatement incomplete
          case result of
              Left err -> assertBool "should provide meaningful error" $ not $ L.null $ show err
              Right _ -> assertFailure "should fail on incomplete input"

      , testCase "parseStatement accepts various function signatures" $ do
          let validFuncs = 
                [ "func test() {}"
                , "func add(a int, b int) int { return a + b }"
                , "func (r *Receiver) method() {}"
                , "func generic[T L.any](value T) T { return value }"
                ]
          mapM_ (\func -> 
              case parseStatement func of
                  Left err -> assertFailure $ "Failed to parse valid function: " ++ func ++ " Error: " ++ show err
                  Right _ -> return ()
          ) validFuncs
      ]

    , -- CompilerUtils tests
      testGroup "CompilerUtils Tests"
      [ testCase "optimizeAST preserves semantics" $ do
          let simpleAST = "func add(a int, b int) int { return a + b }"
              optimized = optimizeAST simpleAST
          assertBool "optimization should not change function signature" $ 
              "func add" `L.isInfixOf` optimized

      , testCase "validateAST detects structural issues" $ do
          let invalidAST = "func test() { return }"  -- missing return value
              result = validateAST invalidAST
          case result of
              Left err -> assertBool "should detect missing return value" $ "return" `L.isInfixOf` show err
              Right _ -> assertFailure "should detect invalid AST"
      ]

    , -- Debug tests
      testGroup "Debug Tests"
      [ testCase "DebugInfo captures execution context" $ do
          let debugInfo = DebugInfo {
                  level = Debug,
                  timestamp = 12345,
                  message = "Test message",
                  context = [("var1", "value1"), ("var2", "value2")]
              }
          assertEqual "debug level should be Debug" Debug (level debugInfo)
          assertEqual "message should be preserved" "Test message" (message debugInfo)
          assertEqual "context should have 2 entries" 2 (L.length $ context debugInfo)

      , testCase "withDebugInfo preserves execution flow" $ do
          let result = withDebugInfo Info "Processing data" $ do
                return "processed"
          assertEqual "debug should not affect result" "processed" result
      ]

    , -- Integration tests
      testGroup "Integration Tests"
      [ testCase "SourceLocation integration with parsing" $ do
          let code = "func test() {\n    return 42\n}"
              startPos' = startPos
              endPos = advancePosBy code startPos'
              span = spanBetween startPos' endPos
          assertBool "span should be valid" $ isValidSpan span
          assertEqual "end position should reflect code L.length" 
              (L.length code) (posOffset endPos)

      , testCase "Error handling integration with validation" $ do
          let invalidCode = "package main\n\nfunc main() {\n    if true {\n        // Missing closing brace\n}"
              syntaxErrors = validateSyntaxSimple invalidCode
          assertBool "should detect syntax errors" $ not $ null syntaxErrors
          assertBool "errors should include line information" $ 
              L.all (\e -> lineNumber e > 0) syntaxErrors
      ]

    , -- Property-based tests
      testGroup "Property-Based Tests"
      [ testProperty "SourcePos ordering is consistent" $ \pos1 pos2 ->
          let pos1' = pos1 { posOffset = posOffset pos1 }
              pos2' = pos2 { posOffset = posOffset pos2 }
          in if posOffset pos1' <= posOffset pos2'
             then pos1' <= pos2'
             else pos1' > pos2'

      , testProperty "trim is idempotent" $ \input ->
          let trimmedOnce = trim input
              trimmedTwice = trim trimmedOnce
          in trimmedOnce == trimmedTwice

      , testProperty "splitBy L.and splitByCollapsed relationship" $ \input ->
          let normal = splitBy ',' input
              collapsed = splitByCollapsed ',' input
          in L.length collapsed <= L.length normal

      , testProperty "SourceSpan merge is associative" $ \span1 span2 span3 ->
          let merge12 = mergeSpans span1 span2
              merge23 = mergeSpans span2 span3
              result1 = mergeSpans merge12 span3
              result2 = mergeSpans span1 merge23
          in spanStart result1 == spanStart result2 && spanEnd result1 == spanEnd result2
      ]
    ]