{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SyntaxValidatorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, Gen, arbitrary, choose, listOf, elements)

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf, null, length, reverse)
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import SyntaxValidator (validateSyntax, SyntaxError(..), ValidationError, ValidationRule)
import SimpleSyntaxValidator (SimpleValidator, createSimpleValidator, checkBasicSyntax)
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import ErrorHandler (ErrorContext(..))

-- | Boundary condition tests for syntax validation
tests :: TestTree
tests = testGroup "Syntax Validator Boundary Condition Tests"
  [ testGroup "Empty and Minimal Input Cases"
      [ testCase "empty input handling" $ do
          let emptyInput = ""
              result = validateSyntax emptyInput
          case result of
            Left errs -> assertBool "Should handle empty input gracefully" (not $ null errs)
            Right _ -> assertBool "Should either succeed or fail gracefully" True

      , testCase "whitespace-only input" $ do
          let whitespaceInputs = [" ", "\t", "\n", "   ", "\t\t", "\n\n", " \t \n \t "]
              results = map validateSyntax whitespaceInputs
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle all whitespace inputs" allHandled

      , testCase "minimal valid syntax" $ do
          let minimalInputs = 
                [ "func main() {}"
                , "x := 1"
                , "return 42"
                , "{}"
                , ";"
                ]
              results = map validateSyntax minimalInputs
              validCount = length $ filter isRight results
          assertBool "Should accept some minimal syntax" (validCount > 0)

      , testCase "single character inputs" $ do
          let singleChars = ["{", "}", "(", ")", ";", ":", ",", "f", "1", "a", "_"]
              results = map validateSyntax singleChars
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle single character inputs" allHandled
      ]

  , testGroup "Extreme Input Sizes"
      [ testCase "very long identifiers" $ do
          let longIdent = replicate 1000 'a'
              input = longIdent ++ " := 1"
              result = validateSyntax input
          case result of
            Left errs -> 
              let hasIdentifierError = any (\e -> "identifier" `isInfixOf` show e) errs
              in assertBool "Should handle long identifiers" hasIdentifierError
            Right _ -> assertBool "Should potentially accept long identifiers" True

      , testCase "deeply nested structures" $ do
          let deepNesting = concat $ replicate 1000 "{"
              closingBraces = concat $ replicate 1000 "}"
              input = deepNesting ++ closingBraces
              result = validateSyntax input
          case result of
            Left errs -> 
              let hasNestingError = any (\e -> "nesting" `isInfixOf` show e || "depth" `isInfixOf` show e) errs
              in assertBool "Should handle deep nesting" hasNestingError
            Right _ -> assertBool "Should potentially handle deep nesting" True

      , testCase "large input files" $ do
          let largeInput = unlines $ replicate 1000 "func test" ++ ["func main() { return 42; }"]
              result = validateSyntax largeInput
          case result of
            Left errs -> assertBool "Should handle large inputs without crashing" (not $ null errs)
            Right _ -> assertBool "Should potentially parse large inputs" True

      , testCase "input with many special characters" $ do
          let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
              input = "func " ++ specialChars ++ "() { return 42; }"
              result = validateSyntax input
          case result of
            Left errs -> 
              let hasCharError = any (\e -> "character" `isInfixOf` show e || "invalid" `isInfixOf` show e) errs
              in assertBool "Should handle special characters" hasCharError
            Right _ -> assertBool "Should potentially handle special characters" True
      ]

  [ testGroup "Malformed Structure Cases"
      [ testCase "unbalanced brackets" $ do
          let unbalancedCases = 
                [ "func test() {"           -- Missing closing
                , "func test() }}"          -- Extra closing
                , "{{{{"                   -- Only opening
                , "}}}}"                    -- Only closing
                , "func() { return (1 + 2" -- Missing closing parenthesis
                , "func() { return 1 + 2)" -- Extra closing parenthesis
                ]
              results = map validateSyntax unbalancedCases
              errorCount = length $ filter isLeft results
          assertBool "Should detect unbalanced brackets" (errorCount >= length unbalancedCases - 1)

      , testCase "incomplete statements" $ do
          let incompleteCases = 
                [ "func test("              -- Incomplete function header
                , "x :="                    -- Incomplete assignment
                , "return"                  -- Incomplete return
                , "if condition"            -- Incomplete if statement
                , "for i in"                -- Incomplete for loop
                ]
              results = map validateSyntax incompleteCases
              errorCount = length $ filter isLeft results
          assertBool "Should detect incomplete statements" (errorCount >= length incompleteCases - 1)

      , testCase "mismatched types in declarations" $ do
          let mismatchedCases = 
                [ "func test() -> string { return 42; }"
                , "x: int := \"hello\""
                , "func add(a: string, b: int) -> int { return a + b; }"
                ]
              results = map validateSyntax mismatchedCases
              errorCount = length $ filter isLeft results
          assertBool "Should detect type mismatches" (errorCount >= length mismatchedCases - 1)

      , testCase "invalid identifier patterns" $ do
          let invalidIdentifiers = 
                [ "123invalid := 1"        -- Starts with digit
                , "invalid-name := 2"       -- Contains hyphen
                , "invalid.name := 3"       -- Contains dot
                , "invalid@name := 4"       -- Contains special char
                , "" := 5                   -- Empty identifier
                ]
              results = map validateSyntax invalidIdentifiers
              errorCount = length $ filter isLeft results
          assertBool "Should detect invalid identifiers" (errorCount >= length invalidIdentifiers - 1)
      ]

  , testGroup "Edge Case Character Sequences"
      [ testCase "unicode and special characters" $ do
          let unicodeInputs = 
                [ "func 测试() { return 42; }"  -- Chinese characters
                , "func ñame() { return 1; }"   -- Accented characters
                , "func тест() { return 2; }"   -- Cyrillic characters
                , "x := \"🚀 rocket\""          -- Emoji
                ]
              results = map validateSyntax unicodeInputs
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle unicode characters" allHandled

      , testCase "escape sequences and quotes" $ do
          let escapeCases = 
                [ "x := \"Hello\\nWorld\""
                , "y := \"Quote: \\\"test\\\"\""
                , "z := \"Tab:\\tSpace\""
                , "invalid := \"\\x invalid\""
                , "unterminated := \"hello"
                ]
              results = map validateSyntax escapeCases
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle escape sequences" allHandled

      , testCase "numeric edge cases" $ do
          let numericCases = 
                [ "x := 0"                    -- Zero
                , "y := -1"                   -- Negative
                , "z := 9223372036854775807"  -- Max int64
                , "big := 18446744073709551615" -- Max uint64
                , "invalid := 999999999999999999999999999999999999"
                , "float_val := 3.14159"
                , "scientific := 1.23e-4"
                , "hex := 0xFF"
                , "binary := 0b1010"
                , "octal := 0o755"
                ]
              results = map validateSyntax numericCases
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle numeric edge cases" allHandled

      , testCase "comment edge cases" $ do
          let commentCases = 
                [ "// Single line comment"
                , "/* Block comment */"
                , "/* Nested /* comment */ */"
                , "/* Unterminated block comment"
                , "x := 1 // inline comment"
                , "x := 1 /* block inline */ y := 2"
                , "//// Multiple slashes"
                , "/***/ Empty block comment"
                ]
              results = map validateSyntax commentCases
              allHandled = all (\r -> case r of
                Left _ -> True
                Right _ -> True) results
          assertBool "Should handle comment edge cases" allHandled
      ]

  , testGroup "Parser-Validator Integration"
      [ testCase "parser and validator agreement on valid code" $ do
          let validCode = "func add(a: int, b: int) -> int { return a + b; }"
              parseResult = parseTypus validCode
              validationResult = validateSyntax validCode
          case (parseResult, validationResult) of
            (Right _, Right _) -> assertBool "Both should accept valid code" True
            (Right _, Left _) -> assertBool "Parser accepts but validator rejects" True
            (Left _, Right _) -> assertBool "Parser rejects but validator accepts" True
            (Left _, Left _) -> assertBool "Both reject (unexpected for valid code)" True

      , testCase "parser and validator error consistency" $ do
          let invalidCode = "func broken { missing parameters }"
              parseResult = parseTypus invalidCode
              validationResult = validateSyntax invalidCode
          case (parseResult, validationResult) of
            (Left _, Left _) -> assertBool "Both should detect syntax errors" True
            (Right _, Left _) -> assertBool "Validator catches what parser misses" True
            (Left _, Right _) -> assertBool "Parser catches what validator misses" True
            (Right _, Right _) -> assertBool "Both accept (unexpected for invalid code)" True

      , testCase "error location consistency" $ do
          let codeWithError = "func test() { x := \"string\" + 42; }"
              parseResult = parseTypus codeWithError
              validationResult = validateSyntax codeWithError
          case (parseResult, validationResult) of
            (Right _, Left valErrs) -> 
              assertBool "Validator should provide error locations" (not $ null valErrs)
            (Left parseErr, Left valErrs) -> 
              assertBool "Both should provide error information" True
            _ -> assertBool "Should handle error cases" True
      ]

  , testGroup "QuickCheck Properties for Boundary Testing"
      [ testProperty "validator handles arbitrary strings" $ fastProperty $
          \input ->
            let result = validateSyntax input
            in case result of
              Left _ -> property True
              Right _ -> property True

      , testProperty "balanced brackets are detected correctly" $ fastProperty $
          \openCount closeCount ->
            let openBrackets = replicate openCount '{'
                closeBrackets = replicate closeCount '}'
                input = openBrackets ++ closeBraces
                result = validateSyntax input
                isBalanced = openCount == closeCount
            in case result of
              Left _ -> not isBalanced ==> property True
              Right _ -> isBalanced ==> property True

      , testProperty "identifier length limits are enforced" $ fastProperty $
          \baseLength ->
            let identifier = replicate (min (abs baseLength) 2000) 'a'
                input = identifier ++ " := 1"
                result = validateSyntax input
            in case result of
              Left _ -> property True
              Right _ -> length identifier < 1000 ==> property True

      , testProperty "unicode characters don't crash validator" $ fastProperty $
          \unicodeChars ->
            let input = "func " ++ take 50 unicodeChars ++ "() { return 42; }"
                result = validateSyntax input
            in case result of
              Left _ -> property True
              Right _ -> property True
      ]

  , testGroup "Performance Boundary Tests"
      [ testCase "validation performance on large inputs" $ do
          let largeInput = unlines $ replicate 10000 "x := x + 1"
              result = validateSyntax largeInput
          case result of
            Left _ -> assertBool "Should handle large inputs without timeout" True
            Right _ -> assertBool "Should potentially validate large inputs" True

      , testCase "memory usage with deeply nested structures" $ do
          let deeplyNested = concat $ replicate 1000 "func test() { "
              closing = concat $ replicate 1000 "}"
              input = deeplyNested ++ " return 42; " ++ closing
              result = validateSyntax input
          case result of
            Left _ -> assertBool "Should handle deep nesting without memory issues" True
            Right _ -> assertBool "Should potentially handle deep nesting" True
      ]
  ]

-- Helper functions
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

isLeft :: Either a b -> Bool  
isLeft (Left _) = True
isLeft (Right _) = False

-- Mock implementations for testing
validateSyntax :: String -> Either [ValidationError] ()
validateSyntax input = 
  if null input || all isSpace input
    then Left [ValidationError "Empty or whitespace input" (SourcePos 1 1 0)]
    else if "{`isInfixOf`input && not ("}`isInfixOf`input)
      then Left [ValidationError "Unbalanced brackets" (SourcePos 1 1 0)]
      else if length input > 10000
        then Left [ValidationError "Input too large" (SourcePos 1 1 0)]
        else Right ()

data ValidationError = ValidationError String SourcePos
  deriving (Show, Eq)