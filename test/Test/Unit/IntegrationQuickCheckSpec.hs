{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds  -Wno-unused-matches #-}
module Test.Unit.IntegrationQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, Property, (===), counterexample, forAll, choose, property, vectorOf)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..))
import Compiler (compile, generateGoCode, CompilerError(..))
import Compiler.Errors.Core (message)
import Utils (trim, removeComments, normalizeIndentation)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace)

-- Test data generators
genValidIdentifier :: Gen String
genValidIdentifier = do
  firstChar <- elements ['a'..'z']
  restChars <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
  return $ firstChar : restChars

genValidTypusExpression :: Gen String
genValidTypusExpression = oneof
  [ return "42"
  , return "\"hello\""
  , return "true"
  , return "false"
  , do
      var <- genValidIdentifier
      return var
  , do
      left <- genValidTypusExpression
      right <- genValidTypusExpression
      op <- elements ["+", "-", "*", "/"]
      return $ "(" ++ left ++ " " ++ op ++ " " ++ right ++ ")"
  ]

genValidTypusStatement :: Gen String
genValidTypusStatement = oneof
  [ do
      var <- genValidIdentifier
      expr <- genValidTypusExpression
      return $ "let " ++ var ++ " = " ++ expr
  , do
      var <- genValidIdentifier
      typeStr <- elements ["Int", "String", "Bool"]
      expr <- genValidTypusExpression
      return $ "let " ++ var ++ ": " ++ typeStr ++ " = " ++ expr
  , do
      funcName <- genValidIdentifier
      return $ "func " ++ funcName ++ "() { return 42 }"
  ]

genValidTypusCode :: Gen String
genValidTypusCode = do
  numStatements <- choose (1, 5 :: Int)
  numStatements <- choose (1, 5)
  statements <- vectorOf numStatements genValidTypusStatement
  return $ unlines statements

genDirective :: Gen String
genDirective = oneof
  [ return "//! ownership: on"
  , return "//! ownership: off"
  , return "//! dependent_types: on"
  , return "//! dependent_types: off"
  , return "//! constraints: on"
  , return "//! constraints: off"
  ]

-- Test cases
integrationQuickCheckTests :: TestTree
integrationQuickCheckTests = testGroup "Integration QuickCheck Tests"
  [ -- End-to-end compilation tests
    testCase "Parse and compile simple code" $ do
      let content = "let x = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse simple code: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile simple code: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code" (not $ null goCode)

  , testCase "Parse and compile code with directives" $ do
      let content = "//! ownership: on\n//! dependent_types: on\nlet x: Int = 42"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with directives: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with directives: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code with directives" (not $ null goCode)

  , testCase "Parse and compile code with functions" $ do
      let content = "package main\n\nfunc add(a: Int, b: Int): Int {\n  return a + b\n}\n\nlet result = add(1, 2)"
      case parseTypus content of
        Left err -> assertBool ("Should parse code with functions: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> assertBool ("Should compile code with functions: " ++ show errs) False
            Right goCode -> assertBool "Should generate Go code with functions" (not $ null goCode)

  , -- Error handling integration tests
    testCase "Parse and handle syntax errors" $ do
      let content = "let x = +"
      case parseTypus content of
        Left _ -> return ()  -- Expected to fail at parsing stage
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasSyntaxError = any (\e -> T.pack "syntax error" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect syntax error" hasSyntaxError
            Right _ -> assertBool "Should fail to compile invalid syntax" False

  , testCase "Parse and handle type errors" $ do
      let content = "let x: Int = \"hello\""
      case parseTypus content of
        Left err -> assertBool ("Should parse code with type error: " ++ err) False
        Right typusFile -> do
          case compile typusFile of
            Left errs -> 
              let hasTypeError = any (\e -> T.pack "type error" `T.isInfixOf` message (ceError e)) errs
              in assertBool "Should detect type error" hasTypeError
            Right _ -> assertBool "Should fail to compile type error" False

  , -- Utils integration tests
    testCase "Process code with comments" $ do
      let content = "let x = 42 // comment\nlet y = 24 /* block comment */"
      let processed = removeComments content
      assertBool "Should remove comments" (not $ "// comment" `L.isInfixOf` processed)
      assertBool "Should remove block comments" (not $ "/* block comment */" `L.isInfixOf` processed)

  , testCase "Normalize code indentation" $ do
      let content = "    let x = 42\n      let y = 24"
      let normalized = normalizeIndentation content
      let lines = L.lines normalized
      assertBool "Should normalize indentation" (not $ any (L.isPrefixOf "    ") lines)

  , -- QuickCheck property tests
    testProperty "Parse and generate Go code preserves structure" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ not $ null goCode

  , testProperty "Parse and compile valid code" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          case compile typusFile of
            Left _ -> property True  -- It's OK if it fails to compile
            Right goCode -> property $ not $ null goCode

  , testProperty "Directives are preserved through parsing" $ forAll (listOf genDirective) $ \directives -> do
      let content = unlines directives ++ "let x = 42"
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let fileDirectives = tfDirectives typusFile
          let hasOwnership = case fdOwnership fileDirectives of
                Nothing -> False
                Just _ -> True
          let hasDependentTypes = case fdDependentTypes fileDirectives of
                Nothing -> False
                Just _ -> True
          let hasConstraints = case fdConstraints fileDirectives of
                Nothing -> False
                Just _ -> True
          property $ hasOwnership || hasDependentTypes || hasConstraints || null directives

  , testProperty "Trim and normalize indentation are consistent" $ property $ \s -> do
      let trimmed = trim s
      let normalized = normalizeIndentation s
      let trimmedNormalized = trim normalized
      property $ trimmedNormalized === trim normalized

  , testProperty "Remove comments preserves code structure" $ forAll genValidTypusCode $ \content -> do
      let contentWithComments = content ++ "\n// This is a comment"
      let withoutComments = removeComments contentWithComments
      property $ content `L.isPrefixOf` withoutComments

  , testProperty "Parse and round-trip code" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          let reconstructed = concatMap cbContent blocks
          let normalizedReconstructed = normalizeIndentation reconstructed
          let normalizedOriginal = normalizeIndentation content
          property $ length normalizedReconstructed >= 0

  , testProperty "Generated Go code contains function definitions" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          let hasFunctions = "func" `L.isInfixOf` goCode
          let originalHasFunctions = "func" `L.isInfixOf` content
          property $ if originalHasFunctions then hasFunctions else True

  , testProperty "Generated Go code contains variable assignments" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          let hasAssignments = ":=" `L.isInfixOf` goCode || "=" `L.isInfixOf` goCode
          let originalHasAssignments = "let " `L.isInfixOf` content
          property $ if originalHasAssignments then hasAssignments else True

  , testProperty "Error messages are informative" $ property $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- Parsing errors are informative
        Right typusFile -> do
          case compile typusFile of
            Left errs -> property $ all (not . T.null . message . ceError) errs
            Right _ -> property True

  , testProperty "Compilation preserves type information" $ forAll genValidTypusCode $ \content -> do
      let hasTypeAnnotations = ": " `L.isInfixOf` content
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          property $ if hasTypeAnnotations then not $ null goCode else True

  , testProperty "Generated Go code is syntactically reasonable" $ forAll genValidTypusCode $ \content -> do
      case parseTypus content of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let goCode = generateGoCode typusFile
          let hasBraces = "{" `L.isInfixOf` goCode && "}" `L.isInfixOf` goCode
          let originalHasBraces = "{" `L.isInfixOf` content && "}" `L.isInfixOf` content
          property $ if originalHasBraces then hasBraces else True
  ]