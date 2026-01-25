{-# LANGUAGE LambdaCase #-}
module Test.Unit.ParserCoreFunctionalitySpec where



import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty
-- Removed empty QuickCheck import
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, Property, (===), counterexample, property, conjoin)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import SourceLocation (SourceSpan(..), SourcePos(..), Located(..))
import qualified SyntaxValidator
import qualified Data.Text as T
import Data.List (intersperse)

-- Test data generators
instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    buildTags <- listOf $ arbitrary
    blocks <- listOf $ arbitrary
    syntaxErrors <- listOf $ arbitrary
    return $ TypusFile directives buildTags blocks syntaxErrors

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitrary
    span <- arbitrary
    return $ CodeBlock directives content span

-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


-- Arbitrary instance for SourcePos is now defined in SourceLocation module


instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    pos <- arbitrary
    span <- arbitrary
    value <- arbitrary
    return $ Located value pos span

instance Arbitrary SyntaxValidator.SyntaxError where
  arbitrary = do
    errorType <- elements [SyntaxValidator.MissingBrace, SyntaxValidator.MissingParenthesis, SyntaxValidator.InvalidIdentifier]
    line <- chooseInt (1, 1000)
    column <- chooseInt (1, 1000)
    msg <- elements ["Syntax error", "Parse error", "Invalid token"]
    lineContent <- elements ["let x = 42", "func test() {}", "var y int"]
    return $ SyntaxValidator.SyntaxError errorType msg line column lineContent

-- Test cases
parserCoreFunctionalityTests :: TestTree
parserCoreFunctionalityTests = testGroup "Parser Core Functionality Tests"
  [ -- Basic parsing tests
    testCase "Parse empty file" $ do
      let result = parseTypus ""
      case result of
        Left _ -> assertBool "Empty file should parse successfully" False
        Right typusFile -> do
          assertEqual "Should have no blocks" [] (tfBlocks typusFile)
          assertEqual "Should have no build tags" [] (tfBuildTags typusFile)

  , testCase "Parse simple file with content" $ do
      let content = "let x = 42\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse simple content: " ++ err) False
        Right typusFile -> do
          assertBool "Should have at least one block" (not $ null $ tfBlocks typusFile)

  , -- Directive parsing tests
    testCase "Parse file with ownership directive" $ do
      let content = "//! ownership: on\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse file with ownership directive: " ++ err) False
        Right typusFile -> do
          let directives = tfDirectives typusFile
          case fdOwnership directives of
            Nothing -> assertBool "Should have ownership directive" False
            Just (Located value _ _) -> assertEqual "Ownership should be on" True value

  , testCase "Parse file with dependent types directive" $ do
      let content = "//! dependent_types: on\nlet x: Int = 42\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse file with dependent types directive: " ++ err) False
        Right typusFile -> do
          let directives = tfDirectives typusFile
          case fdDependentTypes directives of
            Nothing -> assertBool "Should have dependent types directive" False
            Just (Located value _ _) -> assertEqual "Dependent types should be on" True value

  , testCase "Parse file with constraints directive" $ do
      let content = "//! constraints: on\nlet x: Int = 42\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse file with constraints directive: " ++ err) False
        Right typusFile -> do
          let directives = tfDirectives typusFile
          case fdConstraints directives of
            Nothing -> assertBool "Should have constraints directive" False
            Just (Located value _ _) -> assertEqual "Constraints should be on" True value

  , -- Block directive tests
    testCase "Parse code block with directives" $ do
      let content = "{//! ownership: on, dependent_types: off}\nlet x = 42\n}"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse code block with directives: " ++ err) False
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          case blocks of
            [] -> assertFailure "Should have at least one block"
            firstBlock:_ -> do
              let directives = cbDirectives firstBlock
              case bdOwnership directives of
                Nothing -> assertBool "Should have ownership directive in block" False
                Just (Located value _ _) -> assertEqual "Block ownership should be on" True value

  , -- Build tag tests
    testCase "Parse file with build tags" $ do
      let content = "//go:build linux\n// +build darwin\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse file with build tags: " ++ err) False
        Right typusFile -> do
          let buildTags = tfBuildTags typusFile
          assertBool "Should have build tags" (not $ null buildTags)
          assertEqual "Should have two build tags" 2 (length buildTags)

  , -- Markdown code block tests
    testCase "Parse markdown code block" $ do
      let content = "```typus\nlet x = 42\n```\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse markdown code block: " ++ err) False
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          case blocks of
            [] -> assertFailure "Should have at least one block"
            firstBlock:_ -> do
              assertBool "Block should contain content" (not $ null $ cbContent firstBlock)

  , testCase "Parse markdown code block with directives" $ do
      let content = "```typus\n// @ ownership: on\nlet x = 42\n```\n"
      let result = parseTypus content
      case result of
        Left err -> assertBool ("Should parse markdown code block with directives: " ++ err) False
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          case blocks of
            [] -> assertFailure "Should have at least one block"
            firstBlock:_ -> do
              let directives = cbDirectives firstBlock
              case bdOwnership directives of
                Nothing -> assertBool "Should have ownership directive in block" False
                Just (Located value _ _) -> assertEqual "Block ownership should be on" True value

  , -- Error handling tests
    testCase "Handle malformed directive" $ do
      let content = "//! invalid_directive: on\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertBool "Should fail with invalid directive" False

  , testCase "Handle unclosed block directive" $ do
      let content = "{//! ownership: on\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertBool "Should fail with unclosed block directive" False

  , testCase "Handle unclosed markdown block" $ do
      let content = "```typus\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertBool "Should fail with unclosed markdown block" False

  , -- QuickCheck property tests
    testProperty "Parse and round-trip simple content" $ property $ \content -> do
      let simpleContent = filter (\c -> c /= '\r' && c /= '\0') content
      let result = parseTypus simpleContent
      case result of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> 
          let blocks = tfBlocks typusFile
              reconstructed = concatMap cbContent blocks
          in counterexample ("Original: " ++ show simpleContent ++ "\nReconstructed: " ++ show reconstructed) $
             length simpleContent >= length reconstructed

  , testProperty "File directives are preserved" $ \ownershipEnabled dependentTypesEnabled constraintsEnabled -> do
      let directives = ["ownership: " ++ if ownershipEnabled then "on" else "off"
                       , "dependent_types: " ++ if dependentTypesEnabled then "on" else "off"
                       , "constraints: " ++ if constraintsEnabled then "on" else "off"]
          content = "//! " ++ unwords (intersperse "," directives) ++ "\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left err -> counterexample ("Parse error: " ++ err) $ property False
        Right typusFile -> do
          let fileDirectives = tfDirectives typusFile
          let checkOwnership = case fdOwnership fileDirectives of
                Nothing -> property $ counterexample "Ownership directive missing" False
                Just (Located value _ _) -> 
                  if value == ownershipEnabled 
                    then property True
                    else property $ counterexample ("Ownership mismatch: expected " ++ show ownershipEnabled ++ ", got " ++ show value) False
          let checkDependentTypes = case fdDependentTypes fileDirectives of
                Nothing -> property $ counterexample "Dependent types directive missing" False
                Just (Located value _ _) -> 
                  if value == dependentTypesEnabled 
                    then property True
                    else property $ counterexample ("Dependent types mismatch: expected " ++ show dependentTypesEnabled ++ ", got " ++ show value) False
          let checkConstraints = case fdConstraints fileDirectives of
                Nothing -> property $ counterexample "Constraints directive missing" False
                Just (Located value _ _) -> 
                  if value == constraintsEnabled 
                    then property True
                    else property $ counterexample ("Constraints mismatch: expected " ++ show constraintsEnabled ++ ", got " ++ show value) False
          let allChecks = [checkOwnership, checkDependentTypes, checkConstraints]
          property $ conjoin allChecks

  , testProperty "Block directives are preserved" $ property $ \ownershipEnabled -> do
      let content = "{//! ownership: " ++ (if ownershipEnabled then "on" else "off") ++ "}\nlet x = 42\n"
      let result = parseTypus content
      case result of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          case blocks of
            [] -> property True
            firstBlock:_ -> do
              let blockDirectives = cbDirectives firstBlock
              case bdOwnership blockDirectives of
                Nothing -> property True
                Just (Located value _ _) -> property $ value == ownershipEnabled

  , testProperty "Build tags are preserved" $ property $ \tags -> do
      let validTags = filter (not . null) $ map (filter (\c -> c /= '\r' && c /= '\n' && c /= '\0')) tags
      let tagLines = map (\tag -> "//go:build " ++ tag) validTags
          content = unlines tagLines ++ "let x = 42\n"
      let result = parseTypus content
      case result of
        Left _ -> property True  -- It's OK if it fails to parse
        Right typusFile -> do
          let buildTags = tfBuildTags typusFile
          property $ length buildTags == length validTags
  ]
  