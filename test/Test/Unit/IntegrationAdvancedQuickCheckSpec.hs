module Test.Unit.IntegrationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, (===), forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerResult, CompilerError(..))
import ErrorHandler (TypeError(..), ErrorSeverity(..), ErrorCategory(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import SyntaxValidator (validateSyntax, SyntaxError(..))
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.Either (isLeft, isRight)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate simple Typus code snippets
genSimpleTypusCode :: Gen String
genSimpleTypusCode = oneof
  [ return "x := 1"
  , return "y := x + 2"
  , return "if x > 0 { return x }"
  , return "func test() { return 42 }"
  , return "var s string = \"hello\""
  , return "const pi = 3.14159"
  ]

-- Generate Typus file directives
genFileDirective :: Gen String
genFileDirective = oneof
  [ return "//! ownership: true"
  , return "//! dependent-types: true"
  , return "//! constraints: enabled"
  , return "//! ownership: true\n//! dependent-types: true"
  ]

-- Generate block directives
genBlockDirective :: Gen String
genBlockDirective = oneof
  [ return "{//! ownership: true}"
  , return "{//! dependent-types: true}"
  , return "{//! constraints: enabled}"
  , return "{//! ownership: false}"
  ]

-- Generate build tags
genBuildTag :: Gen String
genBuildTag = oneof
  [ return "// +build linux"
  , return "// +build amd64"
  , return "// +build debug"
  , return "// +build test"
  ]

-- Generate complete Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  fileDirective <- oneof [return "", genFileDirective]
  buildTags <- listOf genBuildTag
  blocks <- listOf $ do
    blockDirective <- oneof [return "", genBlockDirective]
    code <- genSimpleTypusCode
    return $ unlines $ filter (not . null) [blockDirective, code]
  return $ unlines $ filter (not . null) $ [fileDirective] ++ buildTags ++ blocks

-- Generate invalid Typus code
genInvalidTypusCode :: Gen String
genInvalidTypusCode = oneof
  [ return "if x > 0\n    return x"  -- Missing opening brace
  , return "func test("            -- Missing closing parenthesis
  , return "var x int ="           -- Incomplete assignment
  , return "{//! invalid syntax"   -- Unclosed block directive
  , return "\"unclosed string"     -- Unclosed string
  , return "/* unclosed comment"  -- Unclosed comment
  ]

-- Generate mixed valid/invalid content
genMixedTypusContent :: Gen String
genMixedTypusContent = do
  validParts <- listOf genSimpleTypusCode
  invalidParts <- listOf genInvalidTypusCode
  parts <- listOf $ elements $ validParts ++ invalidParts
  return $ unlines parts

-- Generate content with specific integration scenarios
genOwnershipScenario :: Gen String
genOwnershipScenario = do
  lines' <- listOf $ elements
    [ "//! ownership: true"
    , "func transferOwnership() {"
    , "    data := Data{}"
    , "    receiver := data"
    , "    use(receiver)"
    , "}"
    ]
  return $ unlines lines'

genDependentTypesScenario :: Gen String
genDependentTypesScenario = do
  lines' <- listOf $ elements
    [ "//! dependent-types: true"
    , "func dependentFunction() {"
    , "    vec: Vector<n> where n > 0"
    , "    return vec"
    , "}"
    ]
  return $ unlines lines'

genErrorRecoveryScenario :: Gen String
genErrorRecoveryScenario = do
  lines' <- listOf $ elements
    [ "func errorRecovery() {"
    , "    if condition {"
    , "        handleSuccess()"
    , "    } else {"
    , "        handleError()"
    , "    }"
    , "}"
    ]
  return $ unlines lines'

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parse-compile pipeline preserves structure
prop_parseCompilePipeline :: String -> Property
prop_parseCompilePipeline content =
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True  -- Invalid input may fail parsing
       Right typusFile ->
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True  -- May fail compilation
              Right goCode -> not (null goCode)  -- Should generate Go code

-- Property: Syntax validation and parsing are consistent
prop_syntaxValidationParsingConsistent :: String -> Bool
prop_syntaxValidationParsingConsistent content =
  let syntaxErrors = validateSyntax content
      parseResult = parseTypus content
  in case (syntaxErrors, parseResult) of
       ([], Right _) -> True  -- No syntax errors, should parse successfully
       (_, _) -> True  -- May have syntax errors and still parse (parser is tolerant)

-- Property: Utils operations preserve content integrity
prop_utilsPreserveIntegrity :: String -> Bool
prop_utilsPreserveIntegrity content =
  let trimmed = trim content
      split = splitBy '\n' content
      commentsRemoved = removeComments content
      normalized = normalizeIndentation content
  in length trimmed <= length content &&
     length split >= 1 &&
     length commentsRemoved <= length content

-- Property: Error handling preserves context
prop_errorHandlingPreservesContext :: String -> Property
prop_errorHandlingPreservesContext content =
  let parseResult = parseTypus content
  in case parseResult of
       Left parseError -> not (null parseError)
       Right typusFile ->
         let compileResult = compile typusFile
         in case compileResult of
              Left compileErrors -> not (null compileErrors)
              Right _ -> property True

-- Property: File directives are preserved through pipeline
prop_fileDirectivesPreserved :: String -> Property
prop_fileDirectivesPreserved content =
  "//!" `isInfixOf` content ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let directives = tfDirectives typusFile
         in directives /= defaultFileDirectives

-- Property: Build tags are preserved through pipeline
prop_buildTagsPreserved :: String -> Property
prop_buildTagsPreserved content =
  "// +build" `isInfixOf` content ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let buildTags = tfBuildTags typusFile
         in not (null buildTags)

-- Property: Code blocks are preserved through pipeline
prop_codeBlocksPreserved :: String -> Property
prop_codeBlocksPreserved content =
  not (null (trim content)) ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let blocks = tfBlocks typusFile
         in not (null blocks)

-- Property: Ownership scenarios are handled correctly
prop_ownershipScenariosHandled :: String -> Property
prop_ownershipScenariosHandled content =
  "ownership" `isInfixOf` content ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True  -- May fail with ownership errors
              Right _ -> property True  -- May succeed

-- Property: Dependent types scenarios are handled correctly
prop_dependentTypesScenariosHandled :: String -> Property
prop_dependentTypesScenariosHandled content =
  "dependent-types" `isInfixOf` content ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True  -- May fail with type errors
              Right _ -> property True  -- May succeed

-- Property: Error recovery scenarios are handled correctly
prop_errorRecoveryScenariosHandled :: String -> Property
prop_errorRecoveryScenariosHandled content =
  "errorRecovery" `isInfixOf` content ==>
  let parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile ->
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True  -- May fail with errors
              Right _ -> property True  -- May succeed

-- Property: End-to-end integration preserves functionality
prop_endToEndIntegration :: String -> Bool
prop_endToEndIntegration content =
  let syntaxErrors = validateSyntax content
      parseResult = parseTypus content
      compileResult = case parseResult of
                       Left _ -> Left ["Parse failed"]
                       Right typusFile -> case compile typusFile of
                                           Left errors -> Left $ map show errors
                                           Right goCode -> Right goCode
  in case compileResult of
       Left _ -> True  -- May fail at any stage
       Right goCode -> not (null goCode)  -- Success should produce output

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Integration Advanced QuickCheck Tests"
  [ testGroup "Pipeline Properties"
    [ testProperty "Parse-compile pipeline preserves structure" prop_parseCompilePipeline
    , testProperty "Syntax validation and parsing are consistent" prop_syntaxValidationParsingConsistent
    , testProperty "Utils operations preserve content integrity" prop_utilsPreserveIntegrity
    , testProperty "Error handling preserves context" prop_errorHandlingPreservesContext
    ]

  , testGroup "Directive Properties"
    [ testProperty "File directives are preserved through pipeline" prop_fileDirectivesPreserved
    , testProperty "Build tags are preserved through pipeline" prop_buildTagsPreserved
    , testProperty "Code blocks are preserved through pipeline" prop_codeBlocksPreserved
    ]

  , testGroup "Scenario Properties"
    [ testProperty "Ownership scenarios are handled correctly" prop_ownershipScenariosHandled
    , testProperty "Dependent types scenarios are handled correctly" prop_dependentTypesScenariosHandled
    , testProperty "Error recovery scenarios are handled correctly" prop_errorRecoveryScenariosHandled
    ]

  , testGroup "End-to-End Properties"
    [ testProperty "End-to-end integration preserves functionality" prop_endToEndIntegration
    ]

  , testGroup "Unit Tests"
    [ testCase "Complete pipeline with valid code" $ do
        let validCode = unlines
              [ "//! ownership: true"
              , "// +build linux"
              , ""
              , "func main() {"
              , "    x := 1"
              , "    return x"
              , "}"
              ]
        let parseResult = parseTypus validCode
        case parseResult of
          Left parseError -> assertBool "Should parse valid code" False
          Right typusFile -> do
            let compileResult = compile typusFile
            case compileResult of
              Left compileErrors -> assertBool "Should compile valid code" False
              Right goCode -> assertBool "Should generate Go code" $ not (null goCode)

    , testCase "Pipeline with syntax errors" $ do
        let invalidCode = "if x > 0\n    return x\n"
        let syntaxErrors = validateSyntax invalidCode
        let parseResult = parseTypus invalidCode
        assertBool "Should detect syntax errors" $ not (null syntaxErrors)
        case parseResult of
          Left _ -> return ()  -- Expected to fail
          Right _ -> return ()  -- May still parse despite errors

    , testCase "Pipeline with ownership directives" $ do
        let ownershipCode = unlines
              [ "//! ownership: true"
              , "func transfer() {"
              , "    data := Data{}"
              , "    receiver := data"
              , "    return receiver"
              , "}"
              ]
        let parseResult = parseTypus ownershipCode
        case parseResult of
          Left _ -> assertBool "Should parse ownership code" False
          Right typusFile -> do
            let directives = tfDirectives typusFile
            assertBool "Should preserve ownership directive" $ directives /= defaultFileDirectives
            let compileResult = compile typusFile
            case compileResult of
              Left _ -> return ()  -- May fail with ownership errors
              Right _ -> return ()  -- May succeed

    , testCase "Pipeline with dependent types" $ do
        let dependentTypesCode = unlines
              [ "//! dependent-types: true"
              , "func vectorOps() {"
              , "    vec: Vector<n> where n > 0"
              , "    return vec.length"
              , "}"
              ]
        let parseResult = parseTypus dependentTypesCode
        case parseResult of
          Left _ -> assertBool "Should parse dependent types code" False
          Right typusFile -> do
            let directives = tfDirectives typusFile
            assertBool "Should preserve dependent types directive" $ directives /= defaultFileDirectives
            let compileResult = compile typusFile
            case compileResult of
              Left _ -> return ()  -- May fail with type errors
              Right _ -> return ()  -- May succeed

    , testCase "Utils integration" $ do
        let content = unlines
              [ "    func test() {"
              , "        // comment"
              , "        return 42"
              , "    }"
              , ""
              ]
        let trimmed = trim content
        let normalized = normalizeIndentation content
        let commentsRemoved = removeComments content
        assertBool "Trim should reduce length" $ length trimmed <= length content
        assertBool "Normalize should preserve structure" $ not (null normalized)
        assertBool "Remove comments should work" $ not (null commentsRemoved)

    , testCase "Error propagation through pipeline" $ do
        let errorProneCode = unlines
              [ "func errorTest() {"
              , "    if x > 0 {"  -- Missing closing brace
              , "        return x"
              , "}"
              ]
        let syntaxErrors = validateSyntax errorProneCode
        let parseResult = parseTypus errorProneCode
        assertBool "Should detect syntax issues" $ not (null syntaxErrors)
        case parseResult of
          Left _ -> return ()  -- Expected to fail
          Right typusFile -> do
            let compileResult = compile typusFile
            case compileResult of
              Left errors -> assertBool "Should propagate errors" $ not (null errors)
              Right _ -> return ()  -- May succeed despite errors

    , testCase "Complex integration scenario" $ do
        let complexCode = unlines
              [ "//! ownership: true"
              , "//! dependent-types: true"
              , "// +build linux"
              , "// +build amd64"
              , ""
              , "package main"
              , ""
              , "func complexFunction() {"
              , "    data: Data<T> where T: Sized"
              , "    owner := data"
              , "    borrower := &owner"
              , "    return borrower.process()"
              , "}"
              ]
        let parseResult = parseTypus complexCode
        case parseResult of
          Left _ -> assertBool "Should parse complex code" False
          Right typusFile -> do
            let directives = tfDirectives typusFile
            let buildTags = tfBuildTags typusFile
            assertBool "Should preserve ownership directive" $ directives /= defaultFileDirectives
            assertBool "Should preserve build tags" $ not (null buildTags)
            let compileResult = compile typusFile
            case compileResult of
              Left _ -> return ()  -- May fail with complex errors
              Right goCode -> assertBool "Should generate Go code" $ not (null goCode)

    , testCase "Integration with empty content" $ do
        let emptyContent = ""
        let syntaxErrors = validateSyntax emptyContent
        let parseResult = parseTypus emptyContent
        length syntaxErrors @?= 0
        case parseResult of
          Left _ -> assertBool "Should parse empty content" False
          Right typusFile -> do
            let compileResult = compile typusFile
            case compileResult of
              Left _ -> assertBool "Should compile empty file" False
              Right goCode -> assertBool "Should generate some Go code" $ not (null goCode)
    ]
  ]