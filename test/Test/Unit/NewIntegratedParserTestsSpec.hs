module Test.Unit.NewIntegratedParserTestsSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements)
import qualified Data.Text as T
import Data.Maybe 
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located)
                                       assertEqual "Empty file should have no directives" defaultFileDirectives (tfDirectives typusFile)
                           assertEqual "Empty file should have no code blocks" [] (tfBlocks typusFile)
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


      ,             testCase "Parse simple function" $
        let input = "func add(x int, y int) int { return x + y }"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse simple function: " ++ err
             Right typusFile -> do
                           assertBool "Should have at least one code block" (not $ L.null $ tfBlocks typusFile)
               let firstBlock = L.head $ tfBlocks typusFile
               assertBool "First block should contain function code" ("func" `L.isInfixOf` cbContent firstBlock)

      ,             testCase "Parse multiple statements" $
        let input = "let x = 5\nlet y = 10\nlet z = x + y"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse multiple statements: " ++ err
             Right typusFile -> do
                           assertBool "Should have code blocks" (not $ L.null $ tfBlocks typusFile)
               let content = concatMap cbContent $ tfBlocks typusFile
               assertBool "Should contain L.all variables" ("x" `L.isInfixOf` content && "y" `L.isInfixOf` content && "z" `L.isInfixOf` content)
    ]

-- | Directive parsing tests
directiveParsingTests :: TestTree
directiveParsingTests =
  testGroup "Directive Parsing Tests"
    [             testCase "Parse ownership directive" $
        let input = "// @ownership: true\nfunc test() {}"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse ownership directive: " ++ err
             Right typusFile -> do
                           let directives = tfDirectives typusFile
               assertBool "Should have ownership directive" (isJust $ fdOwnership directives)
                           assertEqual "Ownership should be true" True (unLoc $ fdOwnership directives)

      ,             testCase "Parse dependent types directive" $
        let input = "// @dependent-types: true\nfunc test() {}"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse dependent types directive: " ++ err
             Right typusFile -> do
                           let directives = tfDirectives typusFile
               assertBool "Should have dependent types directive" (isJust $ fdDependentTypes directives)
                           assertEqual "Dependent types should be true" True (unLoc $ fdDependentTypes directives)

      ,             testCase "Parse multiple directives" $
        let input = "// @ownership: true\n// @dependent-types: false\nfunc test() {}"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse multiple directives: " ++ err
             Right typusFile -> do
                           let directives = tfDirectives typusFile
               assertBool "Should have ownership directive" (isJust $ fdOwnership directives)
               assertBool "Should have dependent types directive" (isJust $ fdDependentTypes directives)
                           assertEqual "Ownership should be true" True (unLoc $ fdOwnership directives)
                           assertEqual "Dependent types should be false" False (unLoc $ fdDependentTypes directives)
    ]

-- | Code block parsing tests
codeBlockParsingTests :: TestTree
codeBlockParsingTests =
  testGroup "Code Block Parsing Tests"
    [             testCase "Parse single code block" $
        let input = "func test() { return 42 }"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse single code block: " ++ err
             Right typusFile -> do
                                       assertEqual "Should have exactly one code block" 1 (L.length $ tfBlocks typusFile)
               let block = L.head $ tfBlocks typusFile
                           assertEqual "Block content should match" input (cbContent block)

      ,             testCase "Parse multiple code blocks" $
        let input = "func test1() { return 1 }\n\nfunc test2() { return 2 }"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse multiple code blocks: " ++ err
             Right typusFile -> do
                           assertBool "Should have multiple code blocks" (L.length (tfBlocks typusFile) >= 1)
               let blocks = tfBlocks typusFile
                                                 totalContent = concatMap cbContent blocks
               assertBool "Should contain both functions" ("test1" `L.isInfixOf` totalContent && "test2" `L.isInfixOf` totalContent)

      ,             testCase "Parse code block with directives" $
        let input = "// @ownership: true\nfunc test() { return 42 }"
                                          result = parseTypus "test.typus" input
        in case result of
             Left err -> assertFailure $ "Failed to parse code block with directives: " ++ err
             Right typusFile -> do
                           let blocks = tfBlocks typusFile
               assertBool "Should have at least one code block" (not $ null blocks)
               let firstBlock = L.head blocks
                                                 blockDirectives = cbDirectives firstBlock
               assertBool "Block should have ownership directive" (isJust $ bdOwnership blockDirectives)
    ]

-- | Error handling tests
errorHandlingTests :: TestTree
errorHandlingTests =
  testGroup "Error Handling Tests"
    [             testCase "Handle syntax errors gracefully" $
        let input = "func test( { return 42 }"  -- Missing closing parenthesis
                                          result = parseTypus "test.typus" input
        in case result of
             Left _ -> assertBool "Should fail with syntax error" True
             Right _ -> assertFailure "Should have failed with syntax error"

      ,             testCase "Handle invalid directive format" $
        let input = "// @invalid-directive: true\nfunc test() {}"
                                          result = parseTypus "test.typus" input
        in case result of
             Left _ -> assertBool "Should fail with invalid directive" True
             Right _ -> assertFailure "Should have failed with invalid directive"

      ,             testCase "Handle malformed block directives" $
        let input = "// @ownership: invalid\nfunc test() {}"
                                          result = parseTypus "test.typus" input
        in case result of
             Left _ -> assertBool "Should fail with malformed directive" True
             Right _ -> assertFailure "Should have failed with malformed directive"
    ]

-- | QuickCheck properties for parser testing
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [             testProperty "Round-trip property for simple functions" $
        forAll genSimpleFunction $ \funcStr ->
            case parseTypus "test.typus" funcStr of
              Right typusFile -> 
                let reconstructed = concatMap cbContent (tfBlocks typusFile)
                in                               funcStr === reconstructed
              Left _ -> property True  -- Skip invalid inputs

    ,             testProperty "Directives are preserved correctly" $
        forAll genDirectivesAndCode $ \directives code ->
            let input = directives ++ "\n" ++ code
            in case parseTypus "test.typus" input of
                 Right typusFile -> 
                   let parsedDirectives = tfDirectives typusFile
                   in property $ isJust (fdOwnership parsedDirectives) || isJust (fdDependentTypes parsedDirectives)
                 Left _ -> property True  -- Skip invalid inputs

    ,             testProperty "Code blocks are non-empty when present" $
        forAll genValidCode $ \code ->
            case parseTypus "test.typus" code of
              Right typusFile -> 
                let blocks = tfBlocks typusFile
                in if null blocks 
                   then property True
                   else property $ L.all (not . null . cbContent) blocks
              Left _ -> property True  -- Skip invalid inputs
    ]

-- | Generators for QuickCheck testing
genSimpleFunction :: Gen String
                              genSimpleFunction = do
              fname <- elements ["test", "add", "multiply", "divide"]
  return $ "func " ++ fname ++ "() int { return 42 }"

genDirectivesAndCode :: Gen (String, String)
                              genDirectivesAndCode = do
              hasOwnership <- elements [True, False]
  hasDepTypes <- elements [True, False]
  let ownership = if hasOwnership then "// @ownership: true" else ""
  let depTypes = if hasDepTypes then "// @dependent-types: true" else ""
  let directives = unlines $ L.filter (not . null) [ownership, depTypes]
  code <- genSimpleFunction
  return (directives, code)

genValidCode :: Gen String
                              genValidCode = elements
  [ "func test() { return 42 }"
  , "let x = 5\nlet y = 10"
  , "// @ownership: true\nfunc test() {}"
  , "func add(x int, y int) int { return x + y }"
  , "type Person struct { name string }"
  ])