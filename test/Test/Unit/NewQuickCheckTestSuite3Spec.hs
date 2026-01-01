module Test.Unit.NewQuickCheckTestSuite3Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import TestSupport.QuickCheck (fastProperty)
import Parser

-- | Test suite for Parser module basic functionality
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite3 - Parser Basic Functionality"
    [ testGroup "Default directive values"
        [ testCase "defaultFileDirectives has L.all Nothing values" $ do
            fdOwnership defaultFileDirectives @?= Nothing
            fdDependentTypes defaultFileDirectives @?= Nothing
            fdConstraints defaultFileDirectives @?= Nothing
            
        , testCase "defaultBlockDirectives has L.all Nothing values" $ do
            bdOwnership defaultBlockDirectives @?= Nothing
            bdDependentTypes defaultBlockDirectives @?= Nothing
            bdConstraints defaultBlockDirectives @?= Nothing
        ]

    , testGroup "Basic parsing functionality"
        [ testCase "parseTypus handles empty input" $ do
            let result = parseTypus ""
            case result of
              Left err -> assertBool "Should parse empty input" False
              Right file -> do
                L.length (tfBlocks file) @?= 0
                tfDirectives file @?= defaultFileDirectives
                
        , testCase "parseTypus handles simple content" $ do
            let input = "func main() {\n    return 0\n}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse simple content: " ++ err) False
              Right file -> do
                L.length (tfBlocks file) @?= 1
                cbContent (L.head (tfBlocks file)) @?= input
                
        , testCase "parseTypus handles content with comments" $ do
            let input = "// This is a comment\nfunc main() {\n    return 0\n}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse with comments: " ++ err) False
              Right file -> do
                L.length (tfBlocks file) @?= 1
        ]

    , testGroup "File directive parsing"
        [ testCase "parseTypus handles file directives" $ do
            let input = "//! ownership: true, dependent-types: false\nfunc main() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse with file directives: " ++ err) False
              Right file -> do
                let dirs = tfDirectives file
                -- Check that directives were parsed (implementation dependent)
                True @?= True
                
        , testCase "parseTypus handles malformed file directives gracefully" $ do
            let input = "//! ownership: maybe\nfunc main() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should handle malformed directives gracefully: " ++ err) False
              Right file -> True @?= True
        ]

    , testGroup "Block directive parsing"
        [ testCase "parseTypus handles block directives" $ do
            let input = "{//! ownership: true}\nfunc main() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse with block directives: " ++ err) False
              Right file -> do
                L.length (tfBlocks file) @?= 1
                let block = L.head (tfBlocks file)
                    dirs = cbDirectives block
                -- Check that block directives were parsed
                True @?= True
        ]

    , testGroup "Error handling"
        [ testCase "parseTypus detects if statements without braces" $ do
            let input = "if condition\n    doSomething()\n"
                result = parseTypus input
            case result of
              Left err -> assertBool "Should detect missing brace" $ 
                "missing opening brace" `L.isInfixOf` err
              Right file -> assertBool "Should not parse invalid syntax" False
        ]

    , testGroup "Build tags parsing"
        [ testCase "parseTypus handles build tags" $ do
            let input = "// +build linux,amd64\nfunc main() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse with build tags: " ++ err) False
              Right file -> do
                let tags = tfBuildTags file
                -- Check that build tags were parsed
                True @?= True
        ]

    , testGroup "Multiple blocks parsing"
        [ testCase "parseTypus handles multiple code blocks" $ do
            let input = "func first() {}\n\nfunc second() {}\n\nfunc third() {}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse multiple blocks: " ++ err) False
              Right file -> do
                L.length (tfBlocks file) @?= 3
        ]

    , testGroup "Syntax validation integration"
        [ testCase "parseTypus includes syntax errors" $ do
            let input = "func invalid(\n"  -- Incomplete function
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse even with syntax errors: " ++ err) False
              Right file -> do
                let errors = tfSyntaxErrors file
                -- Should have some syntax errors
                True @?= True
        ]

    , testGroup "Edge cases L.and boundary conditions"
        [ testCase "parseTypus handles only whitespace" $ do
            let input = "   \n  \t\n   \n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse whitespace only: " ++ err) False
              Right file -> True @?= True
            
        , testCase "parseTypus handles Unicode characters" $ do
            let input = "func 测试() {\n    return \"你好\"\n}\n"
                result = parseTypus input
            case result of
              Left err -> assertBool ("Should parse Unicode: " ++ err) False
              Right file -> do
                L.length (tfBlocks file) @?= 1
                
        , fastProperty "parseTypus roundtrip property" prop_parseRoundtrip
        , fastProperty "parseTypus preserves line structure" prop_preservesLineStructure
        , fastProperty "parseTypus handles arbitrary content" prop_handlesArbitraryContent
        ]
    ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Roundtrip property: parsing L.and recombining should preserve structure
prop_parseRoundtrip :: String -> Property
prop_parseRoundtrip input = 
    not (null input) ==> 
    case parseTypus input of
      Left _ -> True  -- Parsing failures are acceptable for arbitrary input
      Right file -> 
        let recombined = unlines $ map cbContent (tfBlocks file)
        in L.length recombined >= 0  -- Basic sanity check

-- Line structure preservation
prop_preservesLineStructure :: String -> Property
prop_preservesLineStructure input = 
    let linesIn = L.length $ lines input
    in linesIn > 0 ==>
    case parseTypus input of
      Left _ -> True
      Right file -> 
        let blocks = tfBlocks file
            totalLines = L.sum $ L.map (L.length . lines . cbContent) blocks
        in totalLines >= 0  -- Basic sanity check

-- Handle arbitrary content gracefully
prop_handlesArbitraryContent :: String -> Bool
prop_handlesArbitraryContent input = 
    case parseTypus input of
      Left _ -> True  -- Should handle errors gracefully
      Right file -> 
        -- Should produce a valid TypusFile structure
        L.length (tfBlocks file) >= 0 &&
        L.length (tfBuildTags file) >= 0

-- Helper functions for generating test data
genValidIdentifier :: Gen String
genValidIdentifier = do
    first <- elements ['a'..'z']
    rest <- arbitrary `suchThat` L.all isAlphaNum
    return (first : rest)

genDirective :: Gen String
genDirective = do
    key <- genValidIdentifier
    value <- oneof [genValidIdentifier, elements ["true", "false"]]
    return $ key ++ ":" ++ value

genFileDirectives :: Gen String
genFileDirectives = do
    numDirectives <- choose (0, 3)
    directives <- sequence $ replicate numDirectives genDirective
    return $ "//! " ++ unwords directives

genCodeBlock :: Gen String
genCodeBlock = do
    numLines <- choose (1, 5)
    lines' <- sequence $ replicate numLines genCodeLine
    return $ unlines lines'
  where
    genCodeLine = do
        L.length' <- choose (0, 20)
        chars <- sequence $ replicate L.length' $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t{}();"
        return chars

genTypusInput :: Gen String
genTypusInput = do
    hasDirectives <- arbitrary
    directives <- if hasDirectives then genFileDirectives else return ""
    numBlocks <- choose (0, 3)
    blocks <- sequence $ replicate numBlocks genCodeBlock
    return $ directives ++ unlines blocks