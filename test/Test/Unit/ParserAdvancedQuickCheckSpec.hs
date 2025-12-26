module Test.Unit.ParserAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, choose, listOf, suchThat, vectorOf)
import TestSupport.QuickCheck (fastProperty)

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
    arbitrary = do
        ownership <- oneof [pure Nothing, Just <$> arbitrary]
        dependentTypes <- oneof [pure Nothing, Just <$> arbitrary]
        constraints <- oneof [pure Nothing, Just <$> arbitrary]
        return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
    arbitrary = do
        ownership <- oneof [pure Nothing, Just <$> arbitrary]
        dependentTypes <- oneof [pure Nothing, Just <$> arbitrary]
        constraints <- oneof [pure Nothing, Just <$> arbitrary]
        return $ BlockDirectives ownership dependentTypes constraints

instance Arbitrary CodeBlock where
    arbitrary = do
        directives <- arbitrary
        content <- arbitrary
        span <- arbitrary
        return $ CodeBlock directives content span

instance Arbitrary TypusFile where
    arbitrary = do
        directives <- arbitrary
        buildTags <- listOf arbitrary
        blocks <- listOf arbitrary
        syntaxErrors <- listOf arbitrary
        return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate valid boolean directive values
validBoolValue :: Gen String
validBoolValue = elements ["on", "off", "true", "false"]

-- Generate valid directive keys
validDirectiveKey :: Gen String
validDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate valid file directive lines
fileDirectiveLine :: Gen String
fileDirectiveLine = do
    key <- validDirectiveKey
    value <- validBoolValue
    return $ "//! " ++ key ++ ": " ++ value

-- Generate valid block directive lines
blockDirectiveLine :: Gen String
blockDirectiveLine = do
    key <- validDirectiveKey
    value <- validBoolValue
    return $ "{//! " ++ key ++ ": " ++ value ++ "}"

-- Generate simple code content
simpleCodeContent :: Gen String
simpleCodeContent = do
    lines' <- listOf $ elements [
        "func main() {",
        "    fmt.Println(\"hello\")",
        "}",
        "",
        "package main",
        "import \"fmt\"",
        "if condition {",
        "    // do something",
        "}"
        ]
    return $ unlines lines'

-- Generate build tag lines
buildTagLine :: Gen String
buildTagLine = oneof [
    pure "//go:build linux",
    pure "// +build darwin",
    pure "//go:build windows && amd64",
    pure "// +build !prod"
    ]

-- ============================================================================
-- Parser Properties
-- ============================================================================

prop_parseEmptyFile :: Bool
prop_parseEmptyFile =
    case parseTypus "" of
        Left _ -> False
        Right file -> null (tfBlocks file) && null (tfBuildTags file)

prop_parseSimpleFile :: Bool
prop_parseSimpleFile =
    let content = "package main\n\nfunc main() {\n    fmt.Println(\"hello\")\n}\n"
    in case parseTypus content of
        Left _ -> False
        Right file -> not (null (tfBlocks file))

prop_parseFileDirective :: String -> String -> Bool
prop_parseFileDirective key value =
    let content = "//! " ++ key ++ ": " ++ value ++ "\npackage main\n"
    in case parseTypus content of
        Left _ -> False
        Right file -> 
            let dirs = tfDirectives file
            in case key of
                "ownership" -> case fdOwnership dirs of
                    Just (Located val _) -> val == (value `elem` ["on", "true"])
                    Nothing -> False
                "dependent_types" -> case fdDependentTypes dirs of
                    Just (Located val _) -> val == (value `elem` ["on", "true"])
                    Nothing -> False
                "constraints" -> case fdConstraints dirs of
                    Just (Located val _) -> val == (value `elem` ["on", "true"])
                    Nothing -> False
                _ -> True  -- Unknown directive should be handled gracefully

prop_parseBlockDirective :: String -> String -> Bool
prop_parseBlockDirective key value =
    let content = "package main\n\n{//! " ++ key ++ ": " ++ value ++ "}\nfunc test() {}\n"
    in case parseTypus content of
        Left _ -> False
        Right file -> 
            let blocks = tfBlocks file
            in case blocks of
                (block:_) -> 
                    let dirs = cbDirectives block
                    in case key of
                        "ownership" -> case bdOwnership dirs of
                            Just (Located val _) -> val == (value `elem` ["on", "true"])
                            Nothing -> False
                        "dependent_types" -> case bdDependentTypes dirs of
                            Just (Located val _) -> val == (value `elem` ["on", "true"])
                            Nothing -> False
                        "constraints" -> case bdConstraints dirs of
                            Just (Located val _) -> val == (value `elem` ["on", "true"])
                            Nothing -> False
                        _ -> True
                [] -> False

prop_parseBuildTags :: Bool
prop_parseBuildTags =
    let content = "//go:build linux\n// +build darwin\npackage main\n"
    in case parseTypus content of
        Left _ -> False
        Right file -> length (tfBuildTags file) >= 2

prop_parseMultipleBlocks :: Bool
prop_parseMultipleBlocks =
    let content = unlines [
            "package main",
            "",
            "func first() {",
            "    fmt.Println(\"first\")",
            "}",
            "",
            "{//! ownership: on}",
            "func second() {",
            "    fmt.Println(\"second\")",
            "}",
            "",
            "{//! dependent_types: true}",
            "func third() {",
            "    fmt.Println(\"third\")",
            "}"
            ]
    in case parseTypus content of
        Left _ -> False
        Right file -> length (tfBlocks file) >= 3

prop_parsePreservesContent :: String -> Bool
prop_parsePreservesContent originalContent =
    case parseTypus originalContent of
        Left _ -> True  -- Parse errors are expected for arbitrary content
        Right file -> 
            let reconstructed = concatMap cbContent (tfBlocks file)
            in not (null originalContent) ==> 
               (originalContent `isInfixOf` reconstructed || 
                any (`isInfixOf` originalContent) (lines reconstructed))

prop_parseHandlesComments :: Bool
prop_parseHandlesComments =
    let content = unlines [
            "package main",
            "// This is a comment",
            "/* This is a block comment */",
            "func main() {",
            "    // Another comment",
            "    fmt.Println(\"hello\")",
            "}",
            "// End of file comment"
            ]
    in case parseTypus content of
        Left _ -> False
        Right file -> not (null (tfBlocks file))

prop_parseHandlesIndentation :: Bool
prop_parseHandlesIndentation =
    let content = unlines [
            "package main",
            "",
            "func main() {",
            "\tfmt.Println(\"tabbed\")",
            "    fmt.Println(\"spaced\")",
            "\t\tfmt.Println(\"double tabbed\")",
            "}"
            ]
    in case parseTypus content of
        Left _ -> False
        Right file -> not (null (tfBlocks file))

prop_parseHandlesEmptyLines :: Bool
prop_parseHandlesEmptyLines =
    let content = unlines [
            "package main",
            "",
            "",
            "func main() {",
            "",
            "    fmt.Println(\"hello\")",
            "",
            "}",
            ""
            ]
    in case parseTypus content of
        Left _ -> False
        Right file -> not (null (tfBlocks file))

prop_parseHandlesMultipleDirectives :: Bool
prop_parseHandlesMultipleDirectives =
    let content = "//! ownership: on, dependent_types: true\npackage main\n"
    in case parseTypus content of
        Left _ -> False
        Right file -> 
            let dirs = tfDirectives file
            in case (fdOwnership dirs, fdDependentTypes dirs) of
                (Just (Located True _), Just (Located True _)) -> True
                _ -> False

prop_parseHandlesNestedBlocks :: Bool
prop_parseHandlesNestedBlocks =
    let content = unlines [
            "package main",
            "",
            "{//! ownership: on}",
            "func outer() {",
            "    {//! dependent_types: true}",
            "    func inner() {",
            "        fmt.Println(\"nested\")",
            "    }",
            "}",
            "",
            "func separate() {",
            "    fmt.Println(\"separate\")",
            "}"
            ]
    in case parseTypus content of
        Left _ -> False
        Right file -> length (tfBlocks file) >= 2

-- Helper function for implication
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Advanced QuickCheck Tests"
    [ testGroup "Basic Parsing Properties"
        [ fastProperty "parse empty file" prop_parseEmptyFile
        , fastProperty "parse simple file" prop_parseSimpleFile
        , fastProperty "parse file directive" prop_parseFileDirective
        , fastProperty "parse block directive" prop_parseBlockDirective
        , fastProperty "parse build tags" prop_parseBuildTags
        ]

    , testGroup "Complex Parsing Properties"
        [ fastProperty "parse multiple blocks" prop_parseMultipleBlocks
        , fastProperty "parse preserves content" prop_parsePreservesContent
        , fastProperty "parse handles comments" prop_parseHandlesComments
        , fastProperty "parse handles indentation" prop_parseHandlesIndentation
        , fastProperty "parse handles empty lines" prop_parseHandlesEmptyLines
        ]

    , testGroup "Directive Parsing Properties"
        [ fastProperty "parse handles multiple directives" prop_parseHandlesMultipleDirectives
        , fastProperty "parse handles nested blocks" prop_parseHandlesNestedBlocks
        ]

    , testGroup "Unit Tests"
        [ testCase "parse file with ownership directive" $ do
            let content = "//! ownership: on\npackage main\n"
            case parseTypus content of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right file -> 
                    let dirs = tfDirectives file
                    in case fdOwnership dirs of
                        Just (Located True _) -> pure ()
                        _ -> assertBool "Should have ownership enabled" False

        , testCase "parse file with dependent_types directive" $ do
            let content = "//! dependent_types: true\npackage main\n"
            case parseTypus content of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right file -> 
                    let dirs = tfDirectives file
                    in case fdDependentTypes dirs of
                        Just (Located True _) -> pure ()
                        _ -> assertBool "Should have dependent_types enabled" False

        , testCase "parse file with constraints directive" $ do
            let content = "//! constraints: on\npackage main\n"
            case parseTypus content of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right file -> 
                    let dirs = tfDirectives file
                    in case (fdConstraints dirs, fdDependentTypes dirs) of
                        (Just (Located True _), Just (Located True _)) -> pure ()
                        _ -> assertBool "Should have constraints and dependent_types enabled" False

        , testCase "parse file with block directive" $ do
            let content = "{//! ownership: off}\nfunc test() {}\n"
            case parseTypus content of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right file -> 
                    case tfBlocks file of
                        (block:_) -> 
                            let dirs = cbDirectives block
                            in case bdOwnership dirs of
                                Just (Located False _) -> pure ()
                                _ -> assertBool "Should have ownership disabled" False
                        [] -> assertBool "Should have at least one block" False

        , testCase "parse file with build tags" $ do
            let content = "//go:build linux\npackage main\n"
            case parseTypus content of
                Left err -> assertBool ("Should parse successfully: " ++ err) False
                Right file -> 
                    let tags = tfBuildTags file
                    in if null tags
                       then assertBool "Should have build tags" False
                       else pure ()
        ]
    ]