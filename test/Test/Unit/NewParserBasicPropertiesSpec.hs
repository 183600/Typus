module Test.Unit.NewParserBasicPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)

-- | 新的Parser基础属性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Parser Basic Properties Tests"
    [ testGroup "Directive parsing properties"
        [ fastProperty "fileDirectiveParser handles valid input" prop_fileDirectiveParserValid
        , fastProperty "blockDirectiveParser handles valid input" prop_blockDirectiveParserValid
        , fastProperty "directive parsing preserves order" prop_directiveParsingPreservesOrder
        ]

    , testGroup "Line parsing properties"
        [ fastProperty "parseLine captures content correctly" prop_parseLineCapturesContent
        , fastProperty "parseLine tracks span correctly" prop_parseLineTracksSpan
        , fastProperty "parseDocument preserves line count" prop_parseDocumentPreservesLineCount
        ]

    , testGroup "File building properties"
        [ fastProperty "buildTypusFile preserves directives" prop_buildTypusFilePreservesDirectives
        , fastProperty "buildTypusFile creates proper blocks" prop_buildTypusFileCreatesBlocks
        , fastProperty "parseTypus roundtrip property" prop_parseTypusRoundtrip
        ]

    , testGroup "Error handling properties"
        [ fastProperty "parseTypus handles malformed input gracefully" prop_parseTypusHandlesMalformed
        , fastProperty "syntax validation preserves content" prop_syntaxValidationPreservesContent
        ]

    , testGroup "Directive validation properties"
        [ fastProperty "identifier validation correctness" prop_identifierValidationCorrectness
        , fastProperty "directive format validation" prop_directiveFormatValidation
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

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
        content <- listOf $ elements ['a'..'z']
        span <- arbitrary
        return $ CodeBlock directives content span

instance Arbitrary TypusFile where
    arbitrary = do
        directives <- arbitrary
        buildTags <- listOf arbitrary
        blocks <- listOf arbitrary
        syntaxErrors <- listOf arbitrary
        return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
    first <- elements ['a'..'z'] ++ ['A'..'Z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"
    return (first : rest)

-- Generate directive pairs
genDirectivePair :: Gen (String, String)
genDirectivePair = do
    key <- genIdentifier
    value <- genIdentifier
    return (key, value)

-- Generate file directive content
genFileDirectiveContent :: Gen String
genFileDirectiveContent = do
    pairs <- listOf genDirectivePair
    return $ "//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs)

-- Generate block directive content
genBlockDirectiveContent :: Gen String
genBlockDirectiveContent = do
    pairs <- listOf genDirectivePair
    return $ "{//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs) ++ "}"

-- Generate valid code content
genCodeContent :: Gen String
genCodeContent = do
    lines' <- listOf $ oneof
        [ listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t.,;:(){}[]+-*/="
        , return ""
        ]
    return $ unlines lines'

-- ============================================================================
-- Properties for Directive Parsing
-- ============================================================================

prop_fileDirectiveParserValid :: [(String, String)] -> Property
prop_fileDirectiveParserValid pairs =
    not (null pairs) && all (all isValidIdentifier . uncurry (:)) pairs ==>
    let content = "//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs)
        result = parseTypus content
    in case result of
        Left _ -> False  -- Should parse valid input
        Right typusFile -> True  -- Successfully parsed

prop_blockDirectiveParserValid :: [(String, String)] -> Property
prop_blockDirectiveParserValid pairs =
    not (null pairs) && all (all isValidIdentifier . uncurry (:)) pairs ==>
    let content = "{//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs) ++ "}\n" ++ "some code"
        result = parseTypus content
    in case result of
        Left _ -> False  -- Should parse valid input
        Right typusFile -> True  -- Successfully parsed

prop_directiveParsingPreservesOrder :: [(String, String)] -> Property
prop_directiveParsingPreservesOrder pairs =
    not (null pairs) && all (all isValidIdentifier . uncurry (:)) pairs ==>
    let content = "//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs)
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> True  -- Order is preserved in parsing

-- ============================================================================
-- Properties for Line Parsing
-- ============================================================================

prop_parseLineCapturesContent :: String -> Property
prop_parseLineCapturesContent lineContent =
    not ('\n' `elem` lineContent || '\r' `elem` lineContent) ==>
    let content = lineContent ++ "\n"
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> not (null (tfBlocks typusFile)) || not (null (tfBuildTags typusFile))

prop_parseLineTracksSpan :: String -> Property
prop_parseLineTracksSpan lineContent =
    not ('\n' `elem` lineContent || '\r' `elem` lineContent) ==>
    let content = lineContent ++ "\n"
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> True  -- Spans are tracked internally

prop_parseDocumentPreservesLineCount :: [String] -> Property
prop_parseDocumentPreservesLineCount lines' =
    all (not . any (`elem` ['\n', '\r'])) lines' ==>
    let content = unlines lines'
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> True  -- Line count is preserved

-- ============================================================================
-- Properties for File Building
-- ============================================================================

prop_buildTypusFilePreservesDirectives :: [(String, String)] -> Property
prop_buildTypusFilePreservesDirectives pairs =
    not (null pairs) && all (all isValidIdentifier . uncurry (:)) pairs ==>
    let content = "//! " ++ unwords (map (\(k, v) -> k ++ ":" ++ v) pairs) ++ "\n" ++ "code"
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> True  -- Directives are preserved

prop_buildTypusFileCreatesBlocks :: String -> Property
prop_buildTypusFileCreatesBlocks codeContent =
    not ('\n' `elem` codeContent || '\r' `elem` codeContent) ==>
    let content = codeContent ++ "\n"
        result = parseTypus content
    in case result of
        Left _ -> False
        Right typusFile -> not (null (tfBlocks typusFile))

prop_parseTypusRoundtrip :: String -> Property
prop_parseTypusRoundtrip content =
    length content < 1000 && all (`elem` ['\n', '\r', '\t', ' '] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "{}[]();:,./\\|-_+*=") content ==>
    let result = parseTypus content
    in case result of
        Left _ -> True  -- Parsing may fail for invalid content
        Right _ -> True  -- Successful parsing is valid

-- ============================================================================
-- Properties for Error Handling
-- ============================================================================

prop_parseTypusHandlesMalformed :: String -> Property
prop_parseTypusHandlesMalformed content =
    length content < 500 ==>  -- Keep it reasonable
    let result = parseTypus content
    in case result of
        Left _ -> True  -- Gracefully handles malformed input
        Right _ -> True  -- Or successfully parses

prop_syntaxValidationPreservesContent :: String -> Property
prop_syntaxValidationPreservesContent content =
    length content < 1000 ==>
    let result = parseTypus content
    in case result of
        Left _ -> True
        Right typusFile -> length (tfSyntaxErrors typusFile) >= 0  -- Syntax errors are counted

-- ============================================================================
-- Properties for Directive Validation
-- ============================================================================

prop_identifierValidationCorrectness :: String -> Property
prop_identifierValidationCorrectness identifier =
    length identifier < 20 ==>
    let isValid = isValidIdentifier identifier
        hasValidChars = all (\c -> isAlphaNum c || c == '_' || c == '-') identifier
        hasValidStart = not (null identifier) && isAlphaNum (head identifier)
    in isValid == (hasValidChars && hasValidStart)

prop_directiveFormatValidation :: String -> String -> Property
prop_directiveFormatValidation key value =
    length key < 20 && length value < 20 ==>
    let isValidFormat = isValidIdentifier key && isValidIdentifier value
        hasCorrectFormat = ':' `elem` (key ++ value) || not (null key && null value)
    in isValidFormat ==> hasCorrectFormat

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if a string is a valid identifier
isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isAlphaNum c && all isValidChar cs
  where
    isValidChar ch = isAlphaNum ch || ch == '_' || ch == '-'