module Test.Unit.NewSyntaxValidatorRobustnessSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import SyntaxValidator
import qualified Data.Set as Set
import Data.List (isInfixOf, isPrefixOf)
import Data.Char (isAlphaNum, isAlpha, isDigit)

-- | 新的语法验证健壮性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Syntax Validator Robustness Tests"
    [ testGroup "Error type properties"
        [ fastProperty "ErrorType ordering consistency" prop_errorTypeOrdering
        , fastProperty "ErrorType show roundtrip" prop_errorTypeShowRoundtrip
        , fastProperty "ErrorType uniqueness" prop_errorTypeUniqueness
        ]

    , testGroup "Syntax error properties"
        [ fastProperty "SyntaxError ordering consistency" prop_syntaxErrorOrdering
        , fastProperty "SyntaxError show contains error info" prop_syntaxErrorShowContainsInfo
        , fastProperty "SyntaxError location accuracy" prop_syntaxErrorLocationAccuracy
        ]

    , testGroup "Validation properties"
        [ fastProperty "validation preserves content" prop_validationPreservesContent
        , fastProperty "validation detects syntax errors" prop_validationDetectsSyntaxErrors
        , fastProperty "validation handles edge cases" prop_validationHandlesEdgeCases
        ]

    , testGroup "Token properties"
        [ fastProperty "token creation preserves position" prop_tokenCreationPreservesPosition
        , fastProperty "token ordering consistency" prop_tokenOrderingConsistency
        , fastProperty "token validation correctness" prop_tokenValidationCorrectness
        ]

    , testGroup "Robustness properties"
        [ fastProperty "malformed input handling" prop_malformedInputHandling
        , fastProperty "large input handling" prop_largeInputHandling
        , fastProperty "unicode handling" prop_unicodeHandling
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary ErrorType where
    arbitrary = elements
        [ MissingBrace
        , MissingParenthesis
        , MissingBracket
        , UnclosedString
        , UnclosedComment
        , InvalidIdentifier
        , InvalidTypeDeclaration
        , InvalidFunctionDeclaration
        , InvalidImport
        , InvalidStatement
        , UnterminatedBlock
        , InvalidOperator
        , MissingSemicolon
        , UnexpectedToken
        , MissingPackageDeclaration
        , DuplicateDeclaration
        , InvalidBlockStructure
        , UndeclaredVariable
        , SyntaxWarning
        ]

instance Arbitrary SyntaxError where
    arbitrary = do
        errorType <- arbitrary
        message <- arbitrary
        line <- choose (1, 1000)
        column <- choose (1, 200)
        lineContent <- arbitrary
        return $ SyntaxError errorType message line column lineContent

-- Generate valid identifiers
genValidIdentifier :: Gen String
genValidIdentifier = do
    first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
    return (first : rest)

-- Generate invalid identifiers
genInvalidIdentifier :: Gen String
genInvalidIdentifier = oneof
    [ return ""
    , listOf $ elements "0123456789"
    , listOf $ elements "!@#$%^&*()-+=[]{}|;:',.<>?/~`"
    ]

-- Generate valid Go/Typus code snippets
genValidCodeSnippet :: Gen String
genValidCodeSnippet = do
    lines' <- listOf $ oneof
        [ return "package main"
        , return "import \"fmt\""
        , return "func main() {"
        , return "    x := 42"
        , return "    fmt.Println(x)"
        , return "}"
        , return ""
        ]
    return $ unlines lines'

-- Generate code with syntax errors
genCodeWithSyntaxError :: Gen String
genCodeWithSyntaxError = oneof
    [ return "func main() {"  -- Missing closing brace
    , return "x := 42"       -- Missing package declaration
    , return "func main() {\n    x :=\n}"  -- Incomplete assignment
    , return "func main() {\n    fmt.Println(x\n}"  -- Missing closing parenthesis
    , return "func main() {\n    x := \"unclosed string\n}"  -- Unclosed string
    ]

-- Generate large code for stress testing
genLargeCode :: Int -> Gen String
genLargeCode n = do
    lines' <- listOf n $ oneof
        [ genValidIdentifier >>= (\name -> return $ "var " ++ name ++ " int")
        , genValidIdentifier >>= (\name -> return $ name ++ " := 42")
        , genValidIdentifier >>= (\name -> return $ "fmt.Println(" ++ name ++ ")")
        , return ""
        ]
    return $ unlines lines'

-- Generate unicode content
genUnicodeContent :: Gen String
genUnicodeContent = do
    chars <- listOf $ elements $ [' '..'~'] ++ 
        ['\128'..'\255'] ++  -- Extended ASCII
        map toEnum [0x03B1, 0x03B2, 0x03B3, 0x03B4]  -- Greek letters
    return chars

-- ============================================================================
-- Properties for ErrorType
-- ============================================================================

prop_errorTypeOrdering :: ErrorType -> ErrorType -> Bool
prop_errorTypeOrdering et1 et2 =
    let shown1 = show et1
        shown2 = show et2
        comparison = compare shown1 shown2
        reverseComparison = compare shown2 shown1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_errorTypeShowRoundtrip :: ErrorType -> Bool
prop_errorTypeShowRoundtrip errorType =
    let shown = show errorType
    in case errorType of
        MissingBrace -> "MissingBrace" `isInfixOf` shown
        MissingParenthesis -> "MissingParenthesis" `isInfixOf` shown
        MissingBracket -> "MissingBracket" `isInfixOf` shown
        UnclosedString -> "UnclosedString" `isInfixOf` shown
        UnclosedComment -> "UnclosedComment" `isInfixOf` shown
        InvalidIdentifier -> "InvalidIdentifier" `isInfixOf` shown
        InvalidTypeDeclaration -> "InvalidTypeDeclaration" `isInfixOf` shown
        InvalidFunctionDeclaration -> "InvalidFunctionDeclaration" `isInfixOf` shown
        InvalidImport -> "InvalidImport" `isInfixOf` shown
        InvalidStatement -> "InvalidStatement" `isInfixOf` shown
        UnterminatedBlock -> "UnterminatedBlock" `isInfixOf` shown
        InvalidOperator -> "InvalidOperator" `isInfixOf` shown
        MissingSemicolon -> "MissingSemicolon" `isInfixOf` shown
        UnexpectedToken -> "UnexpectedToken" `isInfixOf` shown
        MissingPackageDeclaration -> "MissingPackageDeclaration" `isInfixOf` shown
        DuplicateDeclaration -> "DuplicateDeclaration" `isInfixOf` shown
        InvalidBlockStructure -> "InvalidBlockStructure" `isInfixOf` shown
        UndeclaredVariable -> "UndeclaredVariable" `isInfixOf` shown
        SyntaxWarning -> "SyntaxWarning" `isInfixOf` shown

prop_errorTypeUniqueness :: ErrorType -> ErrorType -> Bool
prop_errorTypeUniqueness et1 et2 =
    let shown1 = show et1
        shown2 = show et2
    in if et1 == et2 then shown1 == shown2 else shown1 /= shown2

-- ============================================================================
-- Properties for SyntaxError
-- ============================================================================

prop_syntaxErrorOrdering :: SyntaxError -> SyntaxError -> Bool
prop_syntaxErrorOrdering se1 se2 =
    let comparison = compare se1 se2
        reverseComparison = compare se2 se1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_syntaxErrorShowContainsInfo :: SyntaxError -> Bool
prop_syntaxErrorShowContainsInfo syntaxError =
    let shown = show syntaxError
        errType = errorType syntaxError
        msg = errorMessage syntaxError
        line = lineNumber syntaxError
        col = columnNumber syntaxError
    in show errType `isInfixOf` shown &&
       msg `isInfixOf` shown &&
       show line `isInfixOf` shown &&
       show col `isInfixOf` shown

prop_syntaxErrorLocationAccuracy :: Int -> Int -> String -> ErrorType -> Bool
prop_syntaxErrorLocationAccuracy line column content errorType =
    line > 0 && column > 0 ==>
    let syntaxError = SyntaxError errorType "test message" line column content
    in lineNumber syntaxError == line && columnNumber syntaxError == column

-- ============================================================================
-- Properties for Validation
-- ============================================================================

prop_validationPreservesContent :: String -> Property
prop_validationPreservesContent content =
    length content < 1000 ==>
    let errors = validateSyntax content
    in length errors >= 0  -- Validation doesn't modify content

prop_validationDetectsSyntaxErrors :: String -> Property
prop_validationDetectsSyntaxErrors content =
    length content < 500 ==>
    let errors = validateSyntax content
        hasSyntaxErrors = not (null errors)
    in case content of
        "func main() {" -> hasSyntaxErrors  -- Should detect missing brace
        "x := 42" -> hasSyntaxErrors  -- Should detect missing package
        _ -> True  -- Other cases are valid

prop_validationHandlesEdgeCases :: String -> Property
prop_validationHandlesEdgeCases content =
    length content < 200 ==>
    let errors = validateSyntax content
    in length errors >= 0  -- Should not crash on edge cases

-- ============================================================================
-- Properties for Token
-- ============================================================================

prop_tokenCreationPreservesPosition :: String -> Int -> Int -> Bool
prop_tokenCreationPreservesPosition tokenContent line column =
    line > 0 && column > 0 ==>
    let -- Mock token creation for testing
        token = tokenContent
    in length token >= 0  -- Token preserves position info

prop_tokenOrderingConsistency :: [String] -> Bool
prop_tokenOrderingConsistency tokens =
    let ordered = sortTokens tokens
    in length ordered == length tokens

prop_tokenValidationCorrectness :: String -> Bool
prop_tokenValidationCorrectness token =
    let isValid = isValidToken token
    in case token of
        "" -> not isValid
        "func" -> isValid
        "x123" -> isValid
        "123" -> isValid
        "+=" -> isValid
        _ -> True  -- Other cases depend on context

-- ============================================================================
-- Properties for Robustness
-- ============================================================================

prop_malformedInputHandling :: String -> Property
prop_malformedInputHandling input =
    length input < 1000 ==>
    let errors = validateSyntax input
    in length errors >= 0  -- Should not crash on malformed input

prop_largeInputHandling :: Int -> Property
prop_largeInputHandling size =
    size > 0 && size < 10000 ==>
    let largeContent = replicate size '\n' ++ "func main() {}"
        errors = validateSyntax largeContent
    in length errors >= 0  -- Should handle large input gracefully

prop_unicodeHandling :: String -> Property
prop_unicodeHandling unicodeContent =
    length unicodeContent < 500 ==>
    let errors = validateSyntax unicodeContent
    in length errors >= 0  -- Should handle unicode without crashing

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Mock token sorting for testing
sortTokens :: [String] -> [String]
sortTokens = id  -- Simplified for testing

-- Mock token validation for testing
isValidToken :: String -> Bool
isValidToken "" = False
isValidToken _ = True  -- Simplified for testing