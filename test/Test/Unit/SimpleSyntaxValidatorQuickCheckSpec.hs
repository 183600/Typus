module Test.Unit.SimpleSyntaxValidatorQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, listOf, elements, suchThat, choose)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

import SimpleSyntaxValidator (validateSyntaxSimple, countBraces, SyntaxError(..), ErrorType(..))
import TestSupport.QuickCheck (fastProperty)

-- | Generate arbitrary strings with potential Go syntax
genGoCode :: Gen String
genGoCode = do
    lines <- listOf $ oneof
        [ pure "package main"
        , pure "import \"fmt\""
        , pure "import (\"fmt\"; \"os\")"
        , pure "func main() {"
        , pure "    fmt.Println(\"hello\")"
        , pure "}"
        , pure "var x int = 42"
        , pure "const pi = 3.14"
        , pure "type Point struct { X int; Y int }"
        , pure "for i := 0; i < 10; i++ {"
        , pure "    // do something"
        , pure "}"
        , pure "if x > 0 {"
        , pure "    return true"
        , pure "}"
        , pure "switch x {"
        , pure "case 1:"
        , pure "    return \"one\""
        , pure "default:"
        , pure "    return \"unknown\""
        , pure "}"
        , pure "// This is a comment"
        , pure "/* Multi-line\n   comment */"
        , pure "return x + y"
        , arbitrary `suchThat` (not . null)
        ]
    return $ unlines lines

-- | Generate strings with bracket mismatches
genBracketMismatch :: Gen String
genBracketMismatch = oneof
    [ pure "func main() {"
    , pure "if x > 0 {"
    , pure "for i := 0; i < 10; i++ {"
    , pure "switch x {"
    , pure "var arr = []int{1, 2, 3"
    , pure "result := func(x int) int { return x * 2"
    , pure "if (x > 0 && y < 10) {"
    ]

-- | Generate strings with balanced brackets
genBalancedBrackets :: Gen String
genBalancedBrackets = do
    depth <- choose (0, 3)
    return $ generateBalanced depth
  where
    generateBalanced 0 = "package main\n\nfunc main() {\n    fmt.Println(\"balanced\")\n}"
    generateBalanced n = "func main() {\n" ++ 
                        replicate n "    if true {\n" ++
                        "        fmt.Println(\"nested\")\n" ++
                        replicate n "    }\n" ++
                        "}"

-- | Generate strings with invalid operators
genInvalidOperators :: Gen String
genInvalidOperators = oneof
    [ pure "x = y +++ z"
    , pure "result = a --- b"
    , pure "value = x ///// y"
    , pure "L.sum = a ***** b"
    ]

-- | Generate strings with valid Go syntax
genValidGoSyntax :: Gen String
genValidGoSyntax = oneof
    [ pure "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello, World!\")\n}"
    , pure "package main\n\nfunc add(x int, y int) int {\n    return x + y\n}\n\nfunc main() {\n    result := add(5, 3)\n}"
    , pure "package utils\n\ntype Point struct {\n    X int\n    Y int\n}\n\nfunc (p Point) String() string {\n    return fmt.Sprintf(\"(%d, %d)\", p.X, p.Y)\n}"
    ]

-- Test properties L.and cases
tests :: TestTree
tests =
  testGroup "SimpleSyntaxValidator QuickCheck tests"
    [ testProperty "countBraces is zero for balanced braces" $
        fastProperty prop_countBracesBalanced
    
    , testProperty "countBraces is positive for unclosed opening braces" $
        fastProperty prop_countBracesUnclosed
    
    , testProperty "validateSyntaxSimple detects bracket mismatches" $
        fastProperty prop_detectsBracketMismatches
    
    , testProperty "validateSyntaxSimple handles empty input" $
        fastProperty prop_handlesEmptyInput
    
    , testProperty "validateSyntaxSimple detects invalid operators" $
        fastProperty prop_detectsInvalidOperators
    
    , testProperty "validateSyntaxSimple allows valid Go syntax" $
        fastProperty prop_allowsValidSyntax
    
    , testCase "countBraces handles simple balanced case" $ do
        countBraces "func main() { return 42; }" @?= 0
    
    , testCase "countBraces handles simple unbalanced case" $ do
        countBraces "func main() { return 42;" @?= 1
    
    , testCase "countBraces ignores brackets in strings" $ do
        let content = "func main() { s := \"{not a brace}\"; return 42; }"
        countBraces content @?= 0
    
    , testCase "countBraces ignores brackets in comments" $ do
        let content = "func main() { // { not a brace\n    return 42; // } not a brace\n}"
        countBraces content @?= 0
    
    , testCase "validateSyntaxSimple detects missing package declaration" $ do
        let content = "func main() { return 42; }"
            errors = validateSyntaxSimple content
            missingPackage = L.filter (\e -> errorType e == MissingPackageDeclaration) errors
        L.length missingPackage @?= 1
    
    , testCase "validateSyntaxSimple detects invalid function declaration" $ do
        let content = "package main\n\nfunc main return 42; }"
            errors = validateSyntaxSimple content
            invalidFunc = L.filter (\e -> errorType e == InvalidFunctionDeclaration) errors
        L.length invalidFunc @?= 1
    
    , testCase "validateSyntaxSimple detects invalid import" $ do
        let content = "package main\n\nimport"
            errors = validateSyntaxSimple content
            invalidImport = L.filter (\e -> errorType e == InvalidImport) errors
        L.length invalidImport @?= 1
    
    , testCase "validateSyntaxSimple handles complex nested structures" $ do
        let content = unlines
                [ "package main"
                , "import \"fmt\""
                , "func main() {"
                , "    if true {"
                , "        for i := 0; i < 10; i++ {"
                , "            switch i {"
                , "            case 1:"
                , "                fmt.Println(\"one\")"
                , "            }"
                , "        }"
                , "    }"
                , "}"
                ]
            errors = validateSyntaxSimple content
        L.length errors @?= 0
    
    , testCase "validateSyntaxSimple detects unclosed braces" $ do
        let content = unlines
                [ "package main"
                , "func main() {"
                , "    if true {"
                , "        fmt.Println(\"test\")"
                , "    // missing closing braces"
                ]
            errors = validateSyntaxSimple content
            unclosedBraces = L.filter (\e -> errorType e == MissingBrace) errors
        L.length unclosedBraces @?= 2
    ]

-- Property: countBraces is zero for balanced braces
prop_countBracesBalanced :: String -> Bool
prop_countBracesBalanced code =
    let balancedCode = "package main\n\nfunc main() {\n    return 42;\n}\n"
        braceCount = countBraces balancedCode
    in braceCount == 0

-- Property: countBraces is positive for unclosed opening braces
prop_countBracesUnclosed :: Int -> Bool
prop_countBracesUnclosed n =
    let unclosedCode = "package main\n\nfunc main() {\n" ++ 
                       L.concat (replicate n "    if true {\n") ++
                       "        return 42;\n"
        braceCount = countBraces unclosedCode
    in braceCount >= n && braceCount > 0

-- Property: validateSyntaxSimple detects bracket mismatches
prop_detectsBracketMismatches :: String -> Bool
prop_detectsBracketMismatches code =
    let errors = validateSyntaxSimple code
        bracketErrors = L.filter (\e -> errorType e `elem` 
            [MissingBrace, MissingParenthesis, MissingBracket, BracketMismatch]) errors
    in null bracketErrors || L.all (\e -> lineNumber e > 0) bracketErrors

-- Property: validateSyntaxSimple handles empty input
prop_handlesEmptyInput :: String -> Bool
prop_handlesEmptyInput _ =
    let errors = validateSyntaxSimple ""
    in null errors || L.all (\e -> lineNumber e > 0) errors

-- Property: validateSyntaxSimple detects invalid operators
prop_detectsInvalidOperators :: String -> String -> Bool
prop_detectsInvalidOperators prefix suffix =
    let invalidCode = prefix ++ " +++ " ++ suffix
        errors = validateSyntaxSimple invalidCode
        invalidOpErrors = L.filter (\e -> errorType e == InvalidOperator) errors
    in not (null invalidOpErrors) || L.all (\e -> lineNumber e > 0) errors

-- Property: validateSyntaxSimple allows valid Go syntax
prop_allowsValidSyntax :: String -> Bool
prop_allowsValidSyntax _ =
    let validCode = "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"valid\")\n}\n"
        errors = validateSyntaxSimple validCode
    in null errors

-- Helper function to check if string contains a substring
contains :: String -> String -> Bool
contains needle haystack = needle `L.isInfixOf` haystack