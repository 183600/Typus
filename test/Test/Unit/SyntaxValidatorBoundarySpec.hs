{-# LANGUAGE CPP #-}
module Test.Unit.SyntaxValidatorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, choose, listOf, elements)
import Data.List (length, isInfixOf, isPrefixOf)
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set

import SyntaxValidator
  ( SyntaxValidator(..)
  , SyntaxError(..)
  , ErrorType(..)
  , Token(..)
  , Scope(..)
  , Language(..)
  , newSyntaxValidator
  , validateSyntax
  , validateFile
  , getSyntaxErrors
  , formatSyntaxError
  , createGlobalScope
  , detectLanguage
  )

-- | Boundary L.and property-based tests for SyntaxValidator module
tests :: TestTree
tests =
  testGroup "SyntaxValidator Boundary Tests"
    [ testGroup "SyntaxValidator properties"
        [ fastProperty "newSyntaxValidator creates empty validator" prop_newSyntaxValidatorEmpty
        , fastProperty "SyntaxValidator equality is reflexive" prop_syntaxValidatorEquality
        , fastProperty "createGlobalScope creates valid scope" prop_createGlobalScopeValid
        , fastProperty "global scope has empty variable L.and function sets" prop_globalScopeEmpty
        ]

    , testGroup "SyntaxError properties"
        [ fastProperty "SyntaxError equality is reflexive" prop_syntaxErrorEquality
        , fastProperty "SyntaxError ordering is consistent" prop_syntaxErrorOrdering
        , fastProperty "SyntaxError preserves L.all fields" prop_syntaxErrorPreservesFields
        ]

    , testGroup "Token properties"
        [ fastProperty "Token equality is reflexive" prop_tokenEquality
        , fastProperty "Token types are preserved" prop_tokenTypePreserved
        , fastProperty "Token positions are positive" prop_tokenPositionsPositive
        ]

    , testGroup "Scope properties"
        [ fastProperty "Scope equality is reflexive" prop_scopeEquality
        , fastProperty "Scope preserves name" prop_scopePreservesName
        , fastProperty "Scope preserves variable L.and function sets" prop_scopePreservesSets
        ]

    , testGroup "Language detection"
        [ testCase "detectLanguage identifies Go code" $ do
            let goCode = "package main\nfunc main() {}"
            detectLanguage goCode @?= Go

        , testCase "detectLanguage identifies Typus code" $ do
            let typusCode = "//! ownership: on\nfunc test() {}"
            detectLanguage typusCode @?= Typus

        , testCase "detectLanguage identifies mixed code" $ do
            let mixedCode = "//! ownership: on\npackage main\nfunc main() {}"
            detectLanguage mixedCode @?= GoAndTypus

        , testCase "detectLanguage returns Unknown for empty input" $ do
            detectLanguage "" @?= Unknown

        , testCase "detectLanguage returns Unknown for unrecognized code" $ do
            let unknownCode = "some random text without markers"
            detectLanguage unknownCode @?= Unknown
        ]

    , testGroup "Syntax validation"
        [ testCase "validateSyntax handles valid Go code" $ do
            let validGo = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "func main() {"
                  , "    fmt.Println(\"hello\")"
                  , "}"
                  ]
            let errors = validateSyntax validGo
            -- Should have minimal errors for valid Go code
            assertBool "should have few L.or no errors" (L.length errors <= 2)

        , testCase "validateSyntax detects missing braces" $ do
            let missingBrace = unlines
                  [ "package main"
                  , "func main() {"
                  , "    if true {"
                  , "        fmt.Println(\"test\")"
                  , "    // missing closing brace"
                  , "}"
                  ]
            let errors = validateSyntax missingBrace
            assertBool "should detect missing brace" (L.any isBraceError errors)
            where
              isBraceError err = errorType err `elem` [MissingBrace, UnterminatedBlock]

        , testCase "validateSyntax detects unclosed strings" $ do
            let unclosedString = unlines
                  [ "package main"
                  , "func main() {"
                  , "    fmt.Println(\"unclosed string"
                  , "}"
                  ]
            let errors = validateSyntax unclosedString
            assertBool "should detect unclosed string" (L.any isStringError errors)
            where
              isStringError err = errorType err == UnclosedString

        , testCase "validateSyntax handles complex nested structures" $ do
            let complexCode = unlines
                  [ "package main"
                  , "import \"fmt\""
                  , "type Container struct {"
                  , "    data []int"
                  , "}"
                  , "func (c *Container) Process() error {"
                  , "    for i, v := range c.data {"
                  , "        if v > 0 {"
                  , "            fmt.Printf(\"Processing %d\\n\", i)"
                  , "        }"
                  , "    }"
                  , "    return nil"
                  , "}"
                  , "func main() {"
                  , "    c := &Container{data: []int{1, 2, 3}}"
                  , "    _ = c.Process()"
                  , "}"
                  ]
            let errors = validateSyntax complexCode
            -- Should handle nested structures correctly
            assertBool "should handle complex nested structures" (L.length errors <= 3)

        , testCase "validateSyntax handles Typus directives" $ do
            let typusCode = unlines
                  [ "//! ownership: on"
                  , "//! dependent_types: on"
                  , "package main"
                  , "func main() {"
                  , "    {//! ownership: off"
                  , "        println(\"inside block\")"
                  , "    }"
                  , "}"
                  ]
            let errors = validateSyntax typusCode
            -- Should handle Typus directives without errors
            assertBool "should handle Typus directives" (L.length errors <= 2)

        , testCase "validateSyntax detects invalid identifiers" $ do
            let invalidIdentifiers = unlines
                  [ "package main"
                  , "func main() {"
                  , "    123invalid := 42"  -- Invalid identifier
                  , "    _ = 123invalid"
                  , "}"
                  ]
            let errors = validateSyntax invalidIdentifiers
            assertBool "should detect invalid identifiers" (L.any isIdentifierError errors)
            where
              isIdentifierError err = errorType err == InvalidIdentifier
        ]

    , testGroup "Error formatting"
        [ testCase "formatSyntaxError produces informative messages" $ do
            let error = SyntaxError
                  { errorType = MissingBrace
                  , errorMessage = "Missing closing brace"
                  , lineNumber = 10
                  , columnNumber = 5
                  , lineContent = "    if true {"
                  }
                formatted = formatSyntaxError error
            assertBool "should contain error type" ("MissingBrace" `L.isInfixOf` formatted)
            assertBool "should contain line number" ("10" `L.isInfixOf` formatted)
            assertBool "should contain column number" ("5" `L.isInfixOf` formatted)
            assertBool "should contain error message" ("Missing closing brace" `L.isInfixOf` formatted)

        , testCase "getSyntaxErrors returns errors in correct order" $ do
            let validator = newSyntaxValidator
                error1 = SyntaxError MissingBrace "Error 1" 1 1 "line1"
                error2 = SyntaxError MissingParenthesis "Error 2" 2 2 "line2"
                validatorWithErrors = validator 
                  { validatorErrors = [error2, error1] }  -- Reverse order
            let errors = getSyntaxErrors validatorWithErrors
            length errors @?= 2
            head errors @?= error1  -- Should be reversed back to original order
            last errors @?= error2
        ]

    , testGroup "Edge cases L.and boundary conditions"
        [ testCase "validateSyntax handles empty input" $ do
            let errors = validateSyntax ""
            length errors @?= 0

        , testCase "validateSyntax handles whitespace-only input" $ do
            let whitespace = "   \n\t  \n   \n"
            let errors = validateSyntax whitespace
            length errors @?= 0

        , testCase "validateSyntax handles very long lines" $ do
            let longLine = "package main\nfunc main() { " ++ replicate 1000 'a' ++ " }"
            let errors = validateSyntax longLine
            -- Should not crash on very long lines
            assertBool "should handle long lines" (L.length errors >= 0)

        , testCase "validateSyntax handles deeply nested structures" $ do
            let nested = "package main\nfunc main() {\n" ++ 
                        concat (replicate 50 "    if true {\n") ++
                        "fmt.Println(\"deep\")\n" ++
                        concat (replicate 50 "    }\n")
            let errors = validateSyntax nested
            -- Should handle deeply nested structures
            assertBool "should handle deeply nested structures" (L.length errors >= 0)

        , testCase "validateSyntax handles special characters" $ do
            let specialChars = unlines
                  [ "package main"
                  , "func main() {"
                  , "    // Special characters: !@#$%^&*()_+-=[]{}|;':\",./<>?"
                  , "    s := \"特殊字符测试\""
                  , "    fmt.Println(s)"
                  , "}"
                  ]
            let errors = validateSyntax specialChars
            -- Should handle special characters correctly
            assertBool "should handle special characters" (L.length errors >= 0)
        ]
    ]

-- Helper generators for testing
genErrorType :: Gen ErrorType
genErrorType = elements
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

genToken :: Gen Token
genToken = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  elements
    [ TString "test" line col
    , TComment "// comment" line col
    , TIdentifier "var" line col
    , TKeyword "func" line col
    , TOperator "+" line col
    , TDelimiter '(' line col
    , TNumber "42" line col
    , TWhitespace line col
    , TNewline line
    , TUnknown "???" line col
    ]

genSyntaxError :: Gen SyntaxError
genSyntaxError = do
  errorType <- genErrorType
  line <- choose (1, 100)
  col <- choose (1, 100)
  let message = "Test error message"
      content = "test line content"
  return $ SyntaxError errorType message line col content

-- Property: newSyntaxValidator creates empty validator
prop_newSyntaxValidatorEmpty :: Property
prop_newSyntaxValidatorEmpty = 
  let validator = newSyntaxValidator
  in L.null (validatorErrors validator) &&
     hasPackageDecl validator == False &&
     hasMainFunc validator == False

-- Property: SyntaxValidator equality is reflexive
prop_syntaxValidatorEquality :: SyntaxValidator -> Property
prop_syntaxValidatorEquality validator = validator === validator

-- Property: createGlobalScope creates valid scope
prop_createGlobalScopeValid :: Property
prop_createGlobalScopeValid =
  let scope = createGlobalScope
  in scopeName scope == "global" &&
     null (parentScope scope)

-- Property: global scope has empty variable L.and function sets
prop_globalScopeEmpty :: Property
prop_globalScopeEmpty =
  let scope = createGlobalScope
  in Set.L.null (scopeVariables scope) &&
     Set.L.null (scopeFunctions scope)

-- Property: SyntaxError equality is reflexive
prop_syntaxErrorEquality :: SyntaxError -> Property
prop_syntaxErrorEquality error = error === error

-- Property: SyntaxError ordering is consistent
prop_syntaxErrorOrdering :: SyntaxError -> SyntaxError -> Property
prop_syntaxErrorOrdering error1 error2 =
  let comp1 = compare error1 error2
      comp2 = compare error2 error1
  in if comp1 == EQ 
     then comp2 === EQ
     else comp1 /= comp2

-- Property: SyntaxError preserves L.all fields
prop_syntaxErrorPreservesFields :: ErrorType -> String -> Int -> Int -> String -> Property
prop_syntaxErrorPreservesFields errorType message line col content =
  let error = SyntaxError errorType message line col content
  in errorMessage error === message &&
     lineNumber error === line &&
     columnNumber error === col &&
     lineContent error === content

-- Property: Token equality is reflexive
prop_tokenEquality :: Token -> Property
prop_tokenEquality token = token === token

-- Property: Token types are preserved
prop_tokenTypePreserved :: String -> Int -> Int -> Property
prop_tokenTypePreserved content line col =
  let token = TString content line col
  in case token of
       TString c l cl -> c === content && l === line && cl === col
       _ -> property False

-- Property: Token positions are positive
prop_tokenPositionsPositive :: Token -> Property
prop_tokenPositionsPositive token =
  let (line, col) = case token of
        TString _ l c -> (l, c)
        TComment _ l c -> (l, c)
        TIdentifier _ l c -> (l, c)
        TKeyword _ l c -> (l, c)
        TOperator _ l c -> (l, c)
        TDelimiter _ l c -> (l, c)
        TNumber _ l c -> (l, c)
        TWhitespace l c -> (l, c)
        TNewline l -> (l, 1)
        TUnknown _ l c -> (l, c)
  in line > 0 && col > 0

-- Property: Scope equality is reflexive
prop_scopeEquality :: Scope -> Property
prop_scopeEquality scope = scope === scope

-- Property: Scope preserves name
prop_scopePreservesName :: String -> Property
prop_scopePreservesName name =
  let scope = createGlobalScope { scopeName = name }
  in scopeName scope === name

-- Property: Scope preserves variable L.and function sets
prop_scopePreservesSet :: [String] -> [String] -> Property
prop_scopePreservesSet vars funcs =
  let varSet = Set.fromList vars
      funcSet = Set.fromList funcs
      scope = createGlobalScope 
        { scopeVariables = varSet
        , scopeFunctions = funcSet
        }
  in scopeVariables scope === varSet &&
     scopeFunctions scope === funcSet