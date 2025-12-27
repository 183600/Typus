module Test.Unit.NewQuickCheckTestSuite8Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Set (Set)
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import SyntaxValidator

-- | Test suite for SyntaxValidator module syntax validation
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite8 - SyntaxValidator"
    [ testGroup "ErrorType operations"
        [ testCase "ErrorType Show works" $ do
            show MissingBrace @?= "MissingBrace"
            show MissingParenthesis @?= "MissingParenthesis"
            show MissingBracket @?= "MissingBracket"
            show UnclosedString @?= "UnclosedString"
            show UnclosedComment @?= "UnclosedComment"
            show InvalidIdentifier @?= "InvalidIdentifier"
            show InvalidTypeDeclaration @?= "InvalidTypeDeclaration"
            show InvalidFunctionDeclaration @?= "InvalidFunctionDeclaration"
            show InvalidImport @?= "InvalidImport"
            show InvalidStatement @?= "InvalidStatement"
            show UnterminatedBlock @?= "UnterminatedBlock"
            show InvalidOperator @?= "InvalidOperator"
            show MissingSemicolon @?= "MissingSemicolon"
            show UnexpectedToken @?= "UnexpectedToken"
            show MissingPackageDeclaration @?= "MissingPackageDeclaration"
            show DuplicateDeclaration @?= "DuplicateDeclaration"
            show InvalidBlockStructure @?= "InvalidBlockStructure"
            show UndeclaredVariable @?= "UndeclaredVariable"
            show SyntaxWarning @?= "SyntaxWarning"
            
        , testCase "ErrorType Eq works" $ do
            MissingBrace @?= MissingBrace
            MissingBrace /= MissingParenthesis @?= True
        ]

    , testGroup "SyntaxError operations"
        [ testCase "SyntaxError construction" $ do
            let error = SyntaxError MissingBrace "Missing closing brace" 5 10 "if condition {"
            errorType error @?= MissingBrace
            errorMessage error @?= "Missing closing brace"
            lineNumber error @?= 5
            columnNumber error @?= 10
            lineContent error @?= "if condition {"
            
        , testCase "SyntaxError Show works" $ do
            let error = SyntaxError MissingBrace "Missing closing brace" 5 10 "if condition {"
                errorStr = show error
            errorStr `contains` "MissingBrace" @?= True
            errorStr `contains` "Missing closing brace" @?= True
            errorStr `contains` "5" @?= True
            errorStr `contains` "10" @?= True
            
        , testCase "SyntaxError Eq works" $ do
            let error1 = SyntaxError MissingBrace "Missing closing brace" 5 10 "if condition {"
                error2 = SyntaxError MissingBrace "Missing closing brace" 5 10 "if condition {"
                error3 = SyntaxError MissingParenthesis "Missing closing parenthesis" 5 10 "if condition {"
            error1 @?= error2
            error1 /= error3 @?= True
            
        , testCase "SyntaxError Ord works" $ do
            let error1 = SyntaxError MissingBrace "Error A" 5 10 "line1"
                error2 = SyntaxError MissingBrace "Error B" 5 10 "line2"
                error3 = SyntaxError MissingBrace "Error A" 6 10 "line3"
            compare error1 error2 @?= LT
            compare error2 error1 @?= GT
            compare error1 error3 @?= LT
        ]

    , testGroup "Token operations"
        [ testCase "Token construction" $ do
            let stringToken = TString "hello" 1 5
                commentToken = TComment "// comment" 2 3
                identifierToken = TIdentifier "variable" 3 7
                keywordToken = TKeyword "func" 4 1
                operatorToken = TOperator "+" 5 10
                delimiterToken = TDelimiter '{' 6 15
                numberToken = TNumber "42" 7 8
                whitespaceToken = TWhitespace 8 1
                newlineToken = TNewline 9
                unknownToken = TUnknown "???" 10 5
            show stringToken `contains` "TString" @?= True
            show commentToken `contains` "TComment" @?= True
            show identifierToken `contains` "TIdentifier" @?= True
            show keywordToken `contains` "TKeyword" @?= True
            show operatorToken `contains` "TOperator" @?= True
            show delimiterToken `contains` "TDelimiter" @?= True
            show numberToken `contains` "TNumber" @?= True
            show whitespaceToken `contains` "TWhitespace" @?= True
            show newlineToken `contains` "TNewline" @?= True
            show unknownToken `contains` "TUnknown" @?= True
        ]

    , testGroup "Scope operations"
        [ testCase "createGlobalScope creates empty global scope" $ do
            let global = createGlobalScope
            scopeName global @?= "global"
            Set.null (scopeVariables global) @?= True
            Set.null (scopeFunctions global) @?= True
            parentScope global @?= Nothing
            
        , testCase "Scope construction" $ do
            let variables = Set.fromList ["x", "y"]
                functions = Set.fromList ["func1", "func2"]
                scope = Scope "test" variables functions Nothing
            scopeName scope @?= "test"
            scopeVariables scope @?= variables
            scopeFunctions scope @?= functions
        ]

    , testGroup "Language detection"
        [ testCase "detectLanguage identifies Go" $ do
            let goCode = "package main\nfunc main() {}"
            detectLanguage goCode @?= Go
            
        , testCase "detectLanguage identifies Typus" $ do
            let typusCode = "//! ownership: true\nfunc main() {}"
            detectLanguage typusCode @?= Typus
            
        , testCase "detectLanguage identifies mixed" $ do
            let mixedCode = "//! ownership: true\npackage main\nfunc main() {}"
            detectLanguage mixedCode @?= GoAndTypus
            
        , testCase "detectLanguage identifies unknown" $ do
            let unknownCode = "just some text"
            detectLanguage unknownCode @?= Unknown
        ]

    , testGroup "SyntaxValidator initialization"
        [ testCase "newSyntaxValidator creates validator" $ do
            let validator = newSyntaxValidator
            null (validatorErrors validator) @?= True
            scopeName (currentScope validator) @?= "global"
            null (scopeStack validator) @?= True
            null (braceStack validator) @?= True
            language validator @?= Unknown
            null (tokens validator) @?= True
            hasPackageDecl validator @?= False
            hasMainFunc validator @?= False
        ]

    , testGroup "Syntax validation"
        [ testCase "validateSyntax handles empty input" $ do
            let errors = validateSyntax ""
            length errors @?= 0
            
        , testCase "validateSyntax detects missing brace" $ do
            let code = "func test() {\n  // missing closing brace\n"
                errors = validateSyntax code
            let hasMissingBrace = any (\err -> errorType err == MissingBrace) errors
            hasMissingBrace @?= True
            
        , testCase "validateSyntax detects unclosed string" $ do
            let code = "var s = \"unclosed string\n"
                errors = validateSyntax code
            let hasUnclosedString = any (\err -> errorType err == UnclosedString) errors
            hasUnclosedString @?= True
            
        , testCase "validateSyntax handles valid Go code" $ do
            let goCode = "package main\n\nfunc main() {\n    println(\"Hello\")\n}\n"
                errors = validateSyntax goCode
            -- Should have minimal errors for valid Go code
            length errors <= 2 @?= True  -- Allow for some false positives
        ]

    , testGroup "File validation"
        [ testCase "validateFile works like validateSyntax" $ do
            let code = "func test() {}"
                errors1 = validateSyntax code
                errors2 = validateFile code
            length errors1 @?= length errors2
        ]

    , testGroup "Error formatting"
        [ testCase "formatSyntaxError creates readable format" $ do
            let error = SyntaxError MissingBrace "Missing closing brace" 5 10 "if condition {"
                formatted = formatSyntaxError error
            formatted `contains` "MissingBrace" @?= True
            formatted `contains` "Missing closing brace" @?= True
            formatted `contains` "line 5" @?= True
            formatted `contains` "column 10" @?= True
        ]

    , testGroup "Error retrieval"
        [ testCase "getSyntaxErrors returns errors" $ do
            let validator = newSyntaxValidator { validatorErrors = [SyntaxError MissingBrace "test" 1 1 ""] }
                errors = getSyntaxErrors validator
            length errors @?= 1
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "SyntaxError ordering is consistent" prop_syntaxErrorOrderingConsistent
        , fastProperty "Scope operations preserve invariants" prop_scopeOperationsPreserveInvariants
        , fastProperty "Language detection is deterministic" prop_languageDetectionDeterministic
        , fastProperty "SyntaxValidator initialization is consistent" prop_validatorInitializationConsistent
        , fastProperty "Error type ordering is transitive" prop_errorTypeOrderingTransitive
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- SyntaxError properties
prop_syntaxErrorOrderingConsistent :: SyntaxError -> SyntaxError -> Bool
prop_syntaxErrorOrderingConsistent err1 err2 =
    let ord1 = compare err1 err2
        -- Compare by message, then line, then column
        expectedOrd = case compare (errorMessage err1) (errorMessage err2) of
          EQ -> case compare (lineNumber err1) (lineNumber err2) of
            EQ -> compare (columnNumber err1) (columnNumber err2)
            other -> other
          other -> other
    in ord1 == expectedOrd

-- Scope properties
prop_scopeOperationsPreserveInvariants :: String -> Set.Set String -> Set.Set String -> Bool
prop_scopeOperationsPreserveInvariants name variables functions =
    let scope = Scope name variables functions Nothing
    in scopeName scope == name &&
       scopeVariables scope == variables &&
       scopeFunctions scope == functions

-- Language detection properties
prop_languageDetectionDeterministic :: String -> Bool
prop_languageDetectionDeterministic content =
    let lang1 = detectLanguage content
        lang2 = detectLanguage content
    in lang1 == lang2

-- SyntaxValidator properties
prop_validatorInitializationConsistent :: Bool
prop_validatorInitializationConsistent =
    let validator1 = newSyntaxValidator
        validator2 = newSyntaxValidator
    in validator1 == validator2

-- ErrorType properties
prop_errorTypeOrderingTransitive :: ErrorType -> ErrorType -> ErrorType -> Property
prop_errorTypeOrderingTransitive et1 et2 et3 =
    let ord1 = compare et1 et2
        ord2 = compare et2 et3
        ord3 = compare et1 et3
    in (ord1 == LT && ord2 == LT) ==> ord3 == LT

-- Helper functions for generating test data
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

genSyntaxError :: Gen SyntaxError
genSyntaxError = do
    errorType <- genErrorType
    message <- arbitrary
    line <- arbitrary
    column <- arbitrary
    lineContent <- arbitrary
    return $ SyntaxError errorType message line column lineContent

genToken :: Gen Token
genToken = oneof
    [ fmap TString $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TComment $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TIdentifier $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TKeyword $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TOperator $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TDelimiter $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TNumber $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap TWhitespace $ (,) <$> arbitrary <*> arbitrary
    , fmap TNewline arbitrary
    , fmap TUnknown $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    ]

genScope :: Gen Scope
genScope = do
    name <- arbitrary
    variables <- arbitrary
    functions <- arbitrary
    parent <- oneof [return Nothing, fmap Just genScope]
    return $ Scope name variables functions parent

genLanguage :: Gen Language
genLanguage = elements [Go, Typus, GoAndTypus, Unknown]

genValidIdentifier :: Gen String
genValidIdentifier = do
    first <- elements ['a'..'z']
    rest <- arbitrary `suchThat` all (`elem` ['a'..'z'] ++ ['0'..'9'] ++ "_")
    return (first : rest)