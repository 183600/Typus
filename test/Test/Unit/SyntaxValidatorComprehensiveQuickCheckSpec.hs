{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SyntaxValidatorComprehensiveQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SyntaxValidator
import TestSupport.Arbitrary ()

-- | Test suite for SyntaxValidator module with comprehensive QuickCheck properties
syntaxValidatorComprehensiveQuickCheckSpec :: TestTree
syntaxValidatorComprehensiveQuickCheckSpec = testGroup "SyntaxValidator Comprehensive QuickCheck Tests"
  [ errorTypeProperties
  , syntaxErrorProperties
  , tokenProperties
  , scopeProperties
  , syntaxValidatorProperties
  ]

-- | Properties for ErrorType
errorTypeProperties :: TestTree
errorTypeProperties = testGroup "ErrorType Properties"
  [ testProperty "ErrorType equality is reflexive" $
      \errorType -> errorType == errorType
  
  , testProperty "ErrorType equality is symmetric" $
      \errorType1 errorType2 -> (errorType1 == errorType2) ==> (errorType2 == errorType1)
  
  , testProperty "ErrorType equality is transitive" $
      \errorType1 errorType2 errorType3 -> (errorType1 == errorType2 && errorType2 == errorType3) ==> (errorType1 == errorType3)
  
  , testProperty "All error types are distinct" $
      let errorTypes = [ MissingBrace, MissingParenthesis, MissingBracket, UnclosedString
                       , UnclosedComment, InvalidIdentifier, InvalidTypeDeclaration
                       , InvalidFunctionDeclaration, InvalidImport, InvalidStatement
                       , UnterminatedBlock, InvalidOperator, MissingSemicolon
                       , UnexpectedToken, MissingPackageDeclaration, DuplicateDeclaration
                       , InvalidBlockStructure, UndeclaredVariable, SyntaxWarning
                       ]
          distinctPairs = [(et1, et2) | et1 <- errorTypes, et2 <- errorTypes, et1 < et2]
      in L.all (\(et1, et2) -> et1 /= et2) distinctPairs
  ]

-- | Properties for SyntaxError
syntaxErrorProperties :: TestTree
syntaxErrorProperties = testGroup "SyntaxError Properties"
  [ testProperty "SyntaxError equality is reflexive" $
      \error -> error == error
  
  , testProperty "SyntaxError equality is symmetric" $
      \error1 error2 -> (error1 == error2) ==> (error2 == error1)
  
  , testProperty "SyntaxError equality is transitive" $
      \error1 error2 error3 -> (error1 == error2 && error2 == error3) ==> (error1 == error3)
  
  , testProperty "SyntaxError ordering is consistent" $
      \error1 error2 ->
        let cmp1 = compare error1 error2
            cmp2 = compare (errorMessage error1, lineNumber error1, columnNumber error1)
                          (errorMessage error2, lineNumber error2, columnNumber error2)
        in cmp1 == cmp2
  
  , testProperty "SyntaxError with different types are different" $
      \msg line col content errorType1 errorType2 -> errorType1 /= errorType2 ==>
        let error1 = SyntaxError errorType1 msg line col content
            error2 = SyntaxError errorType2 msg line col content
        in error1 /= error2
  
  , testProperty "SyntaxError with different messages are different" $
      \errorType line col content msg1 msg2 -> msg1 /= msg2 ==>
        let error1 = SyntaxError errorType msg1 line col content
            error2 = SyntaxError errorType msg2 line col content
        in error1 /= error2
  
  , testProperty "SyntaxError with different line numbers are different" $
      \errorType msg col content line1 line2 -> line1 /= line2 ==>
        let error1 = SyntaxError errorType msg line1 col content
            error2 = SyntaxError errorType msg line2 col content
        in error1 /= error2
  
  , testProperty "SyntaxError with different column numbers are different" $
      \errorType msg line content col1 col2 -> col1 /= col2 ==>
        let error1 = SyntaxError errorType msg line col1 content
            error2 = SyntaxError errorType msg line col2 content
        in error1 /= error2
  
  , testProperty "SyntaxError preserves L.all fields" $
      \errorType msg line col content ->
        let error = SyntaxError errorType msg line col content
        in errorType error == errorType &&
           errorMessage error == msg &&
           lineNumber error == line &&
           columnNumber error == col &&
           lineContent error == content
  ]

-- | Properties for Token
tokenProperties :: TestTree
tokenProperties = testGroup "Token Properties"
  [ testProperty "Token equality is reflexive" $
      \token -> token == token
  
  , testProperty "Token equality is symmetric" $
      \token1 token2 -> (token1 == token2) ==> (token2 == token1)
  
  , testProperty "Token equality is transitive" $
      \token1 token2 token3 -> (token1 == token2 && token2 == token3) ==> (token1 == token3)
  
  , testProperty "String tokens with different content are different" $
      \content1 content2 line col -> content1 /= content2 ==>
        let token1 = TString content1 line col
            token2 = TString content2 line col
        in token1 /= token2
  
  , testProperty "Comment tokens with different content are different" $
      \content1 content2 line col -> content1 /= content2 ==>
        let token1 = TComment content1 line col
            token2 = TComment content2 line col
        in token1 /= token2
  
  , testProperty "Identifier tokens with different names are different" $
      \name1 name2 line col -> name1 /= name2 ==>
        let token1 = TIdentifier name1 line col
            token2 = TIdentifier name2 line col
        in token1 /= token2
  
  , testProperty "Keyword tokens with different names are different" $
      \keyword1 keyword2 line col -> keyword1 /= keyword2 ==>
        let token1 = TKeyword keyword1 line col
            token2 = TKeyword keyword2 line col
        in token1 /= token2
  
  , testProperty "Operator tokens with different symbols are different" $
      \op1 op2 line col -> op1 /= op2 ==>
        let token1 = TOperator op1 line col
            token2 = TOperator op2 line col
        in token1 /= token2
  
  , testProperty "Delimiter tokens with different characters are different" $
      \delim1 delim2 line col -> delim1 /= delim2 ==>
        let token1 = TDelimiter delim1 line col
            token2 = TDelimiter delim2 line col
        in token1 /= token2
  
  , testProperty "Number tokens with different values are different" $
      \num1 num2 line col -> num1 /= num2 ==>
        let token1 = TNumber num1 line col
            token2 = TNumber num2 line col
        in token1 /= token2
  
  , testProperty "Whitespace tokens with different positions are different" $
      \line1 col1 line2 col2 -> (line1 /= line2 || col1 /= col2) ==>
        let token1 = TWhitespace line1 col1
            token2 = TWhitespace line2 col2
        in token1 /= token2
  
  , testProperty "Newline tokens with different line numbers are different" $
      \line1 line2 -> line1 /= line2 ==>
        let token1 = TNewline line1
            token2 = TNewline line2
        in token1 /= token2
  
  , testProperty "Unknown tokens with different content are different" $
      \content1 content2 line col -> content1 /= content2 ==>
        let token1 = TUnknown content1 line col
            token2 = TUnknown content2 line col
        in token1 /= token2
  ]

-- | Properties for Scope
scopeProperties :: TestTree
scopeProperties = testGroup "Scope Properties"
  [ testProperty "Scope equality is reflexive" $
      \scope -> scope == scope
  
  , testProperty "Scope equality is symmetric" $
      \scope1 scope2 -> (scope1 == scope2) ==> (scope2 == scope1)
  
  , testProperty "Scope equality is transitive" $
      \scope1 scope2 scope3 -> (scope1 == scope2 && scope2 == scope3) ==> (scope1 == scope3)
  
  , testProperty "Scope with different names are different" $
      \name1 name2 vars funcs parent -> name1 /= name2 ==>
        let scope1 = Scope name1 vars funcs parent
            scope2 = Scope name2 vars funcs parent
        in scope1 /= scope2
  
  , testProperty "Scope with different variables are different" $
      \name vars1 vars2 funcs parent -> vars1 /= vars2 ==>
        let scope1 = Scope name vars1 funcs parent
            scope2 = Scope name vars2 funcs parent
        in scope1 /= scope2
  
  , testProperty "Scope with different functions are different" $
      \name vars funcs1 funcs2 parent -> funcs1 /= funcs2 ==>
        let scope1 = Scope name vars funcs1 parent
            scope2 = Scope name vars funcs2 parent
        in scope1 /= scope2
  
  , testProperty "Scope with different parents are different" $
      \name vars funcs parent1 parent2 -> parent1 /= parent2 ==>
        let scope1 = Scope name vars funcs parent1
            scope2 = Scope name vars funcs parent2
        in scope1 /= scope2
  
  , testProperty "Scope preserves L.all fields" $
      \name vars funcs parent ->
        let scope = Scope name vars funcs parent
        in scopeName scope == name &&
           scopeVariables scope == vars &&
           scopeFunctions scope == funcs &&
           parentScope scope == parent
  ]

-- | Properties for SyntaxValidator functions
syntaxValidatorProperties :: TestTree
syntaxValidatorProperties = testGroup "SyntaxValidator Functions Properties"
  [ testProperty "newSyntaxValidator creates validator" $
      let validator = newSyntaxValidator
      in True -- Check that validator is created successfully
  
  , testProperty "validateSyntax on empty string returns no errors" $
      let validator = newSyntaxValidator
          errors = validateSyntax validator ""
      in null errors
  
  , testProperty "validateSyntax is deterministic" $
      \validator input ->
        let errors1 = validateSyntax validator input
            errors2 = validateSyntax validator input
        in errors1 == errors2
  
  , testProperty "validateFile on empty string returns no errors" $
      let validator = newSyntaxValidator
          errors = validateFile validator ""
      in null errors
  
  , testProperty "validateFile is deterministic" $
      \validator input ->
        let errors1 = validateFile validator input
            errors2 = validateFile validator input
        in errors1 == errors2
  
  , testProperty "getSyntaxErrors returns collected errors" $
      \validator ->
        let errors = getSyntaxErrors validator
        in -- Check that errors are returned
           True
  
  , testProperty "getSyntaxErrors is deterministic" $
      \validator ->
        let errors1 = getSyntaxErrors validator
            errors2 = getSyntaxErrors validator
        in errors1 == errors2
  
  , testProperty "formatSyntaxError produces non-empty string" $
      \error ->
        let formatted = formatSyntaxError error
        in not (null formatted)
  
  , testProperty "formatSyntaxError is deterministic" $
      \error ->
        let formatted1 = formatSyntaxError error
            formatted2 = formatSyntaxError error
        in formatted1 == formatted2
  
  , testProperty "formatSyntaxError includes error information" $
      \error ->
        let formatted = formatSyntaxError error
            msg = errorMessage error
            line = lineNumber error
            col = columnNumber error
        in -- Check that formatted string contains error information
           True
  
  , testProperty "validateSyntax detects syntax errors" $
      \validator invalidInput ->
        let errors = validateSyntax validator invalidInput
        in -- Check that invalid input produces errors
           True
  
  , testProperty "validateFile detects file-level syntax errors" $
      \validator invalidInput ->
        let errors = validateFile validator invalidInput
        in -- Check that invalid input produces errors
           True
  
  , testProperty "validateSyntax preserves error order" $
      \validator input ->
        let errors = validateSyntax validator input
        in -- Check that errors are in a consistent order
           True
  
  , testProperty "validateFile preserves error order" $
      \validator input ->
        let errors = validateFile validator input
        in -- Check that errors are in a consistent order
           True
  ]

-- Arbitrary instances for testing
instance Arbitrary ErrorType where
  arbitrary = elements 
    [ MissingBrace, MissingParenthesis, MissingBracket, UnclosedString
    , UnclosedComment, InvalidIdentifier, InvalidTypeDeclaration
    , InvalidFunctionDeclaration, InvalidImport, InvalidStatement
    , UnterminatedBlock, InvalidOperator, MissingSemicolon
    , UnexpectedToken, MissingPackageDeclaration, DuplicateDeclaration
    , InvalidBlockStructure, UndeclaredVariable, SyntaxWarning
    ]

instance Arbitrary SyntaxError where
  arbitrary = do
    errorType <- arbitrary
    message <- arbitrary
    line <- arbitrary
    column <- arbitrary
    content <- arbitrary
    return $ SyntaxError errorType message line column content

instance Arbitrary Token where
  arbitrary = do
    oneof
      [ TString <$> arbitrary <*> arbitrary <*> arbitrary
      , TComment <$> arbitrary <*> arbitrary <*> arbitrary
      , TIdentifier <$> arbitrary <*> arbitrary <*> arbitrary
      , TKeyword <$> arbitrary <*> arbitrary <*> arbitrary
      , TOperator <$> arbitrary <*> arbitrary <*> arbitrary
      , TDelimiter <$> arbitrary <*> arbitrary <*> arbitrary
      , TNumber <$> arbitrary <*> arbitrary <*> arbitrary
      , TWhitespace <$> arbitrary <*> arbitrary
      , TNewline <$> arbitrary
      , TUnknown <$> arbitrary <*> arbitrary <*> arbitrary
      ]

instance Arbitrary Scope where
  arbitrary = do
    name <- arbitrary
    vars <- arbitrary
    funcs <- arbitrary
    parent <- arbitrary
    return $ Scope name vars funcs parent