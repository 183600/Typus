{-# LANGUAGE CPP #-}

module Test.Unit.GoLexerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import Compiler.GoLexer

prop_tokenizeGo_reconstructs_source :: String -> Property
prop_tokenizeGo_reconstructs_source source =
  let tokens = tokenizeGo source
      reconstructed = concatMap tokenText tokens
  in reconstructed === source

prop_tokenizeGo_nonempty_produces_tokens :: Property
prop_tokenizeGo_nonempty_produces_tokens =
  forAll (listOf1 (elements ['a'..'z'])) $ \source ->
  let tokens = tokenizeGo source
  in not (null tokens) === True

prop_whitespace_tokens_recognized :: Property
prop_whitespace_tokens_recognized =
  forAll (listOf1 (elements " \t\n\r")) $ \ws ->
  let tokens = tokenizeGo ws
  in L.all isWhitespaceToken tokens === True

prop_comment_tokens_recognized :: Property
prop_comment_tokens_recognized =
  forAll genComment $ \comment ->
  let tokens = tokenizeGo comment
      commentTokens = filter isCommentToken tokens
  in not (null commentTokens) === True
  where
    genComment = do
      content <- listOf (elements ['a'..'z'])
      elements ["// " ++ content, "/* " ++ content ++ " */"]

prop_string_tokens_recognized :: Property
prop_string_tokens_recognized =
  forAll genString $ \str ->
  let tokens = tokenizeGo str
      stringTokens = filter isStringToken tokens
  in not (null stringTokens) === True
  where
    genString = do
      content <- listOf (elements ['a'..'z'])
      elements ["\"" ++ content ++ "\"", "`" ++ content ++ "`"]

prop_identifier_tokens_recognized :: Property
prop_identifier_tokens_recognized =
  forAll genIdentifier $ \ident ->
  let tokens = tokenizeGo ident
      identTokens = filter isIdentifierToken tokens
  in not (null identTokens) === True
  where
    genIdentifier = do
      first <- elements ['a'..'z']
      rest <- listOf (elements (['a'..'z'] ++ ['0'..'9'] ++ ['_']))
      return (first : rest)

prop_tokenKind_consistency :: Property
prop_tokenKind_consistency =
  forAll (arbitrary :: Gen Char) $ \c ->
  let tokens = tokenizeGo [c]
  in L.all (\tok -> tokenKind tok `elem` validKinds) tokens === True
  where
    validKinds = [TokIdentifier, TokKeyword, TokNumber, TokString,
                  TokComment, TokOperator, TokSymbol, TokWhitespace, TokOther]

prop_empty_source_produces_empty_tokens :: Property
prop_empty_source_produces_empty_tokens =
  tokenizeGo "" === []

tests :: TestTree
tests = testGroup "GoLexer Properties QuickCheck Tests"
  [ fastProperty "tokenizeGo reconstructs source" prop_tokenizeGo_reconstructs_source
  , fastProperty "non-empty source produces tokens" prop_tokenizeGo_nonempty_produces_tokens
  , fastProperty "whitespace tokens recognized" prop_whitespace_tokens_recognized
  , fastProperty "comment tokens recognized" prop_comment_tokens_recognized
  , fastProperty "string tokens recognized" prop_string_tokens_recognized
  , fastProperty "identifier tokens recognized" prop_identifier_tokens_recognized
  , fastProperty "token kind consistency" prop_tokenKind_consistency
  , fastProperty "empty source produces empty tokens" prop_empty_source_produces_empty_tokens
  ]
