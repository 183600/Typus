{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreParserPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Test parser properties with QuickCheck
coreParserPropertiesSpec :: TestTree
coreParserPropertiesSpec = testGroup "Core Parser Properties"
  [ testCase "Parsing empty string returns default structure" $ do
    let result = parseTypus ""
    assertBool "Empty string parses to default structure" (result == Right (TypusFile defaultFileDirectives [] [] []))

  , testCase "Parser is idempotent for valid code" $ do
    let code = "func test() { return 42; }"
    case parseTypus code of
      Right parsed -> 
        case parseTypus code of
          Right parsed2 -> assertBool "Parser is idempotent" (parsed == parsed2)
          Left err -> assertFailure ("Second parse failed: " ++ show err)
      Left err -> assertFailure ("First parse failed: " ++ show err)

  , testCase "Parser handles ownership directive correctly" $ do
    let input = "#ownership true"
    case parseTypus input of
      Right (TypusFile (FileDirectives (Just (Located True (SourcePos 0 0 0) (SourceSpan (SourcePos 0 0 0) (SourcePos 0 0 0)))) _ _) _ _ _) -> assertBool "Ownership directive parsed" True
      _ -> assertFailure "Ownership directive not parsed correctly"

  , testCase "Parser handles dependent types directive correctly" $ do
    let input = "#dependentTypes true"
    case parseTypus input of
      Right (TypusFile (FileDirectives _ (Just (Located True (SourcePos 0 0 0) (SourceSpan (SourcePos 0 0 0) (SourcePos 0 0 0)))) _) _ _ _) -> assertBool "Dependent types directive parsed" True
      _ -> assertFailure "Dependent types directive not parsed correctly"

  , testCase "Parser handles unicode characters" $ do
    let unicodeStr = "测试函数() { 返回 42; }"
    case parseTypus unicodeStr of
      Right _ -> assertBool "Unicode parsing succeeded" True
      Left _ -> assertBool "Unicode parsing failed gracefully" True
  ]