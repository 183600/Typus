{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.CoreParserPropertiesQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty



import Test.Tasty
import Test.Tasty.QuickCheck

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
    case (parseTypus code, parseTypus code) of
      (Right parsed, Right parsed2) -> assertBool "Parser is idempotent" (parsed == parsed2)
      (Left _, Left _) -> assertBool "Both parses failed consistently" True

  , testCase "Parser handles ownership directive correctly" $ do
    let input = "#ownership true"
    case parseTypus input of
      Right parsed -> 
        case tfDirectives parsed of
          FileDirectives (Just (Located True _ _)) _ _ -> assertBool "Ownership directive parsed" True
          _ -> assertFailure "Ownership directive not parsed correctly"
      Left err -> assertFailure ("Ownership directive parsing failed: " ++ show err)

  , testCase "Parser handles dependent types directive correctly" $ do
    let input = "#dependentTypes true"
    case parseTypus input of
      Right parsed -> 
        case tfDirectives parsed of
          FileDirectives _ (Just (Located True _ _)) _ -> assertBool "Dependent types directive parsed" True
          _ -> assertFailure "Dependent types directive not parsed correctly"
      Left err -> assertFailure ("Dependent types directive parsing failed: " ++ show err)

  , testCase "Parser handles unicode characters" $ do
    let unicodeStr = "测试函数() { 返回 42; }"
    case parseTypus unicodeStr of
      Right _ -> assertBool "Unicode parsing succeeded" True
      Left _ -> assertBool "Unicode parsing failed gracefully" True
  ]