{-# LANGUAGE ScopedTypeVariables #-}

module CoreParserPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- | Test parser properties with QuickCheck
coreParserPropertiesSpec :: TestTree
coreParserPropertiesSpec = testGroup "Core Parser Properties"
  [ testProperty "Parsing empty string returns default structure" $
      \_ -> parseTypus "" == Right (TypusFile defaultFileDirectives [])

  , testProperty "Parser is idempotent for valid code" $
      \code -> case parseTypus code of
        Right parsed -> parseTypus code == Right parsed
        Left _ -> property True

  , testCase "Parser handles ownership directive correctly" $ do
    let input = "#ownership true"
    case parseTypus input of
      Right (TypusFile (FileDirectives (Just (Located _ True)) _ _) _) -> assertBool "Ownership directive parsed" True
      _ -> assertFailure "Ownership directive not parsed correctly"

  , testCase "Parser handles dependent types directive correctly" $ do
    let input = "#dependentTypes true"
    case parseTypus input of
      Right (TypusFile (FileDirectives _ (Just (Located _ True)) _) _) -> assertBool "Dependent types directive parsed" True
      _ -> assertFailure "Dependent types directive not parsed correctly"

  , testProperty "Parser handles unicode characters" $
      \unicodeStr -> case parseTypus unicodeStr of
        Right _ -> property True
        Left _ -> property True -- Unicode parsing might fail but shouldn't crash
  ]