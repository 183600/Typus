{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.ParserErrorHandlingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Test.Tasty.HUnit (testCase, assertBool)

import qualified Data.Text as T
import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourceSpan(..), SourcePos(..))
import Utils (trim, removeComments)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary FileDirectives where
  arbitrary = FileDirectives <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary BlockDirectives where
  arbitrary = BlockDirectives <$> arbitrary <*> arbitrary <*> arbitrary

-- Generate simple code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = CodeBlock <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary CodeBlock where
  arbitrary = genCodeBlock

instance Arbitrary TypusFile where
  arbitrary = TypusFile <$> arbitrary <*> arbitrary <*> arbitrary

-- Generate strings with potential parsing issues
genProblematicString :: Gen String
genProblematicString = do
  n <- arbitrary `suchThat` (\x -> x >= 0 && x < 100)
  elements $ replicate n "test" ++ ["", " ", "\n", "\t", "// comment", "/* comment */", "unclosed string \""]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parsing empty input should not crash
prop_parseEmptyInput :: Property
prop_parseEmptyInput = 
  let result = parseTypus "" 
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parser should handle whitespace gracefully
prop_handleWhitespace :: String -> Property
prop_handleWhitespace input = 
  let wsInput = "   \n\t  " ++ input ++ "   \n\t  "
      result = parseTypus wsInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Comment removal should be idempotent
prop_commentRemovalIdempotent :: String -> Property
prop_commentRemovalIdempotent input = 
  let once = removeComments input
      twice = removeComments once
  in once === twice

-- Property: Trim function should be idempotent
prop_trimIdempotent :: String -> Property
prop_trimIdempotent input = 
  let once = trim input
      twice = trim once
  in once === twice

-- Property: Parser should handle unicode characters
prop_handleUnicode :: Property
prop_handleUnicode = 
  let unicodeInput = "测试 🚀 αβγ"
      result = parseTypus unicodeInput
  in case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Parsing should be deterministic
prop_parsingDeterministic :: String -> Property
prop_parsingDeterministic input = 
  let result1 = parseTypus input
      result2 = parseTypus input
  in result1 === result2

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Error Handling QuickCheck Tests"
  [ testProperty "Parse empty input without crashing" prop_parseEmptyInput
  , testProperty "Handle whitespace gracefully" prop_handleWhitespace
  , testProperty "Comment removal is idempotent" prop_commentRemovalIdempotent
  , testProperty "Trim function is idempotent" prop_trimIdempotent
  , testProperty "Handle unicode characters" prop_handleUnicode
  , testProperty "Parsing is deterministic" prop_parsingDeterministic
  , testCase "Parser handles malformed input gracefully" $ do
      let malformedInputs = ["unclosed \"string", "/* unclosed comment", "{", "}"]
      mapM_ (\input -> 
        case parseTypus input of
          Left _ -> assertBool "Expected parsing error" True
          Right _ -> assertBool "Unexpected success" False
      ) malformedInputs
  ]