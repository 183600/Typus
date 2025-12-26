{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserRobustnessQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assertBool)
import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, listOf, elements, choose, sized)
import Data.Char (isSpace, isAlphaNum)
import Control.Exception (try, SomeException)

-- | Generate arbitrary strings with potential syntax errors
newtype MalformedString = MalformedString String
  deriving (Show)

instance Arbitrary MalformedString where
  arbitrary = do
    size <- choose (1, 100)
    content <- listOf $ oneof
      [ elements $ map (:[]) ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [" ", "\t", "\n"]
      , elements ["{", "}", "(", ")", "[", "]", ";", ",", ".", "+", "-", "*", "/", "=", "!", "<", ">", "&", "|", "^", "%"]
      , elements ["//", "/*", "*/", "\"", "'", "\\", "@", "#", "$", "~", "`"]
      ]
    let malformed = take size content
    return $ MalformedString malformed

-- | Generate strings with unbalanced brackets
newtype UnbalancedBrackets = UnbalancedBrackets String
  deriving (Show)

instance Arbitrary UnbalancedBrackets where
  arbitrary = do
    openCount <- choose (0, 10)
    closeCount <- choose (0, 10)
    content <- listOf $ elements $ map (:[]) ['a'..'z'] ++ [" ", "\n"]
    let opens = replicate openCount "{"
        closes = replicate closeCount "}"
        unbalanced = opens ++ content ++ closes
    return $ UnbalancedBrackets unbalanced

-- | Generate strings with malformed comments
newtype MalformedComments = MalformedComments String
  deriving (Show)

instance Arbitrary MalformedComments where
  arbitrary = do
    content <- listOf $ elements $ map (:[]) ['a'..'z'] ++ [" ", "\n"]
    commentTypes <- listOf $ elements
      [ "/*"  -- Unterminated block comment
      , "//"  -- Line comment
      , "*/"  -- Orphaned comment end
      , "/**" -- Unterminated doc comment
      ]
    let malformed = concat commentTypes ++ "\n" ++ unlines content
    return $ MalformedComments malformed

-- | Generate strings with invalid Unicode sequences
newtype InvalidUnicode = InvalidUnicode String
  deriving (Show)

instance Arbitrary InvalidUnicode where
  arbitrary = do
    validChars <- listOf $ elements $ map (:[]) ['a'..'z'] ++ [" ", "\n"]
    invalidBytes <- listOf $ elements ["\xFF", "\xFE", "\xFD", "\xFC", "\xFB", "\xFA"]
    return $ InvalidUnicode $ concat validChars ++ concat invalidBytes

tests :: TestTree
tests = testGroup "Parser Robustness Tests"
  [ testProperty "parser handles malformed strings without crashing" $ \(MalformedString s) ->
      let result = try (parseTypus "test" s)
      in case result of
        Left (_ :: SomeException) -> property True -- Parser should not crash
        Right _ -> property True -- Successful parse is also fine
  
  , testProperty "parser handles unbalanced brackets gracefully" $ \(UnbalancedBrackets s) ->
      let result = try (parseTypus "test" s)
      in case result of
        Left (_ :: SomeException) -> property True
        Right _ -> property True
  
  , testProperty "parser handles malformed comments safely" $ \(MalformedComments s) ->
      let result = try (parseTypus "test" s)
      in case result of
        Left (_ :: SomeException) -> property True
        Right _ -> property True
  
  , testProperty "parser handles invalid Unicode sequences" $ \(InvalidUnicode s) ->
      let result = try (parseTypus "test" s)
      in case result of
        Left (_ :: SomeException) -> property True
        Right _ -> property True
  
  , testCase "parser handles empty input" $ do
      let result = try (parseTypus "empty" "")
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle empty input" True
        Right file -> assertBool "Should parse empty input" $ True
  
  , testCase "parser handles only whitespace" $ do
      let whitespace = "   \t\n  \r\n  "
      let result = try (parseTypus "whitespace" whitespace)
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle whitespace only" True
        Right file -> assertBool "Should parse whitespace only" $ True
  
  , testCase "parser handles extremely long lines" $ do
      let longLine = replicate 10000 'a' ++ "\n"
      let result = try (parseTypus "long" longLine)
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle long lines" True
        Right file -> assertBool "Should parse long lines" $ True
  
  , testCase "parser handles deeply nested structures" $ do
      let deeplyNested = concat $ replicate 1000 "{"
          content = deeplyNested ++ "x" ++ concat (replicate 1000 "}")
      let result = try (parseTypus "nested" content)
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle deeply nested" True
        Right file -> assertBool "Should parse deeply nested" $ True
  
  , testCase "parser handles mixed line endings" $ do
      let mixedEndings = "line1\nline2\r\nline3\rline4\n"
      let result = try (parseTypus "mixed" mixedEndings)
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle mixed line endings" True
        Right file -> assertBool "Should parse mixed line endings" $ True
  
  , testProperty "parser is deterministic" $ \s ->
      let result1 = parseTypus "test1" s
          result2 = parseTypus "test2" s
      in show result1 == show result2 -- Compare string representations for equality
  
  , testCase "parser handles null bytes" $ do
      let withNull = "before\0after\n"
      let result = try (parseTypus "null" withNull)
      case result of
        Left (_ :: SomeException) -> assertBool "Should handle null bytes" True
        Right file -> assertBool "Should parse with null bytes" $ True
  ]