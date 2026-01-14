{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.AdvancedTextProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Utils
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isSpace, isControl)
import qualified Data.Text as T

spec :: Spec
spec = describe "Advanced Text Processing Tests" $ do

  describe "trim function edge cases" $ do
    it "handles empty strings" $ do
      trim "" `shouldBe` ""
      
    it "handles strings with only whitespace" $ do
      trim "   " `shouldBe` ""
      trim "\t\n\r " `shouldBe` ""
      trim "\n\t\n\t" `shouldBe` ""
      
    it "preserves internal whitespace" $ do
      trim "  hello  world  " `shouldBe` "hello  world"
      trim "\tfoo\tbar\t" `shouldBe` "foo\tbar"
      
    it "handles Unicode whitespace" $ property $
      \str -> trim (unwords [str, str]) `shouldBe` unwords [str, str]

  describe "splitBy function edge cases" $ do
    it "handles empty input" $ do
      splitBy ',' "" `shouldBe` []
      
    it "handles single character" $ do
      splitBy ',' "a" `shouldBe` ["a"]
      splitBy ',' "," `shouldBe` ["", ""]
      
    it "handles consecutive delimiters" $ do
      splitBy ',' "a,,b" `shouldBe` ["a", "", "b"]
      splitBy ',' "a,,,b" `shouldBe` ["a", "", "", "b"]
      splitBy ',' ",,," `shouldBe` ["", "", "", ""]
      
    it "handles leading and trailing delimiters" $ do
      splitBy ',' ",a,b," `shouldBe` ["", "a", "b", ""]
      splitBy ',' ",a," `shouldBe` ["", "a", ""]
      
    it "preserves empty segments" $ property $
      \delim str -> splitBy delim (delim:str:delim:[]) `shouldBe` ["", str, ""]

  describe "removeLineComments function" $ do
    it "handles empty strings" $ do
      removeLineComments "" `shouldBe` ""
      
    it "handles strings without comments" $ do
      removeLineComments "let x = 42" `shouldBe` "let x = 42"
      
    it "removes comments correctly" $ do
      removeLineComments "let x = 42 // comment" `shouldBe` "let x = 42"
      removeLineComments "// full line comment" `shouldBe` ""
      
    it "preserves comments in string literals" $ do
      removeLineComments "let s = \"// not a comment\"" `shouldBe` "let s = \"// not a comment\""
      removeLineComments "let s = \"hello // world\"" `shouldBe` "let s = \"hello // world\""
      
    it "handles escaped quotes in strings" $ do
      removeLineComments "let s = \"\\\"// not a comment\"" `shouldBe` "let s = \"\\\"// not a comment\""
      
    it "handles character literals" $ do
      removeLineComments "let c = '/' // comment" `shouldBe` "let c = '/'"
      removeLineComments "let c = '\"' // comment" `shouldBe` "let c = '\"'"

  describe "removeComments function" $ do
    it "handles empty strings" $ do
      removeComments "" `shouldBe` ""
      
    it "handles strings without comments" $ do
      removeComments "let x = 42" `shouldBe` "let x = 42"
      
    it "removes line comments" $ do
      removeComments "let x = 42 // comment" `shouldBe` "let x = 42 "
      
    it "removes block comments" $ do
      removeComments "let x = 42 /* comment */" `shouldBe` "let x = 42 "
      
    it "handles nested block comments" $ do
      removeComments "code /* outer /* inner */ still outer */ end" `shouldBe` "code  end"
      
    it "preserves comments in string literals" $ do
      removeComments "let s = \"// not a comment\"" `shouldBe` "let s = \"// not a comment\""
      removeComments "let s = \"/* not a comment */\"" `shouldBe` "let s = \"/* not a comment */\""
      
    it "handles multiline comments" $ do
      removeComments "line1\n/* comment\nspanning multiple\nlines */\nline2" 
        `shouldBe` "line1\n\nline2"

  describe "normalizeIndentation function" $ do
    it "handles empty strings" $ do
      normalizeIndentation "" `shouldBe` ""
      
    it "handles single lines" $ do
      normalizeIndentation "  single line" `shouldBe` "  single line"
      
    it "removes common prefix indentation" $ do
      normalizeIndentation "  line1\n    line2\n  line3" `shouldBe` "line1\n  line2\nline3"
      
    it "preserves relative indentation" $ do
      normalizeIndentation "    outer\n      inner\n    outer" `shouldBe` "outer\n  inner\nouter"
      
    it "handles mixed tabs and spaces" $ do
      normalizeIndentation "\t  mixed\n\t    indentation" `shouldBe` "mixed\n  indentation"
      
    it "preserves empty lines" $ do
      normalizeIndentation "  line1\n\n  line2" `shouldBe` "line1\n\nline2"

  describe "breakOn function" $ do
    it "handles empty pattern" $ do
      breakOn "" "hello" `shouldBe` ("", "hello")
      
    it "handles empty input" $ do
      breakOn "pattern" "" `shouldBe` ("", "")
      
    it "finds first occurrence" $ do
      breakOn "," "a,b,c" `shouldBe` ("a", "b,c")
      breakOn "pattern" "prefix pattern suffix" `shouldBe` ("prefix ", " suffix")
      
    it "handles pattern at start" $ do
      breakOn "pattern" "pattern suffix" `shouldBe` ("", " suffix")
      
    it "handles pattern at end" $ do
      breakOn "pattern" "prefix pattern" `shouldBe` ("prefix ", "")
      
    it "handles non-existent pattern" $ do
      breakOn "xyz" "abc" `shouldBe` ("abc", "")

  describe "safeProcessString function" $ do
    it "handles normal strings" $ do
      safeProcessString "hello world" `shouldBe` Right "hello world"
      
    it "filters control characters" $ do
      safeProcessString "hello\x00world" `shouldBe` Right "hello world"
      safeProcessString "text\x01\x02end" `shouldBe` Right "textend"
      
    it "preserves newlines and tabs" $ do
      safeProcessString "line1\nline2\ttab" `shouldBe` Right "line1\nline2\ttab"
      
    it "preserves carriage returns" $ do
      safeProcessString "windows\r\nline" `shouldBe` Right "windows\r\nline"

  describe "isValidChar function" $ do
    it "accepts printable characters" $ do
      isValidChar 'a' `shouldBe` True
      isValidChar 'Z' `shouldBe` True
      isValidChar '5' `shouldBe` True
      isValidChar '!' `shouldBe` True
      
    it "accepts whitespace characters" $ do
      isValidChar ' ' `shouldBe` True
      isValidChar '\t' `shouldBe` True
      isValidChar '\n' `shouldBe` True
      isValidChar '\r' `shouldBe` True
      
    it "rejects control characters" $ do
      isValidChar '\x00' `shouldBe` False
      isValidChar '\x01' `shouldBe` False
      isValidChar '\x1F' `shouldBe` False

  describe "QuickCheck properties" $ do
    it "trim idempotence" $ property $
      \str -> trim (trim str) `shouldBe` trim str
      
    it "splitBy consistency" $ property $
      \delim str -> concat (splitBy delim str) `shouldSatisfy` (\s -> length s >= length str - length (filter (== delim) str))
      
    it "breakOn consistency" $ property $
      \pat str -> let (before, after) = breakOn pat str
                   in if null pat then before `shouldBe` "" else before ++ pat ++ after `shouldBe` str