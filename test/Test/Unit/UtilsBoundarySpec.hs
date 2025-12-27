{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.UtilsBoundarySpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils

spec :: Spec
spec = describe "Utils Boundary Conditions" $ do
  
  describe "trim" $ do
    it "handles empty strings" $
      trim "" `shouldBe` ""
    
    it "handles strings with only whitespace" $
      trim "   \t\n  " `shouldBe` ""
    
    it "preserves internal whitespace" $
      trim "  hello world  " `shouldBe` "hello world"
    
    it "handles unicode whitespace correctly" $
      trim "\x2000hello\x2009" `shouldBe` "hello"

  describe "splitBy" $ do
    it "handles empty input" $
      splitBy ',' "" `shouldBe` [""]
    
    it "handles single character" $
      splitBy ',' "a" `shouldBe` ["a"]
    
    it "handles consecutive delimiters" $
      splitBy ',' "a,,b" `shouldBe` ["a", "", "b"]
    
    it "handles leading and trailing delimiters" $
      splitBy ',' ",a," `shouldBe` ["", "a", ""]
    
    it "handles only delimiters" $
      splitBy ',' ",,," `shouldBe` ["", "", "", ""]

  describe "splitByCollapsed" $ do
    it "handles empty input" $
      splitByCollapsed ',' "" `shouldBe` []
    
    it "handles single character" $
      splitByCollapsed ',' "a" `shouldBe` ["a"]
    
    it "collapses consecutive delimiters" $
      splitByCollapsed ',' "a,,b" `shouldBe` ["a", "b"]
    
    it "removes leading and trailing delimiters" $
      splitByCollapsed ',' ",a," `shouldBe` ["a"]
    
    it "handles only delimiters" $
      splitByCollapsed ',' ",,," `shouldBe` []

  describe "removeLineComments" $ do
    it "handles empty input" $
      removeLineComments "" `shouldBe` ""
    
    it "preserves lines without comments" $
      removeLineComments "hello\nworld" `shouldBe` "hello\nworld"
    
    it "removes single line comments" $
      removeLineComments "hello // comment\nworld" `shouldBe` "hello \nworld"
    
    it "preserves // in string literals" $
      removeLineComments "let x = \"// not a comment\"\nlet y = // real comment" `shouldBe` "let x = \"// not a comment\"\nlet y = "
    
    it "preserves // in char literals" $
      removeLineComments "let x = '/' // comment\nlet y = 'a'" `shouldBe` "let x = '/' \nlet y = 'a'"
    
    it "handles escaped quotes in strings" $
      removeLineComments "let x = \"\\\"// not comment\\\"\"\nlet y = // comment" `shouldBe` "let x = \"\\\"// not comment\\\"\"\nlet y = "

  describe "removeComments" $ do
    it "handles empty input" $
      removeComments "" `shouldBe` ""
    
    it "handles only line comments" $
      removeComments "// line comment\n" `shouldBe` "\n"
    
    it "handles only block comments" $
      removeComments "/* block comment */" `shouldBe` ""
    
    it "handles nested line and block comments" $
      removeComments "/* block // line */" `shouldBe` ""
    
    it "preserves strings with comment markers" $
      removeComments "\"/* not comment */\" // real comment" `shouldBe` "\"/* not comment */\" "
    
    it "handles multiline block comments" $
      removeComments "before\n/* multi\nline\ncomment */\nafter" `shouldBe` "before\n\n\nafter"
    
    it "handles unclosed block comment gracefully" $
      removeComments "before /* unclosed" `shouldBe` "before "
    
    it "handles unclosed string gracefully" $
      removeComments "\"unclosed string // comment" `shouldBe` "\"unclosed string // comment"

  describe "normalizeIndentation" $ do
    it "handles empty input" $
      normalizeIndentation "" `shouldBe` ""
    
    it "handles single line" $
      normalizeIndentation "    hello" `shouldBe` "hello"
    
    it "handles mixed indentation" $
      normalizeIndentation "    hello\n  world\n\ttest" `shouldBe` "hello\nworld\ntest"
    
    it "preserves relative indentation" $
      normalizeIndentation "  hello\n    world\n  test" `shouldBe` "hello\n  world\ntest"
    
    it "handles lines with only whitespace" $
      normalizeIndentation "  hello\n    \n  world" `shouldBe` "hello\n    \nworld"

  describe "breakOn" $ do
    it "handles empty pattern" $
      breakOn "" "hello" `shouldBe` ("", "hello")
    
    it "handles pattern not found" $
      breakOn "xyz" "hello" `shouldBe` ("hello", "")
    
    it "handles pattern at start" $
      breakOn "hello" "hello world" `shouldBe` ("", " world")
    
    it "handles pattern at end" $
      breakOn "world" "hello world" `shouldBe` ("hello ", "")
    
    it "handles pattern in middle" $
      breakOn "lo" "hello world" `shouldBe` ("hel", " world")
    
    it "handles empty input" $
      breakOn "pattern" "" `shouldBe` ("", "")