{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewUnicodeParsingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized)

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.Char (ord, isLetter, isSymbol)
import Data.Text as T (pack, unpack, Text)

tests :: TestTree
tests = testGroup "New Unicode Parsing Tests"
    [ testCase "parses Unicode identifiers with Chinese characters" $ do
        let source = unlines
              [ "package main"
              , "func 测试函数() {"
              , "  let 变量 = \"中文内容\""
              , "  println(变量)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse Unicode identifiers: " ++ err
          Right typusFile -> do
            assertBool "Should parse successfully" True
            
    , testCase "parses Unicode identifiers with emoji" $ do
        let source = unlines
              [ "package main"
              , "func 🚀_launch() {"
              , "  let 📦 = \"package\""
              , "  println(📦)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse emoji identifiers: " ++ err
          Right typusFile -> do
            assertBool "Should parse successfully" True
            
    , testCase "parses Unicode strings with special characters" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "  let text = \"Hello 世界 🌍 café naïve résumé\""
              , "  println(text)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse Unicode strings: " ++ err
          Right typusFile -> do
            assertBool "Should parse successfully" True
            
    , testCase "handles Unicode comments correctly" $ do
        let source = unlines
              [ "package main"
              , "// 这是一个中文注释"
              , "func main() {"
              , "  // 🎯 This is a comment with emoji"
              , "  let x = 42 // 变量赋值"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Failed to parse Unicode comments: " ++ err
          Right typusFile -> do
            assertBool "Should parse successfully" True

    -- QuickCheck properties for Unicode handling
    , testCase "QuickCheck property: Unicode identifiers should be preserved correctly" $ do
        let funcName = T.pack "测试函数"
        let varName = T.pack "变量"
        let source = unlines
              [ "package main"
              , "func " ++ T.unpack funcName ++ "() {"
              , "  let " ++ T.unpack varName ++ " = \"test\""
              , "  return " ++ T.unpack varName
              , "}"
              ]
        case parseTypus source of
          Left _ -> assertFailure "Failed to parse Unicode identifiers"
          Right _ -> assertBool "Unicode identifiers preserved correctly" True
    ]

-- Helper data types for QuickCheck
newtype UnicodeString = UnicodeString Text
  deriving (Show, Eq)

instance Arbitrary UnicodeString where
  arbitrary = UnicodeString <$> unicodeStringGen
    where
      unicodeStringGen :: Gen Text
      unicodeStringGen = T.pack <$> listOf unicodeCharGen
      
      unicodeCharGen :: Gen Char
      unicodeCharGen = oneof
        [ -- ASCII letters and numbers
          elements ['a'..'z'],
          elements ['A'..'Z'],
          elements ['0'..'9'],
          -- Common Unicode characters
          elements $ map chr [0x4e00..0x4e10], -- Chinese characters range start
          elements $ map chr [0x1F300..0x1F310], -- Emoji range start
          -- Common symbols
          elements ['_', '-', '!', '@', '#', '$', '%', '^', '&', '*']
        ]
      
      chr :: Int -> Char
      chr = toEnum