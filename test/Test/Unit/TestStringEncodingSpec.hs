{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestStringEncodingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck ()

import Utils
import Parser
import SourceLocation (Located(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import TestSupport.Arbitrary ()
import Data.Char (chr)
import Data.Word ()

-- | Test suite for string encoding
testStringEncoding :: TestTree
testStringEncoding = testGroup "String Encoding Tests"
  [ testCase "Utils: safeProcessString handles ASCII correctly" $
      let asciiString = "Hello, World!"
      in case safeProcessString asciiString of
           Left _ -> assertFailure "ASCII processing failed"
           Right result -> result @?= asciiString
           
  , testCase "Utils: safeProcessString handles UTF-8 correctly" $
      let utf8String = "你好, 世界!"
      in case safeProcessString utf8String of
           Left _ -> assertFailure "UTF-8 processing failed"
           Right result -> result @?= utf8String
           
  , testCase "Utils: safeProcessString filters control characters" $
      let stringWithControls = "Hello\x00\x01\x02World"
      in case safeProcessString stringWithControls of
           Right result -> result @?= "HelloWorld"
           Left _ -> assertFailure "Control character filtering failed"
           
  , testCase "Utils: safeProcessString preserves newline characters" $
      let stringWithNewlines = "Line 1\nLine 2\nLine 3"
      in case safeProcessString stringWithNewlines of
           Left _ -> assertFailure "Newline preservation failed"
           Right result -> result @?= stringWithNewlines
           
  , testCase "Utils: safeProcessString preserves tab characters" $
      let stringWithTabs = "Column 1\tColumn 2\tColumn 3"
      in case safeProcessString stringWithTabs of
           Left _ -> assertFailure "Tab preservation failed"
           Right result -> result @?= stringWithTabs
           
  , testCase "Utils: safeProcessString preserves carriage return" $
      let stringWithCR = "Line 1\rLine 2\rLine 3"
      in case safeProcessString stringWithCR of
           Left _ -> assertFailure "Carriage return preservation failed"
           Right result -> result @?= stringWithCR
           
  , testCase "Utils: removeComments handles Unicode in string literals" $
      let input = "const s = \"你好, 世界!\"; // comment"
      in removeComments input @?= "const s = \"你好, 世界!\"; "
      
  , testCase "Utils: removeComments handles Unicode in character literals" $
      let input = "const c = '你'; // comment"
      in removeComments input @?= "const c = '你'; "
      
  , testCase "Utils: removeComments handles Unicode in comments" $
      let input = "code // 你好, 世界!"
      in removeComments input @?= "code "
      
  , testCase "Utils: removeComments handles Unicode in block comments" $
      let input = "code /* 你好, 世界! */ more code"
      in removeComments input @?= "code  more code"
      
  , testCase "Parser: parseTypus handles Unicode in file directives" $
      let input = "//! message=\"你好, 世界!\"\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right _typusFile -> return ()
           
  , testCase "Parser: parseTypus handles Unicode in code blocks" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"你好, 世界!\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             if not (null blocks)
               then do
                 let block = case blocks of
                               (b:_) -> b
                               [] -> error "Impossible: blocks is not empty"
                 "你好, 世界!" `isInfixOf` (cbContent block) @?= True
               else return ()
               
  , testCase "Parser: parseTypus handles mixed ASCII and Unicode" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"Hello 你好 World 世界!\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             if not (null blocks)
               then do
                 let block = case blocks of
                               (b:_) -> b
                               [] -> error "Impossible: blocks is not empty"
                 "Hello 你好 World 世界!" `isInfixOf` (cbContent block) @?= True
               else return ()
               
  , testCase "Parser: parseTypus handles Unicode in build tags" $
      let input = "// +build 你好,世界\n//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let buildTags = tfBuildTags typusFile
             if not (null buildTags)
               then do
                 let tag = case buildTags of
                             (t:_) -> t
                             [] -> error "Impossible: buildTags is not empty"
                 locValue tag `isInfixOf` "你好,世界" @?= True
               else return ()
               
  , testCase "Parser: parseTypus handles Unicode block directives" $
      let input = "//! ownership=true\n```go, message=\"你好, 世界!\"\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right _typusFile -> return ()
           
  , testCase "Encoding: Text to ByteString conversion preserves Unicode" $
      let text = T.pack "你好, 世界!"
          bytes = TE.encodeUtf8 text
          decodedText = TE.decodeUtf8 bytes
      in decodedText @?= text
      
  , testCase "Encoding: ByteString to Text conversion handles invalid UTF-8" $
      let invalidBytes = BS.pack [0xFF, 0xFE, 0xFD]
          decodedText = TE.decodeUtf8 invalidBytes
      in T.length decodedText @?= 1  -- Should handle gracefully
      
  , testCase "Encoding: round-trip conversion preserves content" $
      let originalString = "Hello, 世界! 🌍"
          text = T.pack originalString
          bytes = TE.encodeUtf8 text
          decodedText = TE.decodeUtf8 bytes
          finalString = T.unpack decodedText
      in finalString @?= originalString
      
  , testCase "Encoding: handles Unicode normalization" $
      let composed = "\233"  -- Can be composed (é) or decomposed (e + ´)
          decomposed = "e\777\601"
          result1 = safeProcessString composed
          result2 = safeProcessString decomposed
      in case (result1, result2) of
           (Right r1, Right r2) -> do
             length r1 @?= 1
             length r2 @?= 1
           _ -> assertFailure "Unicode normalization failed"
           
  , testCase "Encoding: handles zero-width characters" $
      let stringWithZeroWidth = "Hello\8203World"  -- Contains zero-width space
      in case safeProcessString stringWithZeroWidth of
           Right result -> length result @?= 1
           Left _ -> assertFailure "Zero-width character handling failed"
           
  , testCase "Encoding: handles right-to-left characters" $
      let rtlString = "مرحبا بالعالم"  -- Arabic text
      in case safeProcessString rtlString of
           Right result -> result @?= rtlString
           Left _ -> assertFailure "RTL character handling failed"
           
  , testCase "Encoding: handles emoji characters" $
      let emojiString = "Hello 🌍 World 🚀"
      in case safeProcessString emojiString of
           Right result -> result @?= emojiString
           Left _ -> assertFailure "Emoji character handling failed"
           
  , testCase "Encoding: handles combining characters" $
      let combiningString = "e\769"  -- e + combining acute accent
      in case safeProcessString combiningString of
           Right result -> length result @?= 1
           Left _ -> assertFailure "Combining character handling failed"
           
  , testCase "Encoding: handles high Unicode code points" $
      let highCodePoint = [chr 0x1F600]  -- 😀 grinning face emoji
          highCodePointString = highCodePoint
      in case safeProcessString highCodePointString of
           Right result -> length result @?= 1
           Left _ -> assertFailure "High Unicode code point handling failed"
  ]

-- Helper function
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings s = [take i s | i <- [1..length s]]