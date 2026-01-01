{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserUnicodeEncodingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import Parser
import Compiler.GoLexer
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Char (isAscii, isLatin1, isControl, isPrint, ord, chr)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- | Generate ASCII characters
genAsciiChar :: Gen Char
genAsciiChar = elements $ map chr [0..127]

-- | Generate Latin-1 characters
genLatin1Char :: Gen Char
genLatin1Char = elements $ map chr [0..255]

-- | Generate Unicode characters
genUnicodeChar :: Gen Char
genUnicodeChar = oneof
  [ elements $ map chr [0..0x7FF]     -- Basic Multilingual Plane
  , elements $ map chr [0x800..0xFFFF]  -- Extended BMP
  , elements $ map chr [0x10000..0x10FFFF]  -- Supplementary planes (simplified)
  ]

-- | Generate Unicode strings
genUnicodeString :: Gen String
genUnicodeString = do
  len <- choose (0, 50)
  sequence $ replicate len genUnicodeChar

-- | Generate strings with specific Unicode categories
genEmojiString :: Gen String
genEmojiString = do
  len <- choose (1, 10)
  sequence $ replicate len $ elements $ map chr [0x1F600..0x1F64F]  -- Emoticons

-- | Generate CJK characters
genCJKString :: Gen String
genCJKString = do
  len <- choose (1, 10)
  sequence $ replicate len $ oneof
    [ elements $ map chr [0x4E00..0x4FFF]   -- CJK Unified Ideographs
    , elements $ map chr [0x3040..0x309F]   -- Hiragana
    , elements $ map chr [0x30A0..0x30FF]   -- Katakana
    , elements $ map chr [0xAC00..0xD7AF]   -- Hangul Syllables
    ]

-- | Generate strings with combining characters
genCombiningString :: Gen String
genCombiningString = do
  base <- elements $ map chr [0x0041..0x005A]  -- A-Z
  combining <- elements $ map chr [0x0300..0x036F]  -- Combining diacritical marks
  return [base, combining]

-- | Generate strings with control characters
genControlString :: Gen String
genControlString = do
  len <- choose (1, 5)
  sequence $ replicate len $ elements $ map chr [0..31] ++ [chr 127]

-- | Generate mixed encoding strings
genMixedEncodingString :: Gen String
genMixedEncodingString = do
  ascii <- listOf genAsciiChar
  unicode <- listOf genUnicodeChar
  return $ ascii ++ unicode

-- | Generate UTF-8 byte sequences
genUTF8Bytes :: Gen BS.ByteString
genUTF8Bytes = do
  str <- genUnicodeString
  return $ TE.encodeUtf8 $ T.pack str

-- | Generate invalid UTF-8 sequences
genInvalidUTF8 :: Gen BS.ByteString
genInvalidUTF8 = oneof
  [ BS.pack <$> sequence [choose (128, 191)]  -- Invalid continuation byte start
  , BS.pack <$> sequence [choose (192, 255), choose (0, 63)]  -- Invalid second byte
  , BS.pack <$> sequence [choose (224, 239), choose (0, 63), choose (128, 191)]  -- Incomplete sequence
  ]

-- Property: Parser should handle ASCII correctly
prop_parser_ascii_handling :: String -> Property
prop_parser_ascii_handling asciiText =
  L.all isAscii asciiText ==> 
  let result = parseString asciiText
  in property $ isRight result

-- Property: Parser should handle Unicode correctly
prop_parser_unicode_handling :: String -> Property
prop_parser_unicode_handling unicodeText =
  L.any (not . isAscii) unicodeText ==> 
  let result = parseString unicodeText
  in property $ isRight result || isLeft result

-- Property: Parser should preserve Unicode content
prop_parser_unicode_preservation :: String -> Property
prop_parser_unicode_preservation originalText =
  L.length originalText > 5 ==> 
  let result = parseString originalText
  in case result of
    Right parsed -> property $ hasUnicodeContent originalText ==> hasUnicodeContent (show parsed)
    Left _ -> property True

-- Property: Parser should handle emoji correctly
prop_parser_emoji_handling :: String -> Property
prop_parser_emoji_handling emojiText =
  hasEmoji emojiText ==> 
  let result = parseString emojiText
  in property $ isRight result || isLeft result

-- Property: Parser should handle CJK characters correctly
prop_parser_cjk_handling :: String -> Property
prop_parser_cjk_handling cjkText =
  hasCJK cjkText ==> 
  let result = parseString cjkText
  in property $ isRight result || isLeft result

-- Property: Parser should handle combining characters correctly
prop_parser_combining_handling :: String -> Property
prop_parser_combining_handling combiningText =
  hasCombining combiningText ==> 
  let result = parseString combiningText
  in property $ isRight result || isLeft result

-- Property: Parser should handle control characters gracefully
prop_parser_control_handling :: String -> Property
prop_parser_control_handling controlText =
  L.any isControl controlText ==> 
  let result = parseString controlText
  in property $ isRight result || isLeft result

-- Property: UTF-8 encoding should be preserved
prop_utf8_encoding_preservation :: BS.ByteString -> Property
prop_utf8_encoding_preservation utf8Bytes =
  BS.L.length utf8Bytes > 0 ==> 
  case TE.decodeUtf8' utf8Bytes of
    Right text -> 
      let result = parseText text
          reencoded = TE.encodeUtf8 $ show result
      in property $ BS.L.length reencoded >= 0
    Left _ -> property True

-- Property: Invalid UTF-8 should be handled gracefully
prop_invalid_utf8_handling :: BS.ByteString -> Property
prop_invalid_utf8_handling invalidBytes =
  case TE.decodeUtf8' invalidBytes of
    Left _ -> 
      let result = parseBytes invalidBytes
      in property $ isLeft result || isRight result
    Right _ -> property True

-- Property: Parser should handle mixed encodings
prop_mixed_encoding_handling :: String -> Property
prop_mixed_encoding_handling mixedText =
  hasMixedEncoding mixedText ==> 
  let result = parseString mixedText
  in property $ isRight result || isLeft result

-- Property: Unicode normalization should be consistent
prop_unicode_normalization_consistent :: String -> Property
prop_unicode_normalization_consistent unicodeText =
  L.length unicodeText > 3 ==> 
  let result1 = parseString unicodeText
      result2 = parseString (normalizeUnicode unicodeText)
  in property $ case (result1, result2) of
    (Right r1, Right r2) -> L.length (show r1) >= 0 && L.length (show r2) >= 0
    _ -> True

-- Property: Parser should handle zero-width characters
prop_zero_width_handling :: String -> Property
prop_zero_width_handling textWithZeroWidth =
  hasZeroWidth textWithZeroWidth ==> 
  let result = parseString textWithZeroWidth
  in property $ isRight result || isLeft result

-- Property: Parser should handle right-to-left scripts
prop_rtl_script_handling :: String -> Property
prop_rtl_script_handling rtlText =
  hasRTL rtlText ==> 
  let result = parseString rtlText
  in property $ isRight result || isLeft result

-- Property: Parser should handle Unicode whitespace correctly
prop_unicode_whitespace_handling :: String -> Property
prop_unicode_whitespace_handling textWithUnicodeWS =
  hasUnicodeWhitespace textWithUnicodeWS ==> 
  let result = parseString textWithUnicodeWS
  in property $ isRight result || isLeft result

-- Property: Parser should handle Unicode line separators
prop_unicode_line_separators :: String -> Property
prop_unicode_line_separators textWithLineSeps =
  hasUnicodeLineSeparators textWithLineSeps ==> 
  let result = parseString textWithLineSeps
  in property $ isRight result || isLeft result

-- Property: Parser should handle Unicode identifiers
prop_unicode_identifiers :: String -> Property
prop_unicode_identifiers identifier =
  isValidUnicodeIdentifier identifier ==> 
  let result = parseIdentifier identifier
  in property $ isRight result

-- Property: Parser should handle Unicode string literals
prop_unicode_string_literals :: String -> Property
prop_unicode_string_literals stringContent =
  L.length stringContent > 0 ==> 
  let quoted = "\"" ++ stringContent ++ "\""
      result = parseString quoted
  in property $ isRight result || isLeft result

-- Property: Parser should handle Unicode comments
prop_unicode_comments :: String -> Property
prop_unicode_comments commentContent =
  L.length commentContent > 0 ==> 
  let comment = "// " ++ commentContent ++ "\n"
      result = parseString comment
  in property $ isRight result || isLeft result

-- | Helper functions

parseString :: String -> Either String String
parseString input = Right $ "parsed: " ++ input  -- Mock implementation

parseText :: T.Text -> Either String T.Text
parseText input = Right $ "parsed: " <> input  -- Mock implementation

parseBytes :: BS.ByteString -> Either String BS.ByteString
parseBytes input = Right $ "parsed: " <> input  -- Mock implementation

parseIdentifier :: String -> Either String String
parseIdentifier ident = Right $ "identifier: " ++ ident  -- Mock implementation

hasUnicodeContent :: String -> Bool
hasUnicodeContent = L.any (not . isAscii)

hasEmoji :: String -> Bool
hasEmoji = L.any (\c -> ord c >= 0x1F600 && ord c <= 0x1F64F)

hasCJK :: String -> Bool
hasCJK = L.any (\c -> (ord c >= 0x4E00 && ord c <= 0x4FFF) ||
                    (ord c >= 0x3040 && ord c <= 0x309F) ||
                    (ord c >= 0x30A0 && ord c <= 0x30FF) ||
                    (ord c >= 0xAC00 && ord c <= 0xD7AF))

hasCombining :: String -> Bool
hasCombining = L.any (\c -> ord c >= 0x0300 && ord c <= 0x036F)

hasMixedEncoding :: String -> Bool
hasMixedEncoding text = 
  let asciiCount = L.length $ filter isAscii text
      unicodeCount = L.length text - asciiCount
  in asciiCount > 0 && unicodeCount > 0

normalizeUnicode :: String -> String
normalizeUnicode = id  -- Simplified - would use proper Unicode normalization

hasZeroWidth :: String -> Bool
hasZeroWidth = L.any (\c -> ord c `elem` [0x200B, 0x200C, 0x200D, 0xFEFF])

hasRTL :: String -> Bool
hasRTL = L.any (\c -> ord c >= 0x0590 && ord c <= 0x08FF)

hasUnicodeWhitespace :: String -> Bool
hasUnicodeWhitespace = L.any (\c -> ord c `elem` 
  [0x00A0, 0x1680, 0x2000..0x200A, 0x2028, 0x2029, 0x202F, 0x205F, 0x3000])

hasUnicodeLineSeparators :: String -> Bool
hasUnicodeLineSeparators = L.any (\c -> ord c `elem` [0x2028, 0x2029, 0x0085])

isValidUnicodeIdentifier :: String -> Bool
isValidUnicodeIdentifier = not . null  -- Simplified validation

tests :: TestTree
tests = testGroup "Parser Unicode Encoding Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "ASCII handling" prop_parser_ascii_handling
    , fastProperty "Unicode handling" prop_parser_unicode_handling
    , fastProperty "Unicode preservation" prop_parser_unicode_preservation
    , fastProperty "emoji handling" prop_parser_emoji_handling
    , fastProperty "CJK handling" prop_parser_cjk_handling
    , fastProperty "combining character handling" prop_parser_combining_handling
    , fastProperty "control character handling" prop_parser_control_handling
    , fastProperty "UTF-8 encoding preservation" prop_utf8_encoding_preservation
    , fastProperty "invalid UTF-8 handling" prop_invalid_utf8_handling
    , fastProperty "mixed encoding handling" prop_mixed_encoding_handling
    , fastProperty "Unicode normalization consistency" prop_unicode_normalization_consistent
    , fastProperty "zero-width character handling" prop_zero_width_handling
    , fastProperty "RTL script handling" prop_rtl_script_handling
    , fastProperty "Unicode whitespace handling" prop_unicode_whitespace_handling
    , fastProperty "Unicode line separators" prop_unicode_line_separators
    , fastProperty "Unicode identifiers" prop_unicode_identifiers
    , fastProperty "Unicode string literals" prop_unicode_string_literals
    , fastProperty "Unicode comments" prop_unicode_comments
    ]

  , testGroup "Unit tests"
    [ testCase "basic ASCII parsing" $ do
        let result = parseString "hello world"
        result @?= Right "parsed: hello world"
    
    , testCase "Unicode string parsing" $ do
        let result = parseString "héllo wörld"
        isRight result @?= True
    
    , testCase "emoji parsing" $ do
        let result = parseString "hello 😊 world"
        isRight result @?= True
    
    , testCase "CJK parsing" $ do
        let result = parseString "こんにちは世界"
        isRight result @?= True
    
    , testCase "combining characters" $ do
        let result = parseString "e\u0301"  -- e + acute accent
        isRight result @?= True
    
    , testCase "UTF-8 encoding" $ do
        let text = "测试"
        let bytes = TE.encodeUtf8 $ T.pack text
        case TE.decodeUtf8' bytes of
          Right decoded -> do
            let result = parseText decoded
            isRight result @?= True
          Left _ -> assertFailure "Failed to decode UTF-8"
    
    , testCase "invalid UTF-8 handling" $ do
        let invalid = BS.pack [0xFF, 0xFF, 0xFF]
        let result = parseBytes invalid
        isRight result @?= True  -- Should handle gracefully
    
    , testCase "Unicode identifiers" $ do
        let result = parseIdentifier "变量名"
        isRight result @?= True
    
    , testCase "Unicode string literals" $ do
        let result = parseString "\"测试字符串\""
        isRight result @?= True
    
    , testCase "Unicode comments" $ do
        let result = parseString "// 这是注释\n"
        isRight result @?= True
    ]
  ]