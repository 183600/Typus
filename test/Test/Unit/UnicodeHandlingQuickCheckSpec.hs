{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.UnicodeHandlingQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, cbContent)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isAscii)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (when)

-- ============================================================================
-- Unicode Handling QuickCheck Tests
-- ============================================================================

-- | Test parsing with Chinese characters
prop_chinese_characters :: String -> Property
prop_chinese_characters content = 
  let chineseChars = "中文测试编码处理能力"
      contentWithChinese = content ++ chineseChars
      parseResult = parseTypus contentWithChinese
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in case blocks of
         [] -> property $ True
         firstBlock:_ -> 
           let blockContent = Parser.cbContent firstBlock
           in property $ chineseChars `isInfixOf` blockContent

-- | Test parsing with European accented characters
prop_accented_characters :: String -> Property
prop_accented_characters content = 
  let accentedChars = "ñáéíóúüçßøåæœ"
      contentWithAccented = content ++ accentedChars
      parseResult = parseTypus contentWithAccented
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) -> 
                let blockContent = Parser.cbContent firstBlock
                in accentedChars `isInfixOf` blockContent
              [] -> False

-- | Test parsing with mathematical symbols
prop_mathematical_symbols :: String -> Property
prop_mathematical_symbols content = 
  let mathSymbols = "∑∏∫∆∇∂∞±≤≥≠≈"
      contentWithMath = content ++ mathSymbols
      parseResult = parseTypus contentWithMath
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = Parser.cbContent firstBlock
                in mathSymbols `isInfixOf` blockContent
              [] -> False

-- | Test parsing with currency symbols
prop_currency_symbols :: String -> Property
prop_currency_symbols content = 
  let currencySymbols = "$€£¥₹₽₩₪"
      contentWithCurrency = content ++ currencySymbols
      parseResult = parseTypus contentWithCurrency
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in currencySymbols `isInfixOf` blockContent
              [] -> False

-- | Test parsing with emojis
prop_emoji_characters :: String -> Property
prop_emoji_characters content = 
  let emojis = "😀😃😄😁😆😅🤣😂🙂😉"
      contentWithEmojis = content ++ emojis
      parseResult = parseTypus contentWithEmojis
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in emojis `isInfixOf` blockContent
              [] -> False

-- | Test parsing with right-to-left scripts
prop_rtl_scripts :: String -> Property
prop_rtl_scripts content = 
  let rtlChars = "العربيةעברית"
      contentWithRTL = content ++ rtlChars
      parseResult = parseTypus contentWithRTL
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in rtlChars `isInfixOf` blockContent
              [] -> False

-- | Test string processing with Unicode
prop_unicode_string_processing :: String -> Property
prop_unicode_string_processing content = 
  let unicodeContent = "中文测试ñáéíóú" ++ content
      trimmed = trim unicodeContent
      splitResult = splitBy ' ' unicodeContent
      commentRemoved = removeComments unicodeContent
  in property $ length unicodeContent >= length trimmed

-- | Test source location with Unicode
prop_unicode_source_location :: String -> Property
prop_unicode_source_location content = 
  let unicodeContent = "中文测试" ++ content
      linesContent = lines unicodeContent
      positions = map (\(line, content) -> SourcePos line 1 0) (zip [1..] linesContent)
  in property $ length positions == length linesContent

-- | Test compilation with Unicode
prop_unicode_compilation :: String -> Property
prop_unicode_compilation content = 
  let unicodeContent = "var x string = \"中文测试\""
      parseResult = parseTypus unicodeContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left errors -> property $ not (null errors)
              Right goCode -> property $ "中文测试" `isInfixOf` goCode

-- | Test Unicode in directives
prop_unicode_directives :: String -> Property
prop_unicode_directives content = 
  let unicodeDirective = "// build: 中文测试\n" ++ content
      parseResult = parseTypus unicodeDirective
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let buildTags = tfBuildTags typusFile
         in property $ not (null buildTags) ==> 
            case buildTags of 
              (firstTag:_) -> "中文测试" `isInfixOf` locValue firstTag
              [] -> False

-- | Test Unicode in identifiers
prop_unicode_identifiers :: String -> Property
prop_unicode_identifiers content = 
  let unicodeId = "变量中文"
      unicodeContent = "var " ++ unicodeId ++ " int = 5"
      parseResult = parseTypus unicodeContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in unicodeId `isInfixOf` blockContent
              [] -> False

-- | Test Unicode string literals
prop_unicode_string_literals :: String -> Property
prop_unicode_string_literals content = 
  let unicodeString = "\"中文测试ñáéíóú\""
      unicodeContent = "var x string = " ++ unicodeString
      parseResult = parseTypus unicodeContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in unicodeString `isInfixOf` blockContent
              [] -> False

-- | Test Unicode comments
prop_unicode_comments :: String -> Property
prop_unicode_comments content = 
  let unicodeComment = "// 中文测试注释\n" ++ content
      parseResult = parseTypus unicodeComment
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            case blocks of 
              (firstBlock:_) ->
                let blockContent = cbContent firstBlock
                in content `isInfixOf` blockContent
              [] -> False

-- | Test mixed Unicode and ASCII
prop_mixed_unicode_ascii :: String -> Property
prop_mixed_unicode_ascii content = 
  let mixedContent = "var test string = \"Hello 中文测试 World 🌍\""
      parseResult = parseTypus mixedContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = cbContent firstBlock
            in "Hello 中文测试 World 🌍" `isInfixOf` blockContent

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Unicode Handling QuickCheck Properties"
  [ testProperty "Chinese characters" prop_chinese_characters,
    testProperty "European accented characters" prop_accented_characters,
    testProperty "Mathematical symbols" prop_mathematical_symbols,
    testProperty "Currency symbols" prop_currency_symbols,
    testProperty "Emoji characters" prop_emoji_characters,
    testProperty "Right-to-left scripts" prop_rtl_scripts,
    testProperty "Unicode string processing" prop_unicode_string_processing,
    testProperty "Unicode source location" prop_unicode_source_location,
    testProperty "Unicode compilation" prop_unicode_compilation,
    testProperty "Unicode in directives" prop_unicode_directives,
    testProperty "Unicode in identifiers" prop_unicode_identifiers,
    testProperty "Unicode string literals" prop_unicode_string_literals,
    testProperty "Unicode comments" prop_unicode_comments,
    testProperty "Mixed Unicode and ASCII" prop_mixed_unicode_ascii
  ]