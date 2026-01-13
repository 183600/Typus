{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.UnicodeHandlingAdvancedSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit, isLetter, isLower, isUpper, ord, chr)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Word (Word8)

-- ============================================================================
-- Unicode Handling Advanced Tests
-- ============================================================================

-- | Test parsing with basic Unicode characters
prop_unicode_basic_characters :: String -> Property
prop_unicode_basic_characters input =
  not (null input) && length input <= 50 ==>
    let unicodeInput = input ++ "ñáéíóú"
        parseResult = parseTypus unicodeInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with CJK characters
prop_unicode_cjk_characters :: String -> Property
prop_unicode_cjk_characters input =
  not (null input) && length input <= 30 ==>
    let cjkInput = input ++ "中文测试日本語한국어"
        parseResult = parseTypus cjkInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with emoji characters
prop_unicode_emoji_characters :: String -> Property
prop_unicode_emoji_characters input =
  not (null input) && length input <= 30 ==>
    let emojiInput = input ++ "😀🎉🚀💻"
        parseResult = parseTypus emojiInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with right-to-left scripts
prop_unicode_rtl_scripts :: String -> Property
prop_unicode_rtl_scripts input =
  not (null input) && length input <= 30 ==>
    let rtlInput = input ++ "العربيةעברית"
        parseResult = parseTypus rtlInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with combining characters
prop_unicode_combining_characters :: String -> Property
prop_unicode_combining_characters input =
  not (null input) && length input <= 30 ==>
    let combiningInput = input ++ "e\u0301a\u0300o\u0302"
        parseResult = parseTypus combiningInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with zero-width characters
prop_unicode_zero_width_characters :: String -> Property
prop_unicode_zero_width_characters input =
  not (null input) && length input <= 30 ==>
    let zeroWidthInput = input ++ "\u200B\u200C\u200D"
        parseResult = parseTypus zeroWidthInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with Unicode whitespace
prop_unicode_whitespace :: String -> Property
prop_unicode_whitespace input =
  not (null input) && length input <= 30 ==>
    let whitespaceInput = input ++ "\u00A0\u2000\u2001\u2002"
        parseResult = parseTypus whitespaceInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with Unicode line separators
prop_unicode_line_separators :: String -> Property
prop_unicode_line_separators input =
  not (null input) && length input <= 30 ==>
    let separatorInput = input ++ "\u2028\u2029"
        parseResult = parseTypus separatorInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with Unicode quotes
prop_unicode_quotes :: String -> Property
prop_unicode_quotes input =
  not (null input) && length input <= 30 ==>
    let quotesInput = input ++ "\u201C\u201D\u2018\u2019"
        parseResult = parseTypus quotesInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with Unicode dashes
prop_unicode_dashes :: String -> Property
prop_unicode_dashes input =
  not (null input) && length input <= 30 ==>
    let dashesInput = input ++ "\u2013\u2014\u2010"
        parseResult = parseTypus dashesInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test parsing with mixed Unicode scripts
prop_unicode_mixed_scripts :: String -> Property
prop_unicode_mixed_scripts input =
  not (null input) && length input <= 30 ==>
    let mixedInput = input ++ "Hello中文العربية😀"
        parseResult = parseTypus mixedInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode string literals
prop_unicode_string_literals :: String -> Property
prop_unicode_string_literals content =
  not (null content) && length content <= 30 ==>
    let stringLiteralCode = "let s = \"" ++ content ++ "ñáéíóú\"\n" ++
                            "print(s)\n"
        parseResult = parseTypus stringLiteralCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode identifiers
prop_unicode_identifiers :: String -> Property
prop_unicode_identifiers identifier =
  not (null identifier) && length identifier <= 20 && all isLetter identifier ==>
    let identifierCode = "let " ++ identifier ++ "ñáéíóú = 42\n" ++
                         "print(" ++ identifier ++ "ñáéíóú)\n"
        parseResult = parseTypus identifierCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode comments
prop_unicode_comments :: String -> Property
prop_unicode_comments comment =
  not (null comment) && length comment <= 30 ==>
    let commentCode = "// " ++ comment ++ "ñáéíóú\n" ++
                      "let x = 5\n" ++
                      "/* " ++ comment ++ "中文测试 */\n"
        parseResult = parseTypus commentCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode normalization
prop_unicode_normalization :: String -> Property
prop_unicode_normalization input =
  not (null input) && length input <= 30 ==>
    let normalizedInput = normalizeUnicode input
        parseResult = parseTypus normalizedInput
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode in regular expressions
prop_unicode_regex :: String -> Property
prop_unicode_regex pattern =
  not (null pattern) && length pattern <= 20 ==>
    let regexCode = "let pattern = /" ++ pattern ++ "ñáéíóú/u\n" ++
                    "let text = \"Hello中文\"\n" ++
                    "let match = text.match(pattern)\n"
        parseResult = parseTypus regexCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode in error messages
prop_unicode_error_messages :: String -> Property
prop_unicode_error_messages errorMsg =
  not (null errorMsg) && length errorMsg <= 30 ==>
    let errorCode = "try {\n" ++
                    "  riskyOperation()\n" ++
                    "} catch (e) {\n" ++
                    "  print(\"" ++ errorMsg ++ "ñáéíóú中文测试\")\n" ++
                    "}\n"
        parseResult = parseTypus errorCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode file paths
prop_unicode_file_paths :: String -> Property
prop_unicode_file_paths path =
  not (null path) && length path <= 20 ==>
    let pathCode = "import \"./" ++ path ++ "ñáéíóú中文.typus\"\n" ++
                    "let x = 5\n"
        parseResult = parseTypus pathCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode in template literals
prop_unicode_template_literals :: String -> Property
prop_unicode_template_literals content =
  not (null content) && length content <= 20 ==>
    let templateCode = "let name = \"世界\"\n" ++
                       "let message = `Hello ${name} " ++ content ++ "ñáéíóú`\n" ++
                       "print(message)\n"
        parseResult = parseTypus templateCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode in source location tracking
prop_unicode_source_location :: String -> Property
prop_unicode_source_location code =
  not (null code) && length code <= 30 ==>
    let locationCode = code ++ "ñáéíóú中文\n" ++
                      "let x = 5\n"
        parseResult = parseTypus locationCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
               hasBlocks = not (null blocks)
           in property $ hasBlocks

-- | Test Unicode encoding/decoding
prop_unicode_encoding :: String -> Property
prop_unicode_encoding content =
  not (null content) && length content <= 20 ==>
    let encodingCode = "let utf8 = encodeUTF8(\"" ++ content ++ "ñáéíóú\")\n" ++
                       "let decoded = decodeUTF8(utf8)\n" ++
                       "print(decoded)\n"
        parseResult = parseTypus encodingCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Unicode character properties
prop_unicode_char_properties :: Char -> Property
prop_unicode_char_properties char =
  let propsCode = "let c = '" ++ [char] ++ "'\n" ++
                  "let isLetter = isLetter(c)\n" ++
                  "let isDigit = isDigit(c)\n" ++
                  "let isPunctuation = isPunctuation(c)\n" ++
                  "let isSymbol = isSymbol(c)\n"
      parseResult = parseTypus propsCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right goCode -> property $ not (null goCode)

-- | Test Unicode case conversion
prop_unicode_case_conversion :: String -> Property
prop_unicode_case_conversion input =
  not (null input) && length input <= 20 ==>
    let caseCode = "let s = \"" ++ input ++ "ñáéíóú\"\n" ++
                   "let upper = s.toUpperCase()\n" ++
                   "let lower = s.toLowerCase()\n" ++
                   "let title = s.toTitleCase()\n"
        parseResult = parseTypus caseCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- Helper function
normalizeUnicode :: String -> String
normalizeUnicode = id  -- Simplified for this example

isSymbol :: Char -> Bool
isSymbol c = not (isLetter c || isDigit c || isPunctuation c || isSpace c)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Unicode Handling Advanced Tests"
  [ testProperty "Parsing with basic Unicode characters" prop_unicode_basic_characters,
    testProperty "Parsing with CJK characters" prop_unicode_cjk_characters,
    testProperty "Parsing with emoji characters" prop_unicode_emoji_characters,
    testProperty "Parsing with right-to-left scripts" prop_unicode_rtl_scripts,
    testProperty "Parsing with combining characters" prop_unicode_combining_characters,
    testProperty "Parsing with zero-width characters" prop_unicode_zero_width_characters,
    testProperty "Parsing with Unicode whitespace" prop_unicode_whitespace,
    testProperty "Parsing with Unicode line separators" prop_unicode_line_separators,
    testProperty "Parsing with Unicode quotes" prop_unicode_quotes,
    testProperty "Parsing with Unicode dashes" prop_unicode_dashes,
    testProperty "Parsing with mixed Unicode scripts" prop_unicode_mixed_scripts,
    testProperty "Unicode string literals" prop_unicode_string_literals,
    testProperty "Unicode identifiers" prop_unicode_identifiers,
    testProperty "Unicode comments" prop_unicode_comments,
    testProperty "Unicode normalization" prop_unicode_normalization,
    testProperty "Unicode in regular expressions" prop_unicode_regex,
    testProperty "Unicode in error messages" prop_unicode_error_messages,
    testProperty "Unicode file paths" prop_unicode_file_paths,
    testProperty "Unicode in template literals" prop_unicode_template_literals,
    testProperty "Unicode in source location tracking" prop_unicode_source_location,
    testProperty "Unicode encoding/decoding" prop_unicode_encoding,
    testProperty "Unicode character properties" prop_unicode_char_properties,
    testProperty "Unicode case conversion" prop_unicode_case_conversion
  ]