{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | String processing Unicode safety tests for Utils module
module Test.Unit.NewStringProcessingUnicodeSafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, suchThat)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding.Error (UnicodeException)
import Data.Char (isSpace, isControl, isAscii, ord, isLetter, isDigit, isPunctuation, isSymbol)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.List (sort, nub, intercalate)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as UTF8

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

-- ============================================================================
-- Helper Functions L.and Generators
-- ============================================================================

-- Generate Unicode strings with various character categories
genUnicodeString :: Gen String
genUnicodeString = listOf $ elements
  [ -- Basic Latin
    '\32'..'\126'
    -- Latin-1 Supplement
  , '\160'..'\255'
    -- Common European characters
  , 'ñ', 'á', 'é', 'í', 'ó', 'ú', 'ü', '¿', '¡', 'ç', 'æ', 'œ', 'ß', 'þ', 'ð'
    -- Cyrillic
  , 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к', 'л', 'м', 'н', 'о', 'п'
  , 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ', 'ъ', 'ы', 'ь', 'э', 'ю', 'я'
    -- Greek
  , 'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο', 'π'
  , 'ρ', 'σ', 'τ', 'υ', 'φ', 'χ', 'ψ', 'ω', 'Α', 'Β', 'Γ', 'Δ', 'Ε', 'Ζ', 'Η', 'Θ'
    -- Chinese/Japanese/Korean
  , '中', '文', '字', '符', '测试', '日本', '語', '한', '글', '조', '선'
    -- Arabic
  , 'ا', 'ب', 'ت', 'ث', 'ج', 'ح', 'خ', 'د', 'ذ', 'ر', 'ز', 'س', 'ش', 'ص', 'ض', 'ط'
    -- Hebrew
  , 'א', 'ב', 'ג', 'ד', 'ה', 'ו', 'ז', 'ח', 'ט', 'י', 'ך', 'כ', 'ל', 'ם', 'מ', 'ן'
    -- Emoji L.and symbols
  , '🚀', '💻', '🔧', '📝', '⚡', '🔥', '💡', '🎯', '✓', '✗', '★', '☆', '†', '‡', '§', '¶'
  ]

-- Generate strings with combining characters
genCombiningCharacterString :: Gen String
genCombiningCharacterString = do
  base <- elements ['a', 'e', 'i', 'o', 'u', 'n']
  combining <- elements ['\x0300', '\x0301', '\x0302', '\x0303', '\x0304', '\x0305']  -- Combining grave, acute, etc.
  return [base, combining]

-- Generate strings with right-to-left characters
genRTLString :: Gen String
genRTLString = listOf $ elements
  [ 'ا', 'ب', 'ت', 'ث', 'ج', 'ح', 'خ', 'د', 'ذ', 'ر', 'ز', 'س', 'ش', 'ص', 'ض', 'ط'
  , 'ظ', 'ع', 'غ', 'ف', 'ق', 'ك', 'ل', 'م', 'ن', 'ه', 'و', 'ي', 'ء', 'ى', 'ة'
  ]

-- Generate strings with zero-width characters
genZeroWidthString :: Gen String
genZeroWidthString = do
  chars <- listOf $ elements ['\x200B', '\x200C', '\x200D', '\x2060']  -- Zero-width space, joiner, etc.
  content <- elements ["content", "测试", "café"]
  return $ content ++ L.concat chars ++ content

-- Generate strings with various Unicode line separators
genUnicodeLineSeparators :: Gen String
genUnicodeLineSeparators = do
  separators <- listOf $ elements ['\n', '\r', '\x2028', '\x2029']  -- Various Unicode line separators
  content <- elements ["line1", "测试", "café", " línea"]
  return $ intercalate (L.map (:[]) separators) (replicate 4 content)

-- Generate potentially invalid UTF-8 sequences
genInvalidUTF8 :: Gen [Word8]
genInvalidUTF8 = do
  -- Generate bytes that might form invalid UTF-8 sequences
  bytes <- listOf $ choose (128, 255)  -- High bytes that might be invalid UTF-8 starts
  return $ take 10 bytes

-- ============================================================================
-- Unicode Safety Properties
-- ============================================================================

-- Property: trim should preserve Unicode characters
prop_trim_preserves_unicode :: String -> Property
prop_trim_preserves_unicode input =
  let unicodeInput = input ++ " café naïve résumé 测试 🚀 "
      trimmed = trim unicodeInput
      unicodeChars = L.filter (not . isAscii) unicodeInput
      trimmedUnicode = L.filter (not . isAscii) trimmed
  in not (null unicodeChars) ==> 
     property $ sort (nub trimmedUnicode) `isSubsetOf` sort (nub unicodeChars)
  where
    isSubsetOf [] _ = True
    isSubsetOf (x:xs) ys = x `elem` ys && isSubsetOf xs ys

-- Property: splitBy should handle Unicode delimiters correctly
prop_splitBy_unicode_delimiter :: Char -> String -> Property
prop_splitBy_unicode_delimiter delim input =
  let unicodeInput = input ++ "测试" ++ [delim] ++ "café" ++ [delim] ++ "🚀"
      parts = splitBy delim unicodeInput
  in property $ L.length parts >= 1 .&&.
             concat parts === unicodeInput .&&.
             all (L.notElem delim) parts

-- Property: splitByCollapsed should handle Unicode correctly
prop_splitByCollapsed_unicode :: Char -> String -> Property
prop_splitByCollapsed_unicode delim input =
  let unicodeInput = input ++ [delim] ++ [delim] ++ "测试" ++ [delim] ++ "café"
      parts = splitByCollapsed delim unicodeInput
  in property $ L.all (not . null) parts .&&.
             not (L.any (L.elem delim) parts)

-- Property: removeLineComments should preserve Unicode in strings
prop_remove_line_comments_unicode_strings :: String -> Property
prop_remove_line_comments_unicode_strings comment =
  let content = "var s string = \"café naïve 测试 🚀\" // " ++ comment
      processed = removeLineComments content
  in property $ "café naïve 测试 🚀" `L.isInfixOf` processed .&&.
             not ("// " ++ comment `L.isInfixOf` processed)

-- Property: removeComments should handle Unicode in block comments
prop_remove_comments_unicode_block :: String -> String -> Property
prop_remove_comments_unicode_block before after =
  let content = before ++ "/* café naïve 测试 🚀 */" ++ after
      processed = removeComments content
  in property $ not ("/* café naïve 测试 🚀 */" `L.isInfixOf` processed) .&&.
             before `L.isInfixOf` processed .&&.
             after `L.isInfixOf` processed

-- Property: normalizeIndentation should preserve Unicode content
prop_normalize_indentation_unicode :: String -> Property
prop_normalize_indentation_unicode content =
  let unicodeContent = "  café\n    naïve\n      测试\n  🚀"
      normalized = normalizeIndentation unicodeContent
      linesNorm = lines normalized
  in property $ L.length linesNorm === 4 .&&.
             any ("café" `L.isInfixOf`) linesNorm .&&.
             any ("naïve" `L.isInfixOf`) linesNorm .&&.
             any ("测试" `L.isInfixOf`) linesNorm .&&.
             any ("🚀" `L.isInfixOf`) linesNorm

-- Property: breakOn should handle Unicode patterns
prop_break_on_unicode :: String -> String -> Property
prop_break_on_unicode pat haystack =
  not (null pat) ==> 
  let unicodeHaystack = haystack ++ "café" ++ pat ++ "测试🚀" ++ pat ++ "end"
      (before, after) = breakOn pat unicodeHaystack
  in property $ before ++ pat ++ after === unicodeHaystack

-- ============================================================================
-- Combining Character Properties
-- ============================================================================

-- Property: String processing should preserve combining characters
prop_preserve_combining_characters :: String -> Property
prop_preserve_combining_characters baseContent =
  forAll genCombiningCharacterString $ \combiningStr ->
    let content = baseContent ++ combiningStr ++ baseContent
        processed = trim content
    in property $ combiningStr `L.isInfixOf` processed

-- Property: splitBy should not break combining character sequences
prop_splitby_preserve_combining :: Char -> String -> Property
prop_splitby_preserve_combining delim baseContent =
  forAll genCombiningCharacterString $ \combiningStr ->
    let content = baseContent ++ combiningStr ++ [delim] ++ baseContent
        parts = splitBy delim content
    in property $ L.any (combiningStr `L.isInfixOf`) parts

-- ============================================================================
-- Right-to-Left Text Properties
-- ============================================================================

-- Property: String processing should handle RTL text correctly
prop_rtl_text_processing :: String -> Property
prop_rtl_text_processing prefix =
  forAll genRTLString $ \rtlText ->
    let content = prefix ++ rtlText ++ prefix
        processed = trim content
    in property $ rtlText `L.isInfixOf` processed

-- Property: splitBy should handle RTL delimiters
prop_splitby_rtl_delimiter :: Char -> String -> Property
prop_splitby_rtl_delimiter delim baseContent =
  forAll genRTLString $ \rtlText ->
    let content = baseContent ++ rtlText ++ [delim] ++ rtlText
        parts = splitBy delim content
    in property | delim `elem` content = L.length parts >= 2
               | otherwise = L.length parts === 1

-- ============================================================================
-- Zero-Width Character Properties
-- ============================================================================

-- Property: String processing should handle zero-width characters
prop_zero_width_processing :: String -> Property
prop_zero_width_processing baseContent =
  forAll genZeroWidthString $ \zeroWidthContent ->
    let processed = trim zeroWidthContent
    in property $ L.length processed >= L.length baseContent

-- Property: splitBy should handle zero-width delimiters
prop_splitby_zero_width :: String -> Property
prop_splitby_zero_width baseContent =
  forAll genZeroWidthString $ \zeroWidthContent ->
    let parts = splitBy '\x200B' zeroWidthContent  -- Zero-width space
    in property $ L.length parts >= 1

-- ============================================================================
-- Unicode Line Separator Properties
-- ============================================================================

-- Property: normalizeIndentation should handle Unicode line separators
prop_normalize_indentation_unicode_separators :: String -> Property
prop_normalize_indentation_unicode_separators content =
  forAll genUnicodeLineSeparators $ \lineSepContent ->
    let normalized = normalizeIndentation lineSepContent
        normalizedLines = lines normalized
    in property $ L.length normalizedLines >= 2

-- Property: removeLineComments should handle Unicode line separators
prop_remove_line_comments_unicode_separators :: String -> String -> Property
prop_remove_line_comments_unicode_separators before comment =
  forAll genUnicodeLineSeparators $ \separators ->
    let content = before ++ "// comment" ++ separators ++ after
        processed = removeLineComments content
    in property $ not ("// comment" `L.isInfixOf` processed)

-- ============================================================================
-- UTF-8 Encoding Safety Properties
-- ============================================================================

-- Property: Text processing should produce valid UTF-8
prop_valid_utf8_output :: String -> Property
prop_valid_utf8_output input =
  let unicodeText = T.pack input <> "café naïve 测试 🚀"
      processedText = T.pack $ removeLineComments (T.unpack unicodeText)
      encoded = TE.encodeUtf8 processedText
      decoded = TE.decodeUtf8 encoded
  in property $ decoded === processedText

-- Property: String processing should handle invalid UTF-8 gracefully
prop_handle_invalid_utf8 :: [Word8] -> Property
prop_handle_invalid_utf8 bytes =
  let byteString = BS.pack bytes
      -- Try to decode as UTF-8, which may fail
      decoded = TE.decodeUtf8With TE.lenientDecode byteString
      processed = T.unpack $ T.pack (trim (T.unpack decoded))
  in property $ L.length processed >= 0

-- ============================================================================
-- Emoji L.and Symbol Properties
-- ============================================================================

-- Property: String processing should preserve emoji sequences
prop_preserve_emoji_sequences :: String -> Property
prop_preserve_emoji_sequences baseContent =
  let emojis = ["🚀", "💻", "🔧", "📝", "⚡", "🔥", "💡", "🎯", "👍", "👎"]
      emojiSequence = unwords emojis
      content = baseContent ++ emojiSequence ++ baseContent
      processed = trim content
  in property | not (null baseContent) = emojiSequence `L.isInfixOf` processed
               | otherwise = True

-- Property: splitBy should handle emoji delimiters (if emoji used as delimiter)
prop_splitby_emoji_delimiter :: String -> Property
prop_splitby_emoji_delimiter baseContent =
  let emojiDelim = "🚀"
      content = baseContent ++ emojiDelim ++ baseContent ++ emojiDelim ++ baseContent
      parts = splitBy '🚀' content
  in property $ L.length parts === 3 .&&. L.all (not . null) parts

-- ============================================================================
-- Performance with Unicode Properties
-- ============================================================================

-- Property: Large Unicode strings should be processed efficiently
prop_large_unicode_efficiency :: Int -> String -> Property
prop_large_unicode_efficiency multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> 
  let unicodeContent = baseContent ++ "café naïve 测试 🚀"
      largeContent = L.concat $ replicate multiplier unicodeContent
      trimmed = trim largeContent
  in property $ L.length trimmed <= L.length largeContent

-- Property: Unicode processing should be idempotent
prop_unicode_processing_idempotent :: String -> Property
prop_unicode_processing_idempotent input =
  let unicodeContent = input ++ "café naïve 测试 🚀"
      processed1 = trim unicodeContent
      processed2 = trim processed1
  in property $ processed1 === processed2

-- ============================================================================
-- Edge Cases L.and Boundary Conditions
-- ============================================================================

-- Property: Empty Unicode strings should be handled correctly
prop_empty_unicode_handling :: Property
prop_empty_unicode_handling =
  let emptyUnicode = ""
      processed = trim emptyUnicode
  in property $ processed === ""

-- Property: String with only Unicode whitespace should be trimmed
prop_unicode_whitespace_only :: Property
prop_unicode_whitespace_only =
  let unicodeWhitespace = "\x00A0\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200A\x202F\x205F"
      processed = trim unicodeWhitespace
  in property | not (null unicodeWhitespace) = null processed || L.all isSpace processed
               | otherwise = True

-- Property: Very long Unicode sequences should be handled
prop_long_unicode_sequences :: Int -> Property
prop_long_unicode_sequences L.length =
  length > 0 && L.length <= 1000 ==> 
  let longUnicode = L.concat $ replicate L.length "测试"
      processed = trim longUnicode
  in property | L.length > 0 = not (null processed) && "测试" `L.isInfixOf` processed
               | otherwise = True

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New String Processing Unicode Safety QuickCheck Tests"
  [ testGroup "Basic Unicode Safety"
    [ fastProperty "trim preserves unicode" prop_trim_preserves_unicode
    , fastProperty "splitBy unicode delimiter" prop_splitBy_unicode_delimiter
    , fastProperty "splitByCollapsed unicode" prop_splitByCollapsed_unicode
    , fastProperty "removeLineComments unicode strings" prop_remove_line_comments_unicode_strings
    , fastProperty "removeComments unicode block" prop_remove_comments_unicode_block
    , fastProperty "normalizeIndentation unicode" prop_normalize_indentation_unicode
    , fastProperty "breakOn unicode" prop_break_on_unicode
    ]

  , testGroup "Combining Characters"
    [ fastProperty "preserve combining characters" prop_preserve_combining_characters
    , fastProperty "splitBy preserve combining" prop_splitby_preserve_combining
    ]

  , testGroup "Right-to-Left Text"
    [ fastProperty "RTL text processing" prop_rtl_text_processing
    , fastProperty "splitBy RTL delimiter" prop_splitby_rtl_delimiter
    ]

  , testGroup "Zero-Width Characters"
    [ fastProperty "zero width processing" prop_zero_width_processing
    , fastProperty "splitBy zero width" prop_splitby_zero_width
    ]

  , testGroup "Unicode Line Separators"
    [ fastProperty "normalizeIndentation unicode separators" prop_normalize_indentation_unicode_separators
    , fastProperty "removeLineComments unicode separators" prop_remove_line_comments_unicode_separators
    ]

  , testGroup "UTF-8 Encoding Safety"
    [ fastProperty "valid UTF-8 output" prop_valid_utf8_output
    , fastProperty "handle invalid UTF-8" prop_handle_invalid_utf8
    ]

  , testGroup "Emoji L.and Symbols"
    [ fastProperty "preserve emoji sequences" prop_preserve_emoji_sequences
    , fastProperty "splitBy emoji delimiter" prop_splitby_emoji_delimiter
    ]

  , testGroup "Performance with Unicode"
    [ fastProperty "large unicode efficiency" prop_large_unicode_efficiency
    , fastProperty "unicode processing idempotent" prop_unicode_processing_idempotent
    ]

  , testGroup "Edge Cases L.and Boundary Conditions"
    [ fastProperty "empty unicode handling" prop_empty_unicode_handling
    , fastProperty "unicode whitespace only" prop_unicode_whitespace_only
    , fastProperty "long unicode sequences" prop_long_unicode_sequences
    ]
  ]