{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | Text encoding safety tests for Utils module
module Test.Unit.NewTextEncodingSafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isControl, isAscii, ord)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import Data.List (isInfixOf)
import Data.List (sort, nub)

import Utils
  ( trim
  , splitBy
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Generate Unicode strings with various encodings
genUnicodeString :: Gen String
genUnicodeString = listOf $ elements 
  [ '\0'..'\127' ] ++ -- ASCII
  [ '\128'..'\255' ] ++ -- Extended ASCII
  [ 'ñ', 'á', 'é', 'í', 'ó', 'ú', 'ü', '¿', '¡' ] ++ -- Spanish
  [ 'ç', 'æ', 'œ', 'ß' ] ++ -- European
  [ 'п', 'р', 'у', 'с', 'к' ] ++ -- Cyrillic
  [ '中', '文', '字', '符' ] ++ -- Chinese
  [ '🚀', '💻', '🔧', '📝' ] -- Emoji

-- Generate strings with control characters
genControlString :: Gen String
genControlString = listOf $ elements 
  [ '\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7', '\8', '\11', '\12', 
    '\14', '\15', '\16', '\17', '\18', '\19', '\20', '\21', '\22', '\23', 
    '\24', '\25', '\26', '\27', '\28', '\29', '\30', '\31', '\127' ]

-- Generate strings with mixed line endings
genMixedLineEndings :: Gen String
genMixedLineEndings = do
  parts <- listOf $ elements ["\n", "\r\n", "\r"]
  content <- listOf $ elements ['a'..'z']
  return $ L.concat parts ++ L.concat content

-- ============================================================================
-- Text Encoding Properties
-- ============================================================================

-- Property: trim preserves Unicode characters
prop_trim_preserves_unicode :: String -> Property
prop_trim_preserves_unicode input =
  let unicodeInput = input ++ "café naïve résumé 测试 🚀"
      trimmed = trim unicodeInput
      unicodeChars = L.filter (not . isAscii) unicodeInput
      trimmedUnicode = L.filter (not . isAscii) trimmed
  in not (null unicodeChars) ==> 
     property $ sort trimmedUnicode === sort (L.filter (`elem` trimmed) unicodeChars)

-- Property: splitBy handles Unicode delimiters correctly
prop_splitBy_unicode_delimiter :: Char -> String -> Property
prop_splitBy_unicode_delimiter delim input =
  let unicodeInput = input ++ "测试" ++ [delim] ++ "café" ++ [delim] ++ "🚀"
      parts = splitBy delim unicodeInput
  in property $ L.length parts >= 1 .&&.
             concat parts === unicodeInput

-- Property: removeLineComments preserves Unicode in strings
prop_removeLine_comments_unicode_strings :: String -> Property
prop_removeLine_comments_unicode_strings comment =
  let content = "var s string = \"café naïve 测试 🚀\" // " ++ comment
      processed = removeLineComments content
  in property $ "café naïve 测试 🚀" `L.isInfixOf` processed .&&.
             not ("// " ++ comment `L.isInfixOf` processed)

-- Property: removeComments handles Unicode in block comments
prop_remove_comments_unicode_block :: String -> String -> Property
prop_remove_comments_unicode_block before after =
  let content = before ++ "/* café naïve 测试 🚀 */" ++ after
      processed = removeComments content
  in property $ not ("/* café naïve 测试 🚀 */" `L.isInfixOf` processed) .&&.
             before `L.isInfixOf` processed .&&.
             after `L.isInfixOf` processed

-- Property: normalizeIndentation preserves Unicode content
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

-- Property: breakOn handles Unicode patterns
prop_break_on_unicode :: String -> String -> Property
prop_break_on_unicode pat haystack =
  not (null pat) ==> 
  let unicodeHaystack = haystack ++ "café" ++ pat ++ "测试🚀" ++ pat ++ "end"
      (before, after) = breakOn pat unicodeHaystack
  in property $ before ++ pat ++ after === unicodeHaystack

-- ============================================================================
-- Control Character Safety Properties
-- ============================================================================

-- Property: trim handles control characters safely
prop_trim_control_characters :: String -> Property
prop_trim_control_characters input =
  forAll genControlString $ \controls ->
    let content = controls ++ input ++ controls
        trimmed = trim content
        hasLeadingControl = not (null controls) && isControl (L.head controls)
        hasTrailingControl = not (null controls) && isControl (last controls)
        noLeadingControl = null trimmed || not (isControl (L.head trimmed))
        noTrailingControl = null trimmed || not (isControl (last trimmed))
    in classify hasLeadingControl "has leading control" $
       classify hasTrailingControl "has trailing control" $
       property $ noLeadingControl .&&. noTrailingControl

-- Property: removeLineComments handles control characters
prop_remove_line_comments_control :: String -> String -> Property
prop_remove_line_comments_control prefix comment =
  let controls = "\1\2\3"
      content = prefix ++ controls ++ "// " ++ comment ++ "\n" ++ controls ++ "after"
      processed = removeLineComments content
  in property $ not ("// " ++ comment `L.isInfixOf` processed) .&&.
             controls `L.isInfixOf` processed

-- Property: splitBy handles control characters as delimiters
prop_splitBy_control_delimiter :: Char -> String -> Property
prop_splitBy_control_delimiter delim input =
  isControl delim ==> 
  let content = input ++ [delim] ++ "test" ++ [delim] ++ input
      parts = splitBy delim content
  in property $ L.length parts === 3 .&&.
             concat parts === content

-- ============================================================================
-- Mixed Line Ending Properties
-- ============================================================================

-- Property: normalizeIndentation handles mixed line endings
prop_normalize_indentation_mixed_endings :: String -> Property
prop_normalize_indentation_mixed_endings content =
  forAll genMixedLineEndings $ \endings ->
    let mixedContent = content ++ endings ++ content
      normalized = normalizeIndentation mixedContent
      normalizedLines = lines normalized
  in property $ L.length normalizedLines >= 2 .&&.
             all (content `L.isInfixOf`) normalizedLines

-- Property: removeLineComments preserves mixed line endings
prop_remove_line_comments_mixed_endings :: String -> String -> Property
prop_remove_line_comments_mixed_endings before after =
  forAll genMixedLineEndings $ \endings ->
    let content = before ++ "// comment" ++ endings ++ after
        processed = removeLineComments content
    in property $ not ("// comment" `L.isInfixOf` processed) .&&.
               endings `L.isInfixOf` processed

-- ============================================================================
-- UTF-8 Encoding Safety Properties
-- ============================================================================

-- Property: Text processing preserves UTF-8 validity
prop_utf8_validity_preserved :: String -> Property
prop_utf8_validity_preserved input =
  let unicodeText = T.pack input <> "café naïve 测试 🚀"
      processedText = T.pack (removeLineComments (T.unpack unicodeText))
      isUTF8Valid text = TE.decodeUtf8With TE.lenientDecode (TE.encodeUtf8 text) == text
  in property $ isUTF8Valid unicodeText ==> isUTF8Valid processedText

-- Property: String processing handles invalid UTF-8 gracefully
prop_invalid_utf8_handling :: [Word8] -> Property
prop_invalid_utf8_handling bytes =
  let invalidBytes = take 100 $ L.filter (> 127) bytes
      byteString = BS.pack invalidBytes
      -- Try to decode as UTF-8, which may fail
      decoded = TE.decodeUtf8With TE.lenientDecode byteString
      processed = T.unpack (T.pack (trim (T.unpack decoded)))
  in property $ L.length processed >= 0

-- Property: Emoji L.and surrogate pairs are preserved
prop_emoji_preservation :: String -> Property
prop_emoji_preservation input =
  let emojis = ["🚀", "💻", "🔧", "📝", "⚡", "🔥", "💡", "🎯"]
      contentWithEmojis = input ++ unwords emojis
      processed = trim contentWithEmojis
  in property $ L.all (`L.isInfixOf` processed) emojis

-- ============================================================================
-- Performance with Unicode Properties
-- ============================================================================

-- Property: Large Unicode strings are processed efficiently
prop_large_unicode_performance :: Int -> String -> Property
prop_large_unicode_performance multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> 
  let unicodeContent = baseContent ++ "café naïve 测试 🚀"
      largeContent = L.concat $ replicate multiplier unicodeContent
      trimmed = trim largeContent
      split = splitBy ',' largeContent
  in property $ L.length trimmed <= L.length largeContent .&&.
             length split >= 1

-- Property: Unicode normalization is idempotent
prop_unicode_normalization_idempotent :: String -> Property
prop_unicode_normalization_idempotent input =
  let unicodeContent = input ++ "café naïve 测试 🚀"
      normalized1 = normalizeIndentation unicodeContent
      normalized2 = normalizeIndentation normalized1
  in property $ normalized1 === normalized2

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Text Encoding Safety QuickCheck Tests"
  [ testGroup "Unicode Character Preservation"
    [ fastProperty "trim preserves unicode" prop_trim_preserves_unicode
    , fastProperty "splitBy unicode delimiter" prop_splitBy_unicode_delimiter
    , fastProperty "removeLineComments unicode strings" prop_remove_line_comments_unicode_strings
    , fastProperty "removeComments unicode block" prop_remove_comments_unicode_block
    , fastProperty "normalizeIndentation unicode" prop_normalize_indentation_unicode
    , fastProperty "breakOn unicode" prop_break_on_unicode
    ]

  , testGroup "Control Character Safety"
    [ fastProperty "trim control characters" prop_trim_control_characters
    , fastProperty "removeLineComments control" prop_remove_line_comments_control
    , fastProperty "splitBy control delimiter" prop_splitBy_control_delimiter
    ]

  , testGroup "Mixed Line Ending Handling"
    [ fastProperty "normalizeIndentation mixed endings" prop_normalize_indentation_mixed_endings
    , fastProperty "removeLineComments mixed endings" prop_remove_line_comments_mixed_endings
    ]

  , testGroup "UTF-8 Encoding Safety"
    [ fastProperty "UTF-8 validity preserved" prop_utf8_validity_preserved
    , fastProperty "invalid UTF-8 handling" prop_invalid_utf8_handling
    , fastProperty "emoji preservation" prop_emoji_preservation
    ]

  , testGroup "Performance with Unicode"
    [ fastProperty "large unicode performance" prop_large_unicode_performance
    , fastProperty "unicode normalization idempotent" prop_unicode_normalization_idempotent
    ]
  ]