{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.TextProcessingSafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, frequency)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isAscii, isControl, ord, chr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.String (IsString)

-- | Generate Unicode strings including edge cases
newtype UnicodeString = UnicodeString { getUnicodeString :: String }
  deriving (Show, Eq)

instance Arbitrary UnicodeString where
  arbitrary = UnicodeString <$> unicodeString
    where
      unicodeString :: Gen String
      unicodeString = listOf $ oneof
        [ -- ASCII characters
          choose ('\32', '\126')
        , -- Common Unicode punctuation
          elements ['"', ''', '`', '´', '¨', '¯', '¸', '·', '¸', '¸']
        , -- Unicode spaces (non-breaking space, en space, em space)
          elements ['\160', '\8194', '\8195', '\8201', '\8239']
        , -- Control characters (careful selection)
          elements ['\t', '\n', '\r']
        , -- Common Unicode symbols
          elements ['—', '–', '…', '‰', '‡', '†', '•', '‣', '‰']
        ]

-- | Generate strings with potential encoding issues
newtype EncodingTestString = EncodingTestString { getEncodingTestString :: String }
  deriving (Show, Eq)

instance Arbitrary EncodingTestString where
  arbitrary = EncodingTestString <$> encodingString
    where
      encodingString :: Gen String
      encodingString = listOf $ frequency
        [ (80, choose ('\32', '\126'))  -- Mostly ASCII
        , (10, elements ['\128', '\255']) -- Extended ASCII
        , (5, choose ('\256', '\511'))   -- Basic multilingual plane
        , (3, elements ['\8232', '\8233']) -- Line separator and paragraph separator
        , (2, elements ['\xFEFF'])       -- Zero-width no-break space (BOM)
        ]

-- Property: trim preserves Unicode spaces correctly
prop_trim_preserves_unicode_content :: UnicodeString -> UnicodeString -> Property
prop_trim_preserves_unicode_content prefix suffix =
  let content = getUnicodeString prefix ++ "测试内容" ++ getUnicodeString suffix
      trimmed = trim content
      expectedContent = "测试内容"
  in classify (not (null (getUnicodeString prefix))) "has leading unicode" $
     classify (not (null (getUnicodeString suffix))) "has trailing unicode" $
     counterexample ("Original: " ++ show content) $
     counterexample ("Trimmed: " ++ show trimmed) $
     property $ trimmed === expectedContent

-- Property: splitBy handles Unicode delimiters correctly
prop_splitBy_unicode_delimiters :: UnicodeString -> UnicodeString -> Property
prop_splitBy_unicode_delimiters delim content =
  let delimStr = getUnicodeString delim
      contentStr = getUnicodeString content
      -- Ensure delimiter is not empty
  in not (null delimStr) ==>
     let result = splitBy (head delimStr) contentStr
         reconstructed = intercalate [head delimStr] result
     in counterexample ("Delimiter: " ++ show delimStr) $
        counterexample ("Content: " ++ show contentStr) $
        counterexample ("Result: " ++ show result) $
        property $ reconstructed === contentStr
  where
    intercalate :: String -> [String] -> String
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property: removeComments handles Unicode in string literals
prop_removeComments_preserves_unicode_strings :: EncodingTestString -> Property
prop_removeComments_preserves_unicode_strings content =
  let testStr = getEncodingTestString content
      codeWithComments = "let x = \"" ++ testStr ++ "\" // comment with 中文"
      result = removeLineComments codeWithComments
      expected = "let x = \"" ++ testStr ++ "\" "
  in counterexample ("Original: " ++ show codeWithComments) $
     counterexample ("Result: " ++ show result) $
     counterexample ("Expected: " ++ show expected) $
     property $ result === expected

-- Property: normalizeIndentation preserves Unicode content
prop_normalizeIndentation_unicode_preservation :: UnicodeString -> UnicodeString -> Property
prop_normalizeIndentation_unicode_preservation indent content =
  let indentStr = getUnicodeString indent
      contentStr = getUnicodeString content
      -- Create indented content with Unicode
      indentedLines = map (\line -> indentStr ++ line) (lines contentStr)
      input = unlines indentedLines
      result = normalizeIndentation input
      -- Unicode content should be preserved
      hasUnicode = any (not . isAscii) contentStr
  in classify hasUnicode "contains unicode characters" $
     counterexample ("Input: " ++ show input) $
     counterexample ("Result: " ++ show result) $
     property $ contentStr `isInfixOf` result

-- Property: breakOn handles Unicode correctly
prop_breakOn_unicode_safety :: UnicodeString -> UnicodeString -> UnicodeString -> Property
prop_breakOn_unicode_safety prefix delimiter suffix =
  let prefixStr = getUnicodeString prefix
      delimStr = getUnicodeString delimiter
      suffixStr = getUnicodeString suffix
      input = prefixStr ++ delimStr ++ suffixStr
  in not (null delimStr) ==>
     let (before, after) = breakOn (head delimStr) input
         expectedBefore = prefixStr
         expectedAfter = delimStr ++ suffixStr
     in counterexample ("Input: " ++ show input) $
        counterexample ("Delim: " ++ show delimStr) $
        counterexample ("Before: " ++ show before ++ ", Expected: " ++ show expectedBefore) $
        counterexample ("After: " ++ show after ++ ", Expected: " ++ show expectedAfter) $
        property $ before === expectedBefore .&&. after === expectedAfter

-- Property: Functions handle zero-width characters correctly
prop_zero_width_character_handling :: UnicodeString -> Property
prop_zero_width_character_handling content =
  let baseStr = getUnicodeString content
      -- Add zero-width characters
      withZW = baseStr ++ "\xFEFF" ++ baseStr ++ "\u200B" ++ baseStr
  in not (null baseStr) ==>
     let trimmed = trim withZW
         splitResult = splitBy '\u200B' withZW
     in counterexample ("With zero-width: " ++ show withZW) $
        counterexample ("Trimmed: " ++ show trimmed) $
        counterexample ("Split: " ++ show splitResult) $
        property $ length splitResult >= 2 .&&. baseStr `isInfixOf` trimmed

-- Property: Encoding safety - functions don't crash on malformed input
prop_encoding_safety :: EncodingTestString -> Property
prop_encoding_safety content =
  let testStr = getEncodingTestString content
      -- Test all functions don't crash
      trimResult = trim testStr
      splitResult = splitBy ',' testStr
      commentResult = removeLineComments testStr
      indentResult = normalizeIndentation testStr
      breakResult = breakOn 'x' testStr
  in property $ 
       length trimResult >= 0 .&&.
       length splitResult >= 0 .&&.
       length commentResult >= 0 .&&.
       length indentResult >= 0 .&&.
       length (fst breakResult) >= 0 .&&.
       length (snd breakResult) >= 0

tests :: TestTree
tests = testGroup "Text Processing Safety QuickCheck Tests"
  [ fastProperty "trim preserves Unicode content" prop_trim_preserves_unicode_content
  , fastProperty "splitBy handles Unicode delimiters" prop_splitBy_unicode_delimiters
  , fastProperty "removeComments preserves Unicode strings" prop_removeComments_preserves_unicode_strings
  , fastProperty "normalizeIndentation preserves Unicode" prop_normalizeIndentation_unicode_preservation
  , fastProperty "breakOn handles Unicode correctly" prop_breakOn_unicode_safety
  , fastProperty "zero-width character handling" prop_zero_width_character_handling
  , fastProperty "encoding safety" prop_encoding_safety
  , testGroup "Manual Unicode tests"
      [ testCase "trim handles various Unicode spaces" $ do
          let input = "\u00A0\u2003测试\u2002\u202F"  -- NBSP, EM SPACE, EN SPACE, NARROW NO-BREAK SPACE
              result = trim input
          assertBool "trim should remove Unicode spaces" $ result == "测试"
          
      , testCase "splitBy handles Unicode line separators" $ do
          let input = "第一\u2028第二\u2029第三"  -- Line separator, paragraph separator
              result = splitBy '\u2028' input
          assertBool "should split on line separator" $ result == ["第一", "第二\u2029第三"]
          
      , testCase "removeComments handles Unicode in comments" $ do
          let input = "code // 注释 with 🚀 and test"
              result = removeLineComments input
              expected = "code "
          assertBool "should handle Unicode in comments" $ result == expected
    ]
  ]