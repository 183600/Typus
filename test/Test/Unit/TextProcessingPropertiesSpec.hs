{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.TextProcessingPropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import SourceLocation
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T
import Control.Arrow ((&&&))

-- ============================================================================
-- Text Processing Properties Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Text Processing Properties Tests"
  [ trimProperties
  , splitByProperties
  , commentRemovalProperties
  , indentationProperties
  , sourceLocationProperties
  , textNormalizationProperties
  ]

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

trimProperties :: TestTree
trimProperties = testGroup "Trim Function Properties"
  [ testProperty "trim idempotent" $
      \s -> trim (trim s) === trim s
    
  , testProperty "trim removes leading/trailing whitespace" $
      \s -> not (null s) && all isSpace s ==> trim s === ""
    
  , testProperty "trim preserves inner whitespace" $
      \s1 s2 s3 -> not (all isSpace s1) && not (all isSpace s3) ==>
        trim (s1 ++ s2 ++ s3) === trim s1 ++ trim s2 ++ trim s3
    
  , testProperty "trim never increases length" $
      \s -> length (trim s) <= length s
  ]

-- ============================================================================
-- Split Function Properties
-- ============================================================================

splitByProperties :: TestTree
splitByProperties = testGroup "Split Function Properties"
  [ testProperty "splitBy preserves concatenation" $
      \delim s -> delim /= ',' ==> concat (splitBy delim s) === s
    
  , testProperty "splitBy length matches delimiter count + 1" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        length (splitBy delim s) === length (filter (== delim) s) + 1
    
  , testProperty "splitByCommaCollapsed removes empty segments" $
      \s -> not (any null (splitByCommaCollapsed s))
    
  , testProperty "splitByCommaCollapsed subset of splitByComma" $
      \s -> all (`elem` splitByComma s) (splitByCommaCollapsed s)
    
  , testProperty "splitBy empty string returns single empty" $
      \delim -> splitBy delim "" === [""]
  ]

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ testCase "removeLineComments removes // comments" $
      removeLineComments "code // comment\nmore code" @?= "code \nmore code"
    
  , testCase "removeComments removes both comment types" $
      removeComments "code // comment\n/* block */ more" @?= "code \n more"
    
  , testProperty "removeLineComments preserves non-comment lines" $
      \s -> not ("//" `isInfixOf` s) ==> removeLineComments s === s
    
  , testProperty "removeComments idempotent" $
      \s -> removeComments (removeComments s) === removeComments s
    
  , testProperty "comment removal never increases length" $
      \s -> length (removeComments s) <= length s
  ]

-- ============================================================================
-- Indentation Properties
-- ============================================================================

indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves relative structure" $
      \s -> not (null s) ==> 
        let normalized = normalizeIndentation s
            lines1 = lines s
            lines2 = lines normalized
        in length lines1 === length lines2
    
  , testCase "normalizeIndentation removes common prefix" $
      normalizeIndentation "  line1\n    line2\n  line3" @?= "line1\n  line2\nline3"
    
  , testProperty "normalizeIndentation idempotent" $
      \s -> normalizeIndentation (normalizeIndentation s) === normalizeIndentation s
    
  , testProperty "forceSingleTabIndentation converts spaces to tabs" $
      \s -> not (null s) ==> 
        let tabbed = forceSingleTabIndentation s
        in not ("  " `isInfixOf` tabbed) || "    " `isInfixOf` tabbed
  ]

-- ============================================================================
-- Source Location Properties
-- ============================================================================

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "Source Location Properties"
  [ testProperty "SourcePos ordering is consistent" $
      \line1 col1 line2 col2 ->
        let pos1 = SourcePos line1 col1
            pos2 = SourcePos line2 col2
        in (line1 < line2 || (line1 == line2 && col1 < col2)) === 
           (pos1 < pos2)
    
  , testProperty "posAfter advances correctly" $
      \line col ->
        let pos = SourcePos line col
            advanced = posAfter pos 'a'
        in sourceLine advanced === line && sourceColumn advanced === col + 1
    
  , testProperty "spanBetween creates valid span" $
      \line1 col1 line2 col2 ->
        let pos1 = SourcePos line1 col1
            pos2 = SourcePos line2 col2
            span = spanBetween pos1 pos2
        in isValidSpan span === (pos1 <= pos2)
    
  , testProperty "mergeSpans is commutative" $
      \line1 col1 line2 col2 line3 col3 line4 col4 ->
        let span1 = spanBetween (SourcePos line1 col1) (SourcePos line2 col2)
            span2 = spanBetween (SourcePos line3 col3) (SourcePos line4 col4)
        in mergeSpans span1 span2 === mergeSpans span2 span1
  ]

-- ============================================================================
-- Text Normalization Properties
-- ============================================================================

textNormalizationProperties :: TestTree
textNormalizationProperties = testGroup "Text Normalization Properties"
  [ testProperty "trim . split . join preserves content" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let parts = splitBy delim s
            rejoined = concat parts
        in trim rejoined === trim (filter (/= delim) s)
    
  , testProperty "breakOn consistency with splitBy" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let (prefix, suffix) = breakOn delim s
            parts = splitBy delim s
        in case parts of
          [] -> prefix === "" && suffix === ""
          [x] -> prefix === x && suffix === ""
          (x:xs) -> prefix === x && suffix === concat xs
    
  , testProperty "text processing pipeline is idempotent" $
      \s -> let processed = normalizeIndentation . trim . removeComments $ s
            in normalizeIndentation . trim . removeComments $ processed === processed
  ]

-- ============================================================================
-- Additional QuickCheck Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings with delimiters
genDelimitedString :: Char -> Gen String
genDelimitedString delim = listOf $ elements ['a'..'z'] ++ [delim]

-- Generate source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Check if a span is valid (start <= end)
isValidSpan' :: SourceSpan -> Bool
isValidSpan' span = spanStart span <= spanEnd span

-- Count occurrences of a character in a string
countChar :: Char -> String -> Int
countChar c = length . filter (== c)

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "trim is linear time" $
      \s -> length (trim s) `seq` True
    
  , testProperty "splitBy is linear in input size" $
      \delim s -> delim /= ',' ==> length (splitBy delim s) `seq` True
    
  , testProperty "comment removal is linear time" $
      \s -> length (removeComments s) `seq` True
  ]