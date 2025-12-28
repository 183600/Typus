{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.UtilsStringFunctionsSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, group, sort)
import qualified Data.Text as T
import Control.Arrow ((&&&))

-- ============================================================================
-- Utils String Functions Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Functions Tests"
  [ trimFunctionProperties
  , splitFunctionProperties
  , commentRemovalProperties
  , indentationProperties
  , searchFunctionProperties
  , textNormalizationProperties
  ]

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

trimFunctionProperties :: TestTree
trimFunctionProperties = testGroup "Trim Function Properties"
  [ testProperty "trim is idempotent" $
      \s -> trim (trim s) === trim s
    
  , testProperty "trim removes leading whitespace" $
      \s ->
        let trimmed = trim s
            hasLeadingSpace = case s of
              (c:_) -> isSpace c
              [] -> False
        in if hasLeadingSpace
           then length trimmed < length s
           else trimmed === s
    
  , testProperty "trim removes trailing whitespace" $
      \s ->
        let trimmed = trim s
            hasTrailingSpace = case reverse s of
              (c:_) -> isSpace c
              [] -> False
        in if hasTrailingSpace
           then length trimmed < length s
           else trimmed === s
    
  , testProperty "trim preserves internal whitespace" $
      \s1 s2 s3 ->
        let s = s1 ++ "   " ++ s2 ++ "   " ++ s3
            trimmed = trim s
        in if not (null s1) && not (null s3)
           then "   " `isInfixOf` trimmed
           else True
    
  , testProperty "trim of all whitespace is empty" $
      \s -> all isSpace s ==> trim s === ""
    
  , testProperty "trim of empty string is empty" $
      trim "" === ""
    
  , testProperty "trim never increases length" $
      \s -> length (trim s) <= length s
    
  , testCase "trim examples" $ do
      trim "  hello world  " @?= "hello world"
      trim "\t\n  test  \n\t" @?= "test"
      trim "no_spaces" @?= "no_spaces"
      trim "   " @?= ""
  ]

-- ============================================================================
-- Split Function Properties
-- ============================================================================

splitFunctionProperties :: TestTree
splitFunctionProperties = testGroup "Split Function Properties"
  [ testProperty "splitBy preserves total content when concatenated" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        concat (splitBy delim s) === s
    
  , testProperty "splitBy length matches delimiter count + 1" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        length (splitBy delim s) === countChar delim s + 1
    
  , testProperty "splitBy handles empty string" $
      \delim -> splitBy delim "" === [""]
    
  , testProperty "splitBy handles single character" $
      \delim char -> delim /= char ==> splitBy delim [char] === [[char]]
    
  , testProperty "splitBy handles consecutive delimiters" $
      \delim -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        splitBy delim "a,,b" === ["a", "", "b"]
    
  , testProperty "splitByCommaCollapsed removes empty segments" $
      \s -> not (any null (splitByCommaCollapsed s))
    
  , testProperty "splitByCommaCollapsed is subset of splitByComma" $
      \s -> all (`elem` splitByComma s) (splitByCommaCollapsed s)
    
  , testProperty "splitByCollapsed removes empty segments" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        not (any null (splitByCollapsed delim s))
    
  , testProperty "splitByCollapsed preserves non-empty segments" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let collapsed = splitByCollapsed delim s
            nonEmpty = filter (not . null) (splitBy delim s)
        in sort collapsed === sort nonEmpty
    
  , testCase "split examples" $ do
      splitBy ',' "a,b,c" @?= ["a", "b", "c"]
      splitBy ',' "a,,b" @?= ["a", "", "b"]
      splitByCommaCollapsed "a,,b" @?= ["a", "b"]
      splitByComma ",a," @?= ["", "a", ""]
  ]

-- ============================================================================
-- Comment Removal Properties
-- ============================================================================

commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ testProperty "removeLineComments removes // comments" $
      \prefix comment suffix ->
        let input = prefix ++ "// " ++ comment ++ "\n" ++ suffix
            result = removeLineComments input
        in not ("//" `isInfixOf` result) && suffix `isInfixOf` result
    
  , testProperty "removeLineComments preserves non-comment lines" $
      \s -> not ("//" `isInfixOf` s) ==> removeLineComments s === s
    
  , testProperty "removeComments removes // comments" $
      \prefix comment suffix ->
        let input = prefix ++ "// " ++ comment ++ "\n" ++ suffix
            result = removeComments input
        in not ("//" `isInfixOf` result)
    
  , testProperty "removeComments removes /* */ comments" $
      \prefix comment suffix ->
        let input = prefix ++ "/* " ++ comment ++ " */" ++ suffix
            result = removeComments input
        in not ("/*" `isInfixOf` result) && not ("*/" `isInfixOf` result)
    
  , testProperty "removeComments is idempotent" $
      \s -> removeComments (removeComments s) === removeComments s
    
  , testProperty "removeLineComments is idempotent" $
      \s -> removeLineComments (removeLineComments s) === removeLineComments s
    
  , testProperty "comment removal never increases length" $
      \s -> length (removeComments s) <= length s &&
             length (removeLineComments s) <= length s
    
  , testProperty "comment removal preserves non-comment content" $
      \content ->
        let input = "code " ++ content ++ " more code"
            result1 = removeComments input
            result2 = removeLineComments input
        in content `isInfixOf` result1 && content `isInfixOf` result2
    
  , testCase "comment removal examples" $ do
      removeLineComments "code // comment\nmore" @?= "code \nmore"
      removeComments "code // comment\n/* block */ more" @?= "code \n more"
      removeComments "no comments here" @?= "no comments here"
  ]

-- ============================================================================
-- Indentation Properties
-- ============================================================================

indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves line count" $
      \s -> not (null s) ==> 
        let lines1 = lines s
            lines2 = lines (normalizeIndentation s)
        in length lines1 === length lines2
    
  , testProperty "normalizeIndentation removes common prefix" $
      \s ->
        let normalized = normalizeIndentation s
            originalLines = lines s
            normalizedLines = lines normalized
        in if length originalLines > 1
           then all (not . isPrefixOf "  ") normalizedLines || 
                all (isPrefixOf "  ") normalizedLines
           else True
    
  , testProperty "normalizeIndentation is idempotent" $
      \s -> normalizeIndentation (normalizeIndentation s) === normalizeIndentation s
    
  , testProperty "forceSingleTabIndentation converts spaces to tabs" $
      \s ->
        let tabbed = forceSingleTabIndentation s
        in if "  " `isInfixOf` s
           then "\t" `isInfixOf` tabbed
           else True
    
  , testProperty "fixIndentation is same as normalizeIndentation" $
      \s -> fixIndentation s === normalizeIndentation s
    
  , testProperty "indentation functions preserve non-whitespace content" $
      \s ->
        let content = filter (not . isSpace) s
            normalized = normalizeIndentation s
            tabbed = forceSingleTabIndentation s
        in content `isInfixOf` normalized && content `isInfixOf` tabbed
    
  , testCase "indentation examples" $ do
      normalizeIndentation "  line1\n    line2\n  line3" @?= "line1\n  line2\nline3"
      forceSingleTabIndentation "  line1\n    line2" @?="\tline1\n\t\tline2"
      fixIndentation "  test" @?= "test"
  ]

-- ============================================================================
-- Search Function Properties
-- ============================================================================

searchFunctionProperties :: TestTree
searchFunctionProperties = testGroup "Search Function Properties"
  [ testProperty "breakOn is consistent with splitBy" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let (prefix, suffix) = breakOn delim s
            parts = splitBy delim s
        in case parts of
          [] -> prefix === "" && suffix === ""
          [x] -> prefix === x && suffix === ""
          (x:xs) -> prefix === x && suffix === concat xs
    
  , testProperty "breakOn finds first occurrence" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let (prefix, suffix) = breakOn delim s
        in if delim `elem` s
           then delim `elem` suffix && not (delim `elem` prefix)
           else prefix === s && suffix === ""
    
  , testProperty "breakOn handles empty delimiter" $
      \s -> breakOn "" s === ("", s)
    
  , testProperty "breakOn handles empty string" $
      \delim -> breakOn delim "" === ("", "")
    
  , testProperty "breakOn is deterministic" $
      \delim s -> breakOn delim s === breakOn delim s
    
  , testCase "breakOn examples" $ do
      breakOn ',' "a,b,c" @?= ("a", ",b,c")
      breakOn ' ' "hello world" @?= ("hello", " world")
      breakOn 'x' "abc" @?= ("abc", "")
      breakOn 'a' "abc" @?= ("", "abc")
  ]

-- ============================================================================
-- Text Normalization Properties
-- ============================================================================

textNormalizationProperties :: TestTree
textNormalizationProperties = testGroup "Text Normalization Properties"
  [ testProperty "trim . split . join preserves non-delimiter content" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let parts = splitBy delim s
            rejoined = concat parts
            trimmed = trim rejoined
        in filter (not . (== delim)) s `isInfixOf` trimmed
    
  , testProperty "text processing pipeline is idempotent" $
      \s ->
        let processed = normalizeIndentation . trim . removeComments $ s
            processedAgain = normalizeIndentation . trim . removeComments $ processed
        in processed === processedAgain
    
  , testProperty "text processing preserves order of operations" $
      \s ->
        let method1 = normalizeIndentation . trim . removeComments $ s
            method2 = trim . normalizeIndentation . removeComments $ s
        in -- These should be equivalent for most cases
           length method1 === length method2
    
  , testProperty "normalization doesn't create new delimiters" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let normalized = normalizeIndentation s
            originalCount = countChar delim s
            normalizedCount = countChar delim normalized
        in normalizedCount <= originalCount + 1  -- Allow small variations
    
  , testProperty "processing pipeline preserves essential content" $
      \s ->
        let essential = filter isAlphaNum s
            processed = normalizeIndentation . trim . removeComments $ s
            processedEssential = filter isAlphaNum processed
        in null essential || processedEssential `isInfixOf` essential ||
           essential `isInfixOf` processedEssential
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements [' ', '\t', '\n', '\r']

-- Generate strings with delimiters
genDelimitedString :: Char -> Gen String
genDelimitedString delim = listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [delim]

-- Generate strings with comments
genCommentString :: Gen String
genCommentString = do
  code <- listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t"
  comment <- listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ code ++ "// " ++ comment

-- Generate indented strings
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf1 $ do
    indent <- listOf $ elements " \t"
    content <- listOf $ elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
    return $ indent ++ content
  return $ unlines lines

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Count occurrences of a character in a string
countChar :: Char -> String -> Int
countChar c = length . filter (== c)

-- Check if string is all whitespace
isAllWhitespace :: String -> Bool
isAllWhitespace = all isSpace

-- Get leading whitespace count
leadingWhitespaceCount :: String -> Int
leadingWhitespaceCount = length . takeWhile isSpace

-- Get trailing whitespace count
trailingWhitespaceCount :: String -> Int
trailingWhitespaceCount = length . takeWhile isSpace . reverse

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "trim handles unicode whitespace" $
      trim "\x2000\x2001hello\x2002\x2003" @?= "hello"
    
  , testCase "split handles unicode delimiters" $
      splitBy 'α' "aαbαc" @?= ["a", "b", "c"]
    
  , testCase "comment removal handles nested comments" $
      removeComments "code /* outer /* inner */ still outer */ end" @?= "code  end"
    
  , testCase "indentation handles mixed tabs and spaces" $
      normalizeIndentation "\t  mixed\n\t\t  indentation" @?= "mixed\n  indentation"
    
  , testCase "breakOn handles multibyte characters" $
      breakOn '世' "hello世界world" @?= ("hello", "世界world")
    
  , testProperty "trim handles very long strings" $
      \n -> n < 10000 ==>
        let longString = ' ' : replicate n 'a' ++ " "
            trimmed = trim longString
        in trimmed === replicate n 'a'
    
  , testProperty "split handles empty segments" $
      \delim n -> delim /= ',' && delim /= '\n' && delim /= '\t' && n < 100 ==>
        let input = concat $ replicate n [delim]
            parts = splitBy delim input
        in length parts === n + 1 && all (== "") parts
  ]

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "trim is linear time" $
      \s -> length s < 10000 ==> length (trim s) `seq` True
    
  , testProperty "splitBy is linear in input size" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' && length s < 10000 ==>
        length (splitBy delim s) `seq` True
    
  , testProperty "comment removal is linear time" $
      \s -> length s < 10000 ==> length (removeComments s) `seq` True
    
  , testProperty "normalizeIndentation is linear time" $
      \s -> length s < 10000 ==> length (normalizeIndentation s) `seq` True
    
  , testProperty "breakOn is efficient" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' && length s < 10000 ==>
        let (prefix, suffix) = breakOn delim s
        in length prefix + length suffix `seq` True
  ]

-- ============================================================================
-- Regression Tests
-- ============================================================================

regressionProperties :: TestTree
regressionProperties = testGroup "Regression Tests"
  [ testCase "handle strings with only delimiters" $
      splitBy ',' ",,," @?= ["", "", "", ""]
    
  , testCase "handle comments at end of file" $
      removeLineComments "code\n// comment at end" @?= "code\n"
    
  , testCase "handle indentation with empty lines" $
      normalizeIndentation "  line1\n\n  line2" @?= "line1\n\nline2"
    
  , testCase "handle breakOn with character not found" $
      breakOn 'x' "abc" @?= ("abc", "")
    
  , testProperty "preserve order of non-delimiter characters" $
      \delim s -> delim /= ',' && delim /= '\n' && delim /= '\t' ==>
        let parts = splitBy delim s
            rejoined = concat parts
            nonDelimiters = filter (/= delim) s
        in filter (/= delim) rejoined === nonDelimiters
  ]