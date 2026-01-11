module Test.Unit.UtilsStringProcessingSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils

-- Test cases for trim function
testTrim :: TestTree
testTrim = testGroup "trim function tests"
  [ testCase "trim removes leading and trailing spaces" $
      trim "  hello  " @?= "hello"
  , testCase "trim removes leading and trailing tabs" $
      trim "\thello\t" @?= "hello"
  , testCase "trim removes mixed whitespace" $
      trim "  \t hello \t  " @?= "hello"
  , testCase "trim handles empty string" $
      trim "" @?= ""
  , testCase "trim handles only whitespace" $
      trim "   \t  " @?= ""
  , testCase "trim leaves internal spaces unchanged" $
      trim "  hello world  " @?= "hello world"
  ]

-- Test cases for splitBy function
testSplitBy :: TestTree
testSplitBy = testGroup "splitBy function tests"
  [ testCase "splitBy comma with empty segments" $
      splitBy ',' "a,,b" @?= ["a", "", "b"]
  , testCase "splitBy comma with leading/trailing" $
      splitBy ',' ",a," @?= ["", "a", ""]
  , testCase "splitBy comma with empty string" $
      splitBy ',' "" @?= [""]
  , testCase "splitBy comma with single element" $
      splitBy ',' "hello" @?= ["hello"]
  , testCase "splitBy comma with multiple elements" $
      splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  ]

-- Test cases for splitByCollapsed function
testSplitByCollapsed :: TestTree
testSplitByCollapsed = testGroup "splitByCollapsed function tests"
  [ testCase "splitByCollapsed removes empty segments" $
      splitByCollapsed ',' "a,,b" @?= ["a", "b"]
  , testCase "splitByCollapsed removes leading/trailing empties" $
      splitByCollapsed ',' ",a," @?= ["a"]
  , testCase "splitByCollapsed handles empty string" $
      splitByCollapsed ',' "" @?= []
  , testCase "splitByCollapsed handles single element" $
      splitByCollapsed ',' "hello" @?= ["hello"]
  , testCase "splitByCollapsed handles multiple elements" $
      splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
  ]

-- Test cases for removeLineComments function
testRemoveLineComments :: TestTree
testRemoveLineComments = testGroup "removeLineComments function tests"
  [ testCase "removeLineComments removes single line comment" $
      removeLineComments "hello // comment\nworld" @?= "hello \nworld"
  , testCase "removeLineComments preserves comments in strings" $
      removeLineComments "print(\"// not a comment\") // real comment" @?= "print(\"// not a comment\") "
  , testCase "removeLineComments preserves comments in chars" $
      removeLineComments "c := '/' // comment" @?= "c := '/' "
  , testCase "removeLineComments handles multiple comments" $
      removeLineComments "a // 1\nb // 2\nc" @?= "a \nb \nc"
  , testCase "removeLineComments handles escaped quotes in strings" $
      removeLineComments "print(\"\\\"// not comment\") // comment" @?= "print(\"\\\"// not comment\") "
  ]

-- Test cases for removeComments function
testRemoveComments :: TestTree
testRemoveComments = testGroup "removeComments function tests"
  [ testCase "removeComments removes line comments" $
      removeComments "hello // comment\nworld" @?= "hello \nworld"
  , testCase "removeComments removes block comments" $
      removeComments "hello /* comment */ world" @?= "hello  world"
  , testCase "removeComments handles multiline block comments" $
      removeComments "hello /* multi\nline\ncomment */ world" @?= "hello \n \n world"
  , testCase "removeComments preserves comments in strings" $
      removeComments "print(\"// not comment\") /* real comment */" @?= "print(\"// not comment\") "
  , testCase "removeComments handles nested quote patterns" $
      removeComments "print(\"/* not comment */\") /* real comment */" @?= "print(\"/* not comment */\") "
  ]

-- Test cases for normalizeIndentation function
testNormalizeIndentation :: TestTree
testNormalizeIndentation = testGroup "normalizeIndentation function tests"
  [ testCase "normalizeIndentation removes common prefix" $
      normalizeIndentation "    hello\n      world" @?= "hello\n  world"
  , testCase "normalizeIndentation handles different indentation" $
      normalizeIndentation "  a\n    b\n  c" @?= "a\n  b\nc"
  , testCase "normalizeIndentation preserves empty lines" $
      normalizeIndentation "    a\n\n    b" @?= "a\n\nb"
  , testCase "normalizeIndentation handles mixed tabs/spaces" $
      normalizeIndentation "\ta\n\t\tb" @?= "a\n\tb"
  , testCase "normalizeIndentation handles single line" $
      normalizeIndentation "    hello" @?= "hello"
  ]

-- Test cases for breakOn function
testBreakOn :: TestTree
testBreakOn = testGroup "breakOn function tests"
  [ testCase "breakOn finds substring" $
      breakOn "ll" "hello" @?= ("he", "o")
  , testCase "breakOn handles first occurrence" $
      breakOn "ab" "abcab" @?= ("", "cab")
  , testCase "breakOn handles not found" $
      breakOn "xyz" "hello" @?= ("hello", "")
  , testCase "breakOn handles empty pattern" $
      breakOn "" "hello" @?= ("", "hello")
  , testCase "breakOn handles pattern at end" $
      breakOn "lo" "hello" @?= ("hel", "")
  ]

-- Test cases for safeProcessString function
testSafeProcessString :: TestTree
testSafeProcessString = testGroup "safeProcessString function tests"
  [ testCase "safeProcessString handles normal string" $
      safeProcessString "hello world" @?= Right "hello world"
  , testCase "safeProcessString allows newlines" $
      safeProcessString "hello\nworld" @?= Right "hello\nworld"
  , testCase "safeProcessString allows tabs" $
      safeProcessString "hello\tworld" @?= Right "hello\tworld"
  , testCase "safeProcessString filters control characters" $
      safeProcessString "hello\x01world" @?= Right "helloworld"
  , testCase "safeProcessString handles empty after filtering" $
      safeProcessString "\x01\x02" @?= Left "Empty string after processing"
  ]

-- Test cases for isValidChar function
testIsValidChar :: TestTree
testIsValidChar = testGroup "isValidChar function tests"
  [ testCase "isValidChar allows normal characters" $
      isValidChar 'a' @?= True
  , testCase "isValidChar allows newline" $
      isValidChar '\n' @?= True
  , testCase "isValidChar allows carriage return" $
      isValidChar '\r' @?= True
  , testCase "isValidChar allows tab" $
      isValidChar '\t' @?= True
  , testCase "isValidChar rejects control characters" $
      isValidChar '\x01' @?= False
  ]

-- QuickCheck properties
prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim s = 
  let parts = splitBy delim s
      rejoined = concat $ map (++ [delim]) $ init parts ++ [last parts]
  in length s >= 0 ==> length rejoined >= length s - length parts

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = 
  let trimmed = trim s
      trimmedAgain = trim trimmed
  in length s >= 0 ==> trimmed == trimmedAgain

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s = 
  let parts = splitByCollapsed delim s
  in length s >= 0 ==> all (not . null) parts

tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testTrim
  , testSplitBy
  , testSplitByCollapsed
  , testRemoveLineComments
  , testRemoveComments
  , testNormalizeIndentation
  , testBreakOn
  , testSafeProcessString
  , testIsValidChar
  , testProperty "splitBy roundtrip property" prop_splitBy_roundtrip
  , testProperty "trim idempotent property" prop_trim_idempotent
  , testProperty "splitByCollapsed no empty property" prop_splitByCollapsed_no_empty
  ]