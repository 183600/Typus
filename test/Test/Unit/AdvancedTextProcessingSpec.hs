{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.AdvancedTextProcessingSpec where



import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Utils

tests :: TestTree
tests = testGroup "Advanced Text Processing Tests"
  [ testGroup "trim function edge cases"
    [ testCase "handles empty strings" $
        trim "" @?= ""
      
    , testCase "handles strings with only whitespace" $ do
        trim "   " @?= ""
        trim "\t\n\r " @?= ""
        trim "\n\t\n\t" @?= ""
      
    , testCase "preserves internal whitespace" $ do
        trim "  hello  world  " @?= "hello  world"
        trim "\tfoo\tbar\t" @?= "foo\tbar"
      
    , testProperty "handles Unicode whitespace" $
        \str -> trim (unwords [str, str]) === unwords [str, str]
    ]
  
  , testGroup "splitBy function edge cases"
    [ testCase "handles empty input" $
        splitBy ',' "" @?= []
      
    , testCase "handles single character" $ do
        splitBy ',' "a" @?= ["a"]
        splitBy ',' "," @?= ["", ""]
      
    , testCase "handles consecutive delimiters" $ do
        splitBy ',' "a,,b" @?= ["a", "", "b"]
        splitBy ',' "a,,,b" @?= ["a", "", "", "b"]
        splitBy ',' ",,," @?= ["", "", "", ""]
      
    , testCase "handles leading and trailing delimiters" $ do
        splitBy ',' ",a,b," @?= ["", "a", "b", ""]
        splitBy ',' ",a," @?= ["", "a", ""]
      
    , testProperty "preserves empty segments" $
        \delim -> splitBy delim [delim] === ["", ""]
    ]

  , testGroup "removeLineComments function"
    [ testCase "handles empty strings" $
        removeLineComments "" @?= ""
      
    , testCase "handles strings without comments" $
        removeLineComments "let x = 42" @?= "let x = 42"
      
    , testCase "removes comments correctly" $ do
        removeLineComments "let x = 42 // comment" @?= "let x = 42"
        removeLineComments "// full line comment" @?= ""
      
    , testCase "preserves comments in string literals" $ do
        removeLineComments "let s = \"// not a comment\"" @?= "let s = \"// not a comment\""
        removeLineComments "let s = \"hello // world\"" @?= "let s = \"hello // world\""
      
    , testCase "handles escaped quotes in strings" $
        removeLineComments "let s = \"\\\"// not a comment\"" @?= "let s = \"\\\"// not a comment\""
      
    , testCase "handles character literals" $ do
        removeLineComments "let c = '/' // comment" @?= "let c = '/'"
        removeLineComments "let c = '\"' // comment" @?= "let c = '\"'"
    ]

  , testGroup "removeComments function"
    [ testCase "handles empty strings" $
        removeComments "" @?= ""
      
    , testCase "handles strings without comments" $
        removeComments "let x = 42" @?= "let x = 42"
      
    , testCase "removes line comments" $
        removeComments "let x = 42 // comment" @?= "let x = 42 "
      
    , testCase "removes block comments" $
        removeComments "let x = 42 /* comment */" @?= "let x = 42 "
      
    , testCase "handles nested block comments" $
        removeComments "code /* outer /* inner */ still outer */ end" @?= "code  end"
      
    , testCase "preserves comments in string literals" $ do
        removeComments "let s = \"// not a comment\"" @?= "let s = \"// not a comment\""
        removeComments "let s = \"/* not a comment */\"" @?= "let s = \"/* not a comment */\""
      
    , testCase "handles multiline comments" $
        removeComments "line1\n/* comment\nspanning multiple\nlines */\nline2" 
          @?= "line1\n\nline2"
    ]

  , testGroup "normalizeIndentation function"
    [ testCase "handles empty strings" $
        normalizeIndentation "" @?= ""
      
    , testCase "handles single lines" $
        normalizeIndentation "  single line" @?= "  single line"
      
    , testCase "removes common prefix indentation" $
        normalizeIndentation "  line1\n    line2\n  line3" @?= "line1\n  line2\nline3"
      
    , testCase "preserves relative indentation" $
        normalizeIndentation "    outer\n      inner\n    outer" @?= "outer\n  inner\nouter"
      
    , testCase "handles mixed tabs and spaces" $
        normalizeIndentation "\t  mixed\n\t    indentation" @?= "mixed\n  indentation"
      
    , testCase "preserves empty lines" $
        normalizeIndentation "  line1\n\n  line2" @?= "line1\n\nline2"
    ]

  , testGroup "breakOn function"
    [ testCase "handles empty pattern" $
        breakOn "" "hello" @?= ("", "hello")
      
    , testCase "handles empty input" $
        breakOn "pattern" "" @?= ("", "")
      
    , testCase "finds first occurrence" $ do
        breakOn "," "a,b,c" @?= ("a", "b,c")
        breakOn "pattern" "prefix pattern suffix" @?= ("prefix ", " suffix")
      
    , testCase "handles pattern at start" $
        breakOn "pattern" "pattern suffix" @?= ("", " suffix")
      
    , testCase "handles pattern at end" $
        breakOn "pattern" "prefix pattern" @?= ("prefix ", "")
      
    , testCase "handles non-existent pattern" $
        breakOn "xyz" "abc" @?= ("abc", "")
    ]

  , testGroup "safeProcessString function"
    [ testCase "handles normal strings" $
        safeProcessString "hello world" @?= Right "hello world"
      
    , testCase "filters control characters" $ do
        safeProcessString "hello\x00world" @?= Right "hello world"
        safeProcessString "text\x01\x02end" @?= Right "textend"
      
    , testCase "preserves newlines and tabs" $
        safeProcessString "line1\nline2\ttab" @?= Right "line1\nline2\ttab"
      
    , testCase "preserves carriage returns" $
        safeProcessString "windows\r\nline" @?= Right "windows\r\nline"
    ]

  , testGroup "isValidChar function"
    [ testCase "accepts printable characters" $ do
        isValidChar 'a' @?= True
        isValidChar 'Z' @?= True
        isValidChar '5' @?= True
        isValidChar '!' @?= True
      
    , testCase "accepts whitespace characters" $ do
        isValidChar ' ' @?= True
        isValidChar '\t' @?= True
        isValidChar '\n' @?= True
        isValidChar '\r' @?= True
      
    , testCase "rejects control characters" $ do
        isValidChar '\x00' @?= False
        isValidChar '\x01' @?= False
        isValidChar '\x1F' @?= False
    ]

  , testGroup "QuickCheck properties"

      [ testProperty "trim idempotence" $

          \str -> trim (trim str) === trim str

        

      , testProperty "splitBy consistency" $

          \delim str -> length (concat (splitBy delim str)) >= length str - length (filter (== delim) str)

        

      , testProperty "breakOn consistency" $

        

              \pat str -> let (before, afterStr) = breakOn pat str

        

                         in if null pat then before == "" else before ++ pat ++ afterStr == str

      ]]