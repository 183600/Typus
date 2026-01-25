{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.UtilsStringProcessingExtraSpec where



import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Test.Tasty
import Test.Tasty.QuickCheck

import Utils
import Data.List (isPrefixOf, isSuffixOf, intercalate, isInfixOf)
import Data.Char (isSpace, isAlphaNum)

-- 辅助函数已在Utils模块中导入

-- | 测试Utils模块中的字符串处理函数
tests :: TestTree
tests = testGroup "UtilsStringProcessingExtraSpec Tests"
  [ testGroup "trim函数测试"
    [ testCase "trim empty string" $ trim "" @?= ""
    , testCase "trim whitespace only" $ 
        (trim "   " @?= "") *>
        (trim "\t\n\r " @?= "")
    , testCase "trim normal string" $
        (trim "hello world" @?= "hello world") *>
        (trim "  hello world  " @?= "hello world")
    , testCase "trim with newlines" $
        trim "\nhello\nworld\n" @?= "hello\nworld"
    , testCase "trim preserve internal whitespace" $
        trim "  hello   world  " @?= "hello   world"
    , testProperty "trim removes whitespace from ends" $
        \s -> not (null (trim s)) ==> 
          let trimmed = trim s
              firstChar str = case str of
                                (c:_) -> c
                                [] -> ' '
              lastChar str = case reverse str of
                               (c:_) -> c
                               [] -> ' '
          in not (isSpace (firstChar trimmed)) && not (isSpace (lastChar trimmed))
    , testProperty "trim is idempotent" $
        \s -> trim (trim s) == trim s
    ]
  
  , testGroup "splitBy函数测试"
    [ testCase "splitBy empty string" $ splitBy ',' "" @?= []
    , testCase "splitBy single character" $ splitBy ',' "a" @?= ["a"]
    , testCase "splitBy with delimiter" $ splitBy ',' "a,b,c" @?= ["a", "b", "c"]
    , testCase "splitBy preserves empty segments" $
        (splitBy ',' "a,,b" @?= ["a", "", "b"]) *>
        (splitBy ',' ",a," @?= ["", "a", ""])
    , testCase "splitBy only delimiters" $
        (splitBy ',' "," @?= [""]) *>
        (splitBy ',' ",," @?= ["", ""])
    , testProperty "splitBy join equals original" $
        \delim s -> delim /= ',' && not (null s) ==>
          let parts = splitBy delim s
              joined = intercalate [delim] parts
          in joined == s
    , testProperty "splitBy length equals delimiter count + 1" $
        \delim s -> delim /= ',' && not (null s) ==>
          let parts = splitBy delim s
              delimCount = length (filter (== delim) s)
          in length parts == delimCount + 1
    ]
  
  , testGroup "splitByComma函数测试"
    [ testCase "splitByComma empty string" $ splitByComma "" @?= []
    , testCase "splitByComma normal CSV" $ splitByComma "a,b,c" @?= ["a", "b", "c"]
    , testCase "splitByComma preserves empty fields" $
        (splitByComma "a,,c" @?= ["a", "", "c"]) *>
        (splitByComma ",a,b" @?= ["", "a", "b"])
    , testCase "splitByComma only commas" $
        (splitByComma "," @?= [""]) *>
        (splitByComma ",," @?= ["", ""])
    , testCase "splitByComma with spaces" $
        splitByComma "a, b, c" @?= ["a", " b", " c"]
    ]
  
  , testGroup "splitByCommaCollapsed函数测试"
    [ testCase "splitByCommaCollapsed empty string" $ splitByCommaCollapsed "" @?= []
    , testCase "splitByCommaCollapsed normal CSV" $ splitByCommaCollapsed "a,b,c" @?= ["a", "b", "c"]
    , testCase "splitByCommaCollapsed collapses consecutive commas" $
        (splitByCommaCollapsed "a,,c" @?= ["a", "c"]) *>
        (splitByCommaCollapsed "a,,,b" @?= ["a", "b"])
    , testCase "splitByCommaCollapsed handles leading/trailing commas" $
        (splitByCommaCollapsed ",a,b" @?= ["a", "b"]) *>
        (splitByCommaCollapsed "a,b," @?= ["a", "b"])
    , testCase "splitByCommaCollapsed only commas" $
        (splitByCommaCollapsed "," @?= []) *>
        (splitByCommaCollapsed ",," @?= [])
    ]
  
  , testGroup "removeLineComments函数测试"
    [ testCase "removeLineComments no comments" $ 
        removeLineComments "hello world" @?= "hello world"
    , testCase "removeLineComments with comment" $
        removeLineComments "hello // comment" @?= "hello "
    , testCase "removeLineComments multiline" $
        removeLineComments "hello // comment\nworld" @?= "hello \nworld"
    , testCase "removeLineComments preserves string literals" $
        removeLineComments "hello \"// not a comment\" world" @?= "hello \"// not a comment\" world"
    , testCase "removeLineComments preserves char literals" $
        removeLineComments "hello '/' not a comment world" @?= "hello '/' not a comment world"
    , testCase "removeLineComments multiline comments" $
        removeLineComments "line1\nline2 // comment\nline3" @?= "line1\nline2 \nline3"
    , testProperty "removeLineComments removes line comments" $
        \s -> not (null s) && "//" `isInfixOf` s ==>
          let cleaned = removeLineComments s
              lines' = lines cleaned
          in all (not . ("//" `isPrefixOf`)) (dropWhile null lines')
    ]
  
  , testGroup "removeComments函数测试"
    [ testCase "removeComments no comments" $ 
        removeComments "hello world" @?= "hello world"
    , testCase "removeComments line comment" $
        removeComments "hello // comment" @?= "hello "
    , testCase "removeComments block comment" $
        removeComments "hello /* comment */ world" @?= "hello  world"
    , testCase "removeComments preserves string literals" $
        (removeComments "hello \"// not a comment\" world" @?= "hello \"// not a comment\" world") *>
        (removeComments "hello \"/* not a comment */\" world" @?= "hello \"/* not a comment */\" world")
    , testCase "removeComments nested comments" $
        removeComments "hello /* outer /* inner */ outer */ world" @?= "hello  world"
    , testCase "removeComments multiline block comment" $
        removeComments "line1\n/* comment\nmore comment */\nline2" @?= "line1\n\nline2"
    ]
  
  , testGroup "normalizeIndentation函数测试"
    [ testCase "normalizeIndentation no indentation" $
        normalizeIndentation "line1\nline2" @?= "line1\nline2"
    , testCase "normalizeIndentation uniform indentation" $
        normalizeIndentation "  line1\n  line2" @?= "line1\nline2"
    , testCase "normalizeIndentation preserves relative indentation" $
        normalizeIndentation "  line1\n    line2" @?= "line1\n  line2"
    , testCase "normalizeIndentation mixed indentation" $
        normalizeIndentation "\tline1\n\t\tline2" @?= "line1\n\tline2"
    , testCase "normalizeIndentation handles empty lines" $
        normalizeIndentation "  line1\n  \n  line2" @?= "line1\n\nline2"
    ]
  
  , testGroup "breakOn函数测试"
    [ testCase "breakOn empty strings" $ breakOn "" "" @?= ("", "")
    , testCase "breakOn no delimiter" $ breakOn "," "hello" @?= ("hello", "")
    , testCase "breakOn with delimiter" $ breakOn "," "hello,world" @?= ("hello", ",world")
    , testCase "breakOn multiple delimiters" $ breakOn "," "a,b,c" @?= ("a", ",b,c")
    , testCase "breakOn delimiter at start" $ breakOn "," ",hello" @?= ("", ",hello")
    , testProperty "breakOn join equals original" $
        \delim s -> not (null delim) && delim `isInfixOf` s ==>
          let (before, after) = breakOn delim s
          in before ++ after == s
    ]
  
  , testGroup "safeProcessString函数测试"
    [ testCase "safeProcessString empty string" $ 
        safeProcessString "" @?= Right ""
    , testCase "safeProcessString normal string" $
        safeProcessString "hello world" @?= Right "hello world"
    , testCase "safeProcessString special characters" $
        safeProcessString "hello\n\tworld" @?= Right "hello\n\tworld"
    , testCase "safeProcessString unicode" $
        safeProcessString "你好世界" @?= Right "你好世界"
    , testCase "safeProcessString control characters" $
        safeProcessString "hello\x00world" @?= Right "hello world"
    , testProperty "safeProcessString is idempotent" $
        \s -> case safeProcessString s of
                Right s' -> property (safeProcessString s' == Right s')
                Left _ -> property True
    ]
  
  , testGroup "isValidChar函数测试"
    [ testCase "isValidChar letters" $
        (isValidChar 'a' @?= True) *>
        (isValidChar 'Z' @?= True)
    , testCase "isValidChar digits" $
        (isValidChar '0' @?= True) *>
        (isValidChar '9' @?= True)
    , testCase "isValidChar punctuation" $
        (isValidChar '.' @?= True) *>
        (isValidChar ',' @?= True) *>
        (isValidChar ';' @?= True)
    , testCase "isValidChar whitespace" $
        (isValidChar ' ' @?= True) *>
        (isValidChar '\t' @?= True) *>
        (isValidChar '\n' @?= True)
    , testCase "isValidChar control characters" $
        (isValidChar '\0' @?= False) *>
        (isValidChar '\DEL' @?= False)
    , testProperty "isValidChar accepts alphanumeric" $
        \c -> isAlphaNum c ==> isValidChar c
    ]
  ]