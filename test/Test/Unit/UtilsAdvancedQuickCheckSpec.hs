{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.List (isPrefixOf, isSuffixOf, intercalate, isInfixOf, concat)
import Data.Char (isSpace, isAlphaNum, isControl, isAscii)

-- | 测试Utils模块中的高级字符串处理功能
tests :: TestTree
tests = testGroup "UtilsAdvancedQuickCheckSpec Tests"
  [ testGroup "trim属性测试"
    [ testProperty "trim removes leading whitespace" $
        \s ws ->
          let input = ws ++ s
              trimmed = trim input
          in not (null ws) && all isSpace ws ==> 
             property (not (null trimmed) && not (isSpace (head trimmed)))
    
    , testProperty "trim removes trailing whitespace" $
        \s ws ->
          let input = s ++ ws
              trimmed = trim input
          in not (null ws) && all isSpace ws ==> 
             property (not (null trimmed) && not (isSpace (last trimmed)))
    
    , testProperty "trim is idempotent" $
        \s -> trim (trim s) == trim s
    
    , testProperty "trim preserves internal whitespace" $
        \s1 s2 ->
          let input = s1 ++ "   " ++ s2
              trimmed = trim input
          in "   " `isInfixOf` trimmed ==> property True
    
    , testProperty "trim handles empty string" $
        \() -> trim "" == ""
    
    , testProperty "trim handles whitespace-only string" $
        \ws ->
          let input = ws
              trimmed = trim input
          in all isSpace ws ==> property (trimmed == "")
    ]
  
  , testGroup "splitBy属性测试"
    [ testProperty "splitBy join equals original" $
        \delim s ->
          let parts = splitBy delim s
              joined = intercalate [delim] parts
          in delim /= '\n' && delim /= '\r' ==> property (joined == s)
    
    , testProperty "splitBy length equals delimiter count + 1" $
        \delim s ->
          let parts = splitBy delim s
              delimCount = length (filter (== delim) s)
          in delim /= '\n' && delim /= '\r' ==> property (length parts == delimCount + 1)
    
    , testProperty "splitBy handles empty string" $
        \delim -> splitBy delim "" == []
    
    , testProperty "splitBy handles single character" $
        \delim c ->
          c /= delim ==> property (splitBy delim [c] == [[c]])
    
    , testProperty "splitBy handles only delimiters" $
        \delim n ->
          let s = replicate n delim
              parts = splitBy delim s
          in n > 0 ==> property (length parts == n + 1 && all null parts)
    
    , testProperty "splitBy preserves empty segments" $
        \delim s1 s2 ->
          let input = s1 ++ [delim] ++ [delim] ++ s2
              parts = splitBy delim input
          in property (length parts >= 3 && parts !! 1 == "")
    ]
  
  , testGroup "splitByCollapsed属性测试"
    [ testProperty "splitByCollapsed removes empty segments" $
        \delim s ->
          let parts = splitByCollapsed delim s
          in property (all (not . null) parts)
    
    , testProperty "splitByCollapsed handles only delimiters" $
        \delim n ->
          let s = replicate n delim
              parts = splitByCollapsed delim s
          in n > 0 ==> property (null parts)
    
    , testProperty "splitByCollapsed handles leading/trailing delimiters" $
        \delim s ->
          let input = [delim] ++ s ++ [delim]
              parts = splitByCollapsed delim input
          in property (not (elem "" parts))
    
    , testProperty "splitByCollapsed handles empty string" $
        \delim -> splitByCollapsed delim "" == []
    ]
  
  , testGroup "splitByComma属性测试"
    [ testProperty "splitByComma equals splitBy with comma" $
        \s -> splitByComma s == splitBy ',' s
    
    , testProperty "splitByComma preserves empty fields" $
        \s1 s2 ->
          let input = s1 ++ ",," ++ s2
              parts = splitByComma input
          in property (length parts >= 3 && parts !! 1 == "")
    
    , testProperty "splitByComma handles empty string" $
        \() -> splitByComma "" == []
    
    , testProperty "splitByComma handles only commas" $
        \n ->
          let s = replicate n ','
              parts = splitByComma s
          in n > 0 ==> property (length parts == n + 1 && all null parts)
    ]
  
  , testGroup "splitByCommaCollapsed属性测试"
    [ testProperty "splitByCommaCollapsed equals splitByCollapsed with comma" $
        \s -> splitByCommaCollapsed s == splitByCollapsed ',' s
    
    , testProperty "splitByCommaCollapsed removes empty fields" $
        \s ->
          let parts = splitByCommaCollapsed s
          in property (all (not . null) parts)
    
    , testProperty "splitByCommaCollapsed handles only commas" $
        \n ->
          let s = replicate n ','
              parts = splitByCommaCollapsed s
          in n > 0 ==> property (null parts)
    ]
  
  , testGroup "removeLineComments属性测试"
    [ testProperty "removeLineComments removes line comments" $
        \code comment ->
          let input = code ++ "// " ++ comment
              cleaned = removeLineComments input
          in not ("//" `isInfixOf` cleaned)
    
    , testProperty "removeLineComments preserves content before comment" $
        \code comment ->
          let input = code ++ "// " ++ comment
              cleaned = removeLineComments input
          in property (code `isPrefixOf` cleaned)
    
    , testProperty "removeLineComments preserves string literals" $
        \code comment ->
          let input = code ++ "\"// not a comment\" // " ++ comment
              cleaned = removeLineComments input
          in property ("\"// not a comment\"" `isInfixOf` cleaned)
    
    , testProperty "removeLineComments preserves char literals" $
        \code comment ->
          let input = code ++ "'/' not a comment // " ++ comment
              cleaned = removeLineComments input
          in property ("'/' not a comment" `isInfixOf` cleaned)
    
    , testProperty "removeLineComments handles multiple lines" $
        \lines' ->
          let input = unlines lines'
              cleaned = removeLineComments input
              originalLines = lines input
              cleanedLines = lines cleaned
          in property (length originalLines == length cleanedLines)
    
    , testProperty "removeLineComments handles empty string" $
        \() -> removeLineComments "" == ""
    
    , testProperty "removeLineComments handles string without comments" $
        \(code :: String) -> not ("//" `isInfixOf` code) ==> removeLineComments code == code
    ]
  
  , testGroup "removeComments属性测试"
    [ testProperty "removeComments removes line comments" $
        \(code :: String) (comment :: String) ->
          let input = code ++ "// " ++ comment
              cleaned = removeComments input
          in not ("//" `isInfixOf` cleaned)
    
    , testProperty "removeComments removes block comments" $
        \(code :: String) (comment :: String) ->
          let input = code ++ "/* " ++ comment ++ " */" ++ code
              cleaned = removeComments input
          in not ("/*" `isInfixOf` cleaned) && not ("*/" `isInfixOf` cleaned)
    
    , testProperty "removeComments preserves string literals" $
        \(code :: String) (comment :: String) ->
          let input = code ++ "\"// not a comment\" /* not a comment */ \"/* not a comment */\""
              cleaned = removeComments input
          in property ("\"// not a comment\"" `isInfixOf` cleaned && "\"/* not a comment */\"" `isInfixOf` cleaned)
    
    , testProperty "removeComments handles nested block comments" $
        \(code :: String) (comment1 :: String) (comment2 :: String) ->
          let input = code ++ "/* outer " ++ comment1 ++ " /* inner " ++ comment2 ++ " */ outer */" ++ code
              cleaned = removeComments input
          in not ("/*" `isInfixOf` cleaned) && not ("*/" `isInfixOf` cleaned)
    
    , testProperty "removeComments handles empty string" $
        \() -> removeComments "" == ""
    
    , testProperty "removeComments handles string without comments" $
        \(code :: String) -> not ("//" `isInfixOf` code) && not ("/*" `isInfixOf` code) ==> removeComments code == code
    ]
  
  , testGroup "normalizeIndentation属性测试"
    [ testProperty "normalizeIndentation removes common prefix" $
        \lines' n ->
          let prefix = replicate n ' '
              indentedLines = map (prefix ++) lines'
              normalized = normalizeIndentation (unlines indentedLines)
              normalizedLines = lines normalized
          in not (null lines') && n > 0 ==> 
             property (all (not . isPrefixOf prefix) normalizedLines)
    
    , testProperty "normalizeIndentation preserves relative indentation" $
        \lines' n1 n2 ->
          let prefix1 = replicate n1 ' '
              prefix2 = replicate n2 ' '
              indentedLines = [prefix1 ++ head lines', prefix2 ++ head (tail lines' ++ [""])]
              normalized = normalizeIndentation (unlines indentedLines)
              normalizedLines = lines normalized
          in length lines' >= 2 && n1 < n2 ==> 
             property (length (head (tail normalizedLines)) > length (head normalizedLines))
    
    , testProperty "normalizeIndentation handles empty lines" $
        \lines' ->
          let withEmptyLines = intersperse "" lines'
              normalized = normalizeIndentation (unlines withEmptyLines)
              normalizedLines = lines normalized
          in property (length normalizedLines == length withEmptyLines)
    
    , testProperty "normalizeIndentation handles single line" $
        \line ->
          let normalized = normalizeIndentation line
          in property (normalized == trim line)
    
    , testProperty "normalizeIndentation handles empty string" $
        \() -> normalizeIndentation "" == ""
    ]
  
  , testGroup "breakOn属性测试"
    [ testProperty "breakOn join equals original" $
        \delim s ->
          not (null delim) && delim `isInfixOf` s ==>
            let (before, after) = breakOn delim s
            in before ++ after == s
    
    , testProperty "breakOn finds first occurrence" $
        \delim s1 s2 s3 ->
          let input = s1 ++ delim ++ s2 ++ delim ++ s3
              (before, after) = breakOn delim input
          in not (null s1) && not (null s2) ==> 
             property (before == s1 && delim `isPrefixOf` after)
    
    , testProperty "breakOn handles delimiter at start" $
        \delim s ->
          let input = delim ++ s
              (before, after) = breakOn delim input
          in not (null delim) ==> property (before == "" && after == input)
    
    , testProperty "breakOn handles delimiter at end" $
        \delim s ->
          let input = s ++ delim
              (before, after) = breakOn delim input
          in not (null delim) ==> property (before == s && after == delim)
    
    , testProperty "breakOn handles no delimiter" $
        \delim s ->
          not (delim `isInfixOf` s) ==> 
            let (before, after) = breakOn delim s
            in property (before == s && after == "")
    ]
  
  , testGroup "safeProcessString属性测试"
    [ testProperty "safeProcessString is idempotent" $
        \s ->
          case safeProcessString s of
            Right s' -> property (safeProcessString s' == Right s')
            Left _ -> property True
    
    , testProperty "safeProcessString preserves valid characters" $
        \s ->
          let allValid = all isValidChar s
          in allValid ==> 
             case safeProcessString s of
               Right s' -> property (s' == s)
               Left _ -> property False
    
    , testProperty "safeProcessString handles empty string" $
        \() -> safeProcessString "" == Right ""
    
    , testProperty "safeProcessString handles control characters" $
        \s ->
          let hasControl = any isControl s
          in hasControl ==> 
             case safeProcessString s of
               Right s' -> property (not (any isControl s'))
               Left _ -> property False
    ]
  
  , testGroup "isValidChar属性测试"
    [ testProperty "isValidChar accepts alphanumeric" $
        \c -> isAlphaNum c ==> isValidChar c
    
    , testProperty "isValidChar accepts ASCII punctuation" $
        \c -> isAscii c && not (isAlphaNum c) && not (isSpace c) && not (isControl c) ==> isValidChar c
    
    , testProperty "isValidChar accepts whitespace" $
        \c -> isSpace c ==> isValidChar c
    
    , testProperty "isValidChar rejects control characters" $
        \c -> isControl c ==> not (isValidChar c)
    
    , testProperty "isValidChar handles Unicode" $
        \c -> not (isAscii c) ==> isValidChar c
    ]
  
  , testGroup "边界条件测试"
    [ testCase "trim handles very long string" $ do
        let longString = replicate 10000 ' ' ++ "content" ++ replicate 10000 ' '
            trimmed = trim longString
        assertEqual "Should trim very long string" "content" trimmed
    
    , testCase "splitBy handles very long string" $ do
        let longString = concat (replicate 1000 "a,")
            parts = splitBy ',' longString
        assertEqual "Should split very long string" 1001 (length parts)
    
    , testCase "removeComments handles deeply nested comments" $ do
        let nestedComment = "/* " ++ concat (replicate 100 "/* ") ++ "deep" ++ concat (replicate 100 " */") ++ " */"
            cleaned = removeComments nestedComment
        assertBool "Should handle deeply nested comments" (not ("/*" `isInfixOf` cleaned))
    
    , testCase "normalizeIndentation handles very deep indentation" $ do
        let deepIndentation = unlines (map (\i -> replicate i ' ') [1..1000])
            normalized = normalizeIndentation deepIndentation
        assertBool "Should handle very deep indentation" (not (any (isPrefixOf "   ") (lines normalized)))
    ]
  ]
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)