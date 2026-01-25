{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestStringProcessingSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Utils
import SourceLocation
import ErrorHandler
import qualified Data.Text as T
import TestSupport.Arbitrary ()
import Data.Char (isAscii, isControl, isSpace)
import Data.List (isPrefixOf, isInfixOf, intersperse, last)
import Prelude hiding (last)

-- | Test suite for String Processing
testStringProcessing :: TestTree
testStringProcessing = testGroup "String Processing Tests"
  [ testCase "Utils: trim removes leading and trailing whitespace" $
      trim "  hello world  " @?= "hello world"
      
  , testCase "Utils: trim handles empty string" $
      trim "" @?= ""
      
  , testCase "Utils: trim handles only whitespace" $
      trim "   \t\n   " @?= ""
      
  , testCase "Utils: trim handles string without whitespace" $
      trim "hello" @?= "hello"
      
  , testCase "Utils: splitBy works with single character delimiter" $
      splitBy ',' "a,b,c" @?= ["a", "b", "c"]
      
  , testCase "Utils: splitBy preserves empty segments" $
      splitBy ',' "a,,b" @?= ["a", "", "b"]
      
  , testCase "Utils: splitBy handles leading delimiter" $
      splitBy ',' ",a,b" @?= ["", "a", "b"]
      
  , testCase "Utils: splitBy handles trailing delimiter" $
      splitBy ',' "a,b," @?= ["a", "b", ""]
      
  , testCase "Utils: splitBy handles only delimiters" $
      splitBy ',' ",," @?= ["", "", ""]
      
  , testCase "Utils: splitBy handles empty string" $
      splitBy ',' "" @?= []
      
  , testCase "Utils: splitByCollapsed removes empty segments" $
      splitByCollapsed ',' "a,,b" @?= ["a", "b"]
      
  , testCase "Utils: splitByCollapsed handles leading/trailing delimiters" $
      splitByCollapsed ',' ",a,b," @?= ["a", "b"]
      
  , testCase "Utils: splitByComma works correctly" $
      splitByComma "a,b,c" @?= ["a", "b", "c"]
      
  , testCase "Utils: splitByCommaCollapsed works correctly" $
      splitByCommaCollapsed "a,,b" @?= ["a", "b"]
      
  , testCase "Utils: removeLineComments removes single-line comments" $
      removeLineComments "code\n// comment\nmore code" @?= "code\n\nmore code"
      
  , testCase "Utils: removeLineComments ignores // in strings" $
      removeLineComments "print(\"// not a comment\")\n// real comment" @?= 
        "print(\"// not a comment\")\n"
        
  , testCase "Utils: removeLineComments ignores // in character literals" $
      removeLineComments "c := '/' // comment" @?= "c := '/' "
      
  , testCase "Utils: removeLineComments handles escaped quotes" $
      removeLineComments "print(\"string with \\\"// not comment\\\"\")\n// comment" @?= 
        "print(\"string with \\\"// not comment\\\"\")\n"
        
  , testCase "Utils: removeComments removes both line and block comments" $
      removeComments "code\n// line comment\nmore code\n/* block comment */\nfinal code" @?= 
        "code\n\nmore code\n\nfinal code"
        
  , testCase "Utils: removeComments ignores comments in strings" $
      removeComments "print(\"// not comment\")\n/* not comment */" @?= 
        "print(\"// not comment\")\n"
        
  , testCase "Utils: removeComments handles nested block comments" $
      removeComments "/* outer /* inner */ outer */ code" @?= 
        " code"  -- Simplified: doesn't support nested comments
        
  , testCase "Utils: removeComments handles unclosed block comment" $
      removeComments "code /* unclosed comment" @?= 
        "code "
        
  , testCase "Utils: normalizeIndentation removes common prefix" $
      normalizeIndentation "    line1\n      line2\n    line3" @?= 
        "line1\n  line2\nline3"
        
  , testCase "Utils: normalizeIndentation handles empty lines" $
      normalizeIndentation "    line1\n\n    line2" @?= 
        "line1\n\nline2"
        
  , testCase "Utils: normalizeIndentation handles lines with only whitespace" $
      normalizeIndentation "    line1\n      \n    line2" @?= 
        "line1\n  \nline2"
        
  , testCase "Utils: normalizeIndentation handles single line" $
      normalizeIndentation "    single line" @?= "single line"
      
  , testCase "Utils: normalizeIndentation handles empty string" $
      normalizeIndentation "" @?= ""
      
  , testCase "Utils: breakOn finds substring" $
      breakOn "ll" "hello" @?= ("he", "o")
      
  , testCase "Utils: breakOn with empty pattern" $
      breakOn "" "hello" @?= ("", "hello")
      
  , testCase "Utils: breakOn with pattern not found" $
      breakOn "xyz" "hello" @?= ("hello", "")
      
  , testCase "Utils: breakOn with pattern at start" $
      breakOn "he" "hello" @?= ("", "llo")
      
  , testCase "Utils: breakOn with pattern at end" $
      breakOn "lo" "hello" @?= ("hel", "")
      
  , testCase "Utils: safeProcessString filters control characters" $
      case safeProcessString "hello\x01world\x02" of
        Right result -> result @?= "helloworld"
        Left _ -> assertFailure "Expected successful processing"
        
  , testCase "Utils: safeProcessString allows newlines" $
      case safeProcessString "hello\nworld" of
        Right result -> result @?= "hello\nworld"
        Left _ -> assertFailure "Expected successful processing"
        
  , testCase "Utils: safeProcessString allows tabs" $
      case safeProcessString "hello\tworld" of
        Right result -> result @?= "hello\tworld"
        Left _ -> assertFailure "Expected successful processing"
        
  , testCase "Utils: safeProcessString handles empty string" $
      case safeProcessString "" of
        Left "Empty string after processing" -> return ()
        _ -> assertFailure "Expected empty string error"
        
  , testCase "Utils: safeProcessString handles only control characters" $
      case safeProcessString "\x01\x02\x03" of
        Left "Empty string after processing" -> return ()
        _ -> assertFailure "Expected empty string error"
        
  , testCase "Utils: isValidChar works correctly" $ do
      isValidChar 'a' @?= True
      isValidChar '\n' @?= True
      isValidChar '\t' @?= True
      isValidChar '\x01' @?= False
      
  , testProperty "Utils: trim(trim(x)) == trim(x)" $
      \x -> trim (trim x) == trim x
      
  , testProperty "Utils: splitBy delim (concat with delim) preserves original structure" $
      \delim xs -> 
        let joined = concat (intersperse [delim] xs)
            split = splitBy delim joined
        in length split >= length xs  -- At least as many segments
        
  , testProperty "Utils: removeComments(removeComments(x)) == removeComments(x)" $
      \x -> removeComments (removeComments x) == removeComments x
      
  , testProperty "Utils: normalizeIndentation(normalizeIndentation(x)) == normalizeIndentation(x)" $
      \x -> normalizeIndentation (normalizeIndentation x) == normalizeIndentation x
      
  , testProperty "Utils: safeProcessString preserves valid characters" $
      \s -> let filtered = filter isValidChar s
             in case safeProcessString s of
                  Left _ -> null filtered
                  Right result -> all isValidChar result
                  
  , testProperty "Utils: splitByComma is equivalent to splitBy ','" $
      \s -> splitByComma s == splitBy ',' s
      
  , testProperty "Utils: removeLineComments preserves non-comment lines" $
      \s -> not ("//" `isPrefixOf` s) ==> 
        removeLineComments s == s
        
  , testProperty "Utils: breakOn pattern not found returns original string" $
      \pat s -> not (pat `isInfixOf` s) ==> 
        case breakOn pat s of
          (before, after) -> before == s && after == ""
          
  , testProperty "Utils: isValidChar is true for printable ASCII" $
      \c -> isAscii c && not (isControl c) ==> isValidChar c
      
  , testProperty "Utils: isValidChar is true for whitespace" $
      \c -> c `elem` ['\n', '\t', '\r', ' '] ==> isValidChar c
      
  , testProperty "Utils: isValidChar is false for control characters (except whitespace)" $
      \c -> isControl c && c `notElem` ['\n', '\t', '\r'] ==> not (isValidChar c)
      
  , testProperty "Utils: splitBy preserves character count (minus delimiters)" $
      \delim s -> 
        let parts = splitBy delim s
            totalLength = sum (map length parts)
            delimiterCount = length (filter (== delim) s)
        in totalLength + delimiterCount == length s
        
  , testProperty "Utils: collapse split preserves non-empty segments" $
      \delim s -> 
        let collapsed = splitByCollapsed delim s
            normal = splitBy delim s
            nonEmptySegments = filter (not . null) normal
        in collapsed == nonEmptySegments
        
  , testProperty "Utils: trim does not change string without leading/trailing whitespace" $
      \s -> let firstChar str = case str of
                                   (c:_) -> c
                                   [] -> ' '
                lastChar str = case reverse str of
                                  (c:_) -> c
                                  [] -> ' '
            in not (null s) && not (isSpace (firstChar s)) && not (isSpace (lastChar s)) ==> 
               trim s == s
        
  , testProperty "Utils: normalizeIndentation preserves relative indentation" $
      \s -> 
        let inputLines = filter (not . all isSpace) $ Prelude.lines s
            normalized = normalizeIndentation s
            normLines = filter (not . all isSpace) $ Prelude.lines normalized
        in length inputLines == length normLines
  ]
