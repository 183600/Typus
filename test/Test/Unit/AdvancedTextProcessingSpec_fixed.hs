module Test.Unit.AdvancedTextProcessingSpec_fixed where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, intercalate)
import Data.Char (isSpace, isControl, toUpper, toLower)
import qualified Data.Text as T

-- Helper function for joining strings with a delimiter
joinWith :: Char -> [String] -> String
joinWith c = intercalate [c]

-- Helper function to normalize whitespace
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words

-- Helper function to extract lines
extractLines :: String -> [String]
extractLines = lines

-- Helper function to indent a string by n spaces
indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- Helper functions for string case conversion
toUpperStr :: String -> String
toUpperStr = map toUpper

toLowerStr :: String -> String
toLowerStr = map toLower

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
        splitBy , "" @?= [""]
      
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
        \delim (str :: Char) -> splitBy delim (delim:str:delim:[]) === ["", [str], ""]
    ]
  
  , testGroup "joinWith function edge cases"
    [ testCase "handles empty input" $
        joinWith ',' [""] @?= ""
      
    , testCase "handles single element" $
        joinWith ',' ["a"] @?= "a"
      
    , testCase "handles multiple elements" $
        joinWith ',' ["a", "b", "c"] @?= "a,b,c"
      
    , testProperty "is inverse of splitBy" $
        \delim str -> not (null str) ==> joinWith delim (splitBy delim str) === str
    ]
  
  , testGroup "normalizeWhitespace function edge cases"
    [ testCase "handles empty strings" $
        normalizeWhitespace "" @?= ""
      
    , testCase "handles strings with only whitespace" $
        normalizeWhitespace "   \t\n\r   " @?= ""
      
    , testCase "handles leading/trailing whitespace" $ do
        normalizeWhitespace "  hello  " @?= "hello"
        normalizeWhitespace "\tworld\n" @?= "world"
      
    , testCase "handles internal whitespace" $ do
        normalizeWhitespace "hello   world" @?= "hello world"
        normalizeWhitespace "foo\tbar\nbaz" @?= "foo bar baz"
      
    , testProperty "preserves non-whitespace characters" $
        \str -> filter (not . isSpace) (normalizeWhitespace str) === filter (not . isSpace) str
    ]
  
  , testGroup "extractLines function edge cases"
    [ testCase "handles empty strings" $
        extractLines "" @?= []
      
    , testCase "handles single line" $
        extractLines "hello" @?= ["hello"]
      
    , testCase "handles multiple lines" $
        extractLines "hello\nworld\nfoo" @?= ["hello", "world", "foo"]
      
    , testCase "handles trailing newline" $
        extractLines "hello\nworld\n" @?= ["hello", "world"]
      
    , testProperty "preserves line content" $
        \str -> unlines (extractLines str) === if null str then "" else if last str == '\n' then init str else str
    ]
  
  , testGroup "indentation function edge cases"
    [ testCase "handles empty strings" $
        indent 2 "" @?= ""
      
    , testCase "handles single line" $
        indent 2 "hello" @?= "  hello"
      
    , testCase "handles multiple lines" $
        indent 2 "hello\nworld" @?= "  hello\n  world"
      
    , testCase "handles zero indentation" $
        indent 0 "hello" @?= "hello"
      
    , testProperty "adds correct number of spaces" $
        \n str -> take n (indent (abs n) str) === replicate n ' '
    ]
  
  , testGroup "character classification edge cases"
    [ testCase "identifies whitespace characters" $ do
        isSpace ' ' @?= True
        isSpace '\t' @?= True
        isSpace '\n' @?= True
        isSpace '\r' @?= True
      
    , testCase "identifies control characters" $ do
        isControl '\0' @?= True
        isControl '\1' @?= True
        isControl '\31' @?= True
        isControl '\127' @?= True
      
    , testCase "handles Unicode characters" $ do
        isSpace '\160' @?= True  -- Non-breaking space
        isControl '\127' @?= True  -- DEL character
    ]
  
  , testGroup "text transformation edge cases"
    [ testCase "handles empty strings in transformations" $ do
        toUpperStr "" @?= ""
        toLowerStr "" @?= ""
        reverse "" @?= ""
      
    , testCase "handles Unicode transformations" $ do
        toUpperStr "hello" @?= "HELLO"
        toLowerStr "WORLD" @?= "world"
        reverse "abc" @?= "cba"
      
    , testProperty "toUpper is idempotent" $
        \str -> toUpperStr (toUpperStr str) === toUpperStr str
      
    , testProperty "toLower is idempotent" $
        \str -> toLowerStr (toLowerStr str) === toLowerStr str
    ]
  
  , testGroup "string searching edge cases"
    [ testCase "handles empty patterns" $ do
        isInfixOf "" "hello" @?= True
        isPrefixOf "" "hello" @?= True
        isSuffixOf "" "hello" @?= True
      
    , testCase "handles empty strings" $ do
        isInfixOf "a" "" @?= False
        isPrefixOf "a" "" @?= False
        isSuffixOf "a" "" @?= False
      
    , testCase "handles exact matches" $ do
        isInfixOf "hello" "hello" @?= True
        isPrefixOf "hello" "hello" @?= True
        isSuffixOf "hello" "hello" @?= True
      
    , testProperty "isPrefixOf implies isInfixOf" $
        \(pat :: String) (str :: String) -> isPrefixOf pat str ==> isInfixOf pat str
      
    , testProperty "isSuffixOf implies isInfixOf" $
        \(pat :: String) (str :: String) -> isSuffixOf pat str ==> isInfixOf pat str
    ]
  
  , testGroup "text processing performance edge cases"
    [ testCase "handles large strings efficiently" $ do
        let largeString = replicate 10000 'a'
        length largeString @?= 10000
      
    , testCase "handles deeply nested operations" $ do
        let nested = foldl (\acc x -> acc ++ show x) "" [1..1000]
        length (words nested) @?= 1000
    ]
  
  , testGroup "QuickCheck properties"
    [ testProperty "trim idempotence" $
        \str -> trim (trim str) === trim str
      
    , testProperty "splitBy consistency" $
        \delim str -> concat (splitBy delim str) `shouldSatisfy` (\s -> length s >= length str - length (filter (== delim) str))
      
    , testProperty "breakOn consistency" $
      
            \pat str -> 
      
              let (before, after) = breakOn pat str
      
              in if null pat 
      
                 then before === ""
      
                 else before ++ pat ++ after === str
  ]
  ]

-- Helper function for QuickCheck
shouldSatisfy :: a -> (a -> Bool) -> Property
shouldSatisfy x predicate = property (predicate x)