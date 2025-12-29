module Test.Unit.StringProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, choose, listOf)
import Utils (trim, breakOn)

-- | Tests for string processing functions in Utils module
tests :: TestTree
tests =
  testGroup "Utils String Processing"
    [ testGroup "trim function"
        [ testGroup "Basic functionality"
            [ testCase "removes leading and trailing spaces" $ do
                trim "  hello  " @?= "hello"
            
            , testCase "removes leading and trailing tabs" $ do
                trim "\thello\t" @?= "hello"
            
            , testCase "removes mixed whitespace" $ do
                trim "\t  hello  \t" @?= "hello"
            
            , testCase "handles empty string" $ do
                trim "" @?= ""
            
            , testCase "handles string with only whitespace" $ do
                trim "   \t  " @?= ""
            
            , testCase "preserves internal whitespace" $ do
                trim "  hello world  " @?= "hello world"
            ]
        
        , testGroup "Edge cases"
            [ testCase "handles newlines" $ do
                trim "\nhello\n" @?= "hello"
            
            , testCase "handles mixed whitespace types" $ do
                trim "\n\t  hello  \t\n" @?= "hello"
            
            , testCase "handles single character" $ do
                trim "a" @?= "a"
            
            , testCase "handles single whitespace character" $ do
                trim " " @?= ""
            ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "trim never increases string length" $
                \s -> length (trim s) <= length s
            
            , fastProperty "trim is idempotent" $
                \s -> trim (trim s) == trim s
            
            , fastProperty "trim removes all leading and trailing whitespace" $
                \s -> let trimmed = trim s
                       in (null trimmed || head trimmed `notElem` " \t\n\r") &&
                          (null trimmed || last trimmed `notElem` " \t\n\r")
            ]
        ]
    
    , testGroup "breakOn function integration"
        [ testGroup "Real-world scenarios"
            [ testCase "handles URL parsing" $ do
                let url = "https://example.com/path/resource"
                    (domain, path) = breakOn "/path" url
                domain @?= "https://example.com"
                path @?= "resource"
            
            , testCase "handles file path parsing" $ do
                let filePath = "/home/user/documents/file.txt"
                    (dir, file) = breakOn "/file.txt" filePath
                dir @?= "/home/user/documents"
                file @?= ""
            
            , testCase "handles protocol separation" $ do
                let fullUrl = "ftp://files.example.com/data"
                    (protocol, rest) = breakOn "://" fullUrl
                protocol @?= ""
                rest @?= "files.example.com/data"
            ]
        
        , testGroup "Error handling"
            [ testCase "handles pattern not found" $ do
                let text = "hello world"
                    (prefix, suffix) = breakOn "xyz" text
                prefix @?= text
                suffix @?= ""
            
            , testCase "handles empty pattern" $ do
                let text = "hello"
                    (prefix, suffix) = breakOn "" text
                prefix @?= ""
                suffix @?= text
            ]
        ]
    
    , testGroup "Combined string operations"
        [ testCase "trim and breakOn combination" $ do
            let input = "  https://example.com/path  "
                trimmed = trim input
                (domain, path) = breakOn "/path" trimmed
            domain @?= "https://example.com"
            path @?= ""
        
        , testCase "multiple breakOn operations" $ do
            let url = "https://sub.domain.example.com/path/to/resource"
                (protocol, rest1) = breakOn "://" url
                (domain, rest2) = breakOn "/" (drop 3 rest1)  -- Drop ://
                (firstPath, remaining) = breakOn "/" rest2
            protocol @?= ""
            firstPath @?= "sub.domain.example.com"
            remaining @?= "to/resource"
        ]
    
    , testGroup "Performance and stress tests"
        [ testCase "handles very long strings" $ do
            let longString = replicate 10000 'a' ++ "marker" ++ replicate 10000 'b'
                (prefix, suffix) = breakOn "marker" longString
            length prefix @?= 10000
            length suffix @?= 10000
        
        , testCase "handles many small operations" $ do
            let words = ["word1", "word2", "word3", "word4", "word5"]
                processed = map trim words
            processed @?= words
        
        , testCase "handles unicode strings" $ do
            let unicodeText = "  héllo wörld 🌟  "
                trimmed = trim unicodeText
            trimmed @?= "héllo wörld 🌟"
        ]
    
    , testGroup "QuickCheck integration properties"
        [ fastProperty "trim after breakOn preserves content" $
            \s pat -> let (prefix, suffix) = breakOn pat s
                          trimmedPrefix = trim prefix
                          trimmedSuffix = trim suffix
                      in (trimmedPrefix ++ pat ++ trimmedSuffix) `isSubsequenceOf` (s ++ pat)
        
        , fastProperty "breakOn on trimmed string behaves predictably" $
            \s pat -> let trimmed = trim s
                          (prefix, suffix) = breakOn pat trimmed
                      in null prefix || head prefix `notElem` " \t\n\r"
        
        , fastProperty "trim doesn't affect breakOn results for non-whitespace patterns" $
            \s pat -> not (any (`elem` " \t\n\r") pat) ==>
                      let (p1, s1) = breakOn pat s
                          (p2, s2) = breakOn pat (trim s)
                      in (trim p1, s1) == (p2, s2)
        ]
    ]

-- Helper function to check if one string is a subsequence of another
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
    | x == y    = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf (x:xs) ys