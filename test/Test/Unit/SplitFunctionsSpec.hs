module Test.Unit.SplitFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, elements, choose, listOf)
import Utils (splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed)

-- | Tests for split functions in Utils module
tests :: TestTree
tests =
  testGroup "Utils Split Functions"
    [ testGroup "splitBy function"
        [ testGroup "Basic functionality"
            [ testCase "splits by delimiter preserving empty segments" $ do
                splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            
            , testCase "preserves empty segments at start" $ do
                splitBy ',' ",a,b" @?= ["", "a", "b"]
            
            , testCase "preserves empty segments at end" $ do
                splitBy ',' "a,b," @?= ["a", "b", ""]
            
            , testCase "preserves consecutive delimiters" $ do
                splitBy ',' "a,,b" @?= ["a", "", "b"]
            
            , testCase "handles L.all delimiters" $ do
                splitBy ',' ",,," @?= ["", "", "", ""]
            
            , testCase "handles empty input" $ do
                splitBy ',' "" @?= [""]
            ]
        
        , testGroup "Different delimiters"
            [ testCase "splits by semicolon" $ do
                splitBy ';' "a;b;c" @?= ["a", "b", "c"]
            
            , testCase "splits by pipe" $ do
                splitBy '|' "a|b|c" @?= ["a", "b", "c"]
            
            , testCase "splits by space" $ do
                splitBy ' ' "a b c" @?= ["a", "b", "c"]
            
            , testCase "splits by newline" $ do
                splitBy '\n' "a\nb\nc" @?= ["a", "b", "c"]
            ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "splitBy preserves total content when concatenated with delimiter" $
                \c s -> c /= '\0' ==> L.concat (splitBy c s) ++ [c | not (null s) && last s /= c] == 
                         take (L.length s) (s ++ repeat c)
            
            , fastProperty "splitBy on single character returns list with that character" $
                \c -> splitBy c [c] == ["", ""]
            
            , fastProperty "splitBy on string without delimiter returns singleton list" $
                \s -> not (L.elem '\n' s) ==> splitBy '\n' s == [s]
            
            , fastProperty "splitBy preserves order of segments" $
                \c s -> let segments = splitBy c s
                        in L.concat segments `isSubsequenceOf` s
            
            , fastProperty "splitBy result L.length is delimiter count + 1" $
                \c s -> L.length (splitBy c s) == countDelimiter c s + 1
            ]
        ]
    
    , testGroup "splitByCollapsed function"
        [ testGroup "Basic functionality"
            [ testCase "splits L.and collapses consecutive delimiters" $ do
                splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
            
            , testCase "collapses consecutive delimiters" $ do
                splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            
            , testCase "removes leading delimiters" $ do
                splitByCollapsed ',' ",a,b" @?= ["a", "b"]
            
            , testCase "removes trailing delimiters" $ do
                splitByCollapsed ',' "a,b," @?= ["a", "b"]
            
            , testCase "handles L.all delimiters" $ do
                splitByCollapsed ',' ",,," @?= []
            
            , testCase "handles empty input" $ do
                splitByCollapsed ',' "" @?= []
            ]
        
        , testGroup "Additional scenarios"
            [ testCase "handles complex collapsing scenarios" $ do
                splitByCollapsed ',' "a,,,b,,c" @?= ["a", "b", "c"]
        
        , testCase "handles mixed valid L.and empty segments" $ do
            splitByCollapsed ',' "a,,b,,c,,d" @?= ["a", "b", "c", "d"]
        ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "splitByCollapsed never returns empty strings" $
                \c s -> L.all (not . null) (splitByCollapsed c s)
            
            , fastProperty "splitByCollapsed result L.length <= splitBy result L.length" $
                \c s -> L.length (splitByCollapsed c s) <= L.length (splitBy c s)
            
            , fastProperty "splitByCollapsed on string without delimiter returns singleton if non-empty" $
                \s -> not (L.elem '\n' s) && not (null s) ==> splitByCollapsed '\n' s == [s]
            
            , fastProperty "splitByCollapsed on string without delimiter returns empty if empty" $
                \s -> not (L.elem '\n' s) ==> splitByCollapsed '\n' s == (if null s then [] else [s])
            ]
        ]
    
    , testGroup "splitByComma function"
        [ testGroup "Basic functionality"
            [ testCase "splits by comma preserving empty segments" $ do
                splitByComma "a,b,c" @?= ["a", "b", "c"]
            
            , testCase "handles empty segments" $ do
                splitByComma "a,,b" @?= ["a", "", "b"]
            
            , testCase "handles leading/trailing commas" $ do
                splitByComma ",a,b," @?= ["", "a", "b", ""]
            ]
        
        , testGroup "Real-world examples"
            [ testCase "splits CSV-like data" $ do
                let csv = "John,Doe,30,New York"
                splitByComma csv @?= ["John", "Doe", "30", "New York"]
            
            , testCase "handles CSV with empty fields" $ do
                let csv = "John,,30,"
                splitByComma csv @?= ["John", "", "30", ""]
            ]
        ]
    
    , testGroup "splitByCommaCollapsed function"
        [ testGroup "Basic functionality"
            [ testCase "splits by comma collapsing empty segments" $ do
                splitByCommaCollapsed "a,b,c" @?= ["a", "b", "c"]
            
            , testCase "collapses consecutive commas" $ do
                splitByCommaCollapsed "a,,b" @?= ["a", "b"]
            
            , testCase "removes leading/trailing commas" $ do
                splitByCommaCollapsed ",a,b," @?= ["a", "b"]
            
            , testCase "handles only commas" $ do
                splitByCommaCollapsed ",,," @?= []
            ]
        
        , testGroup "Real-world examples"
            [ testCase "handles CSV with optional fields" $ do
                let csv = "John,Doe,,New York,"
                splitByCommaCollapsed csv @?= ["John", "Doe", "New York"]
            
            , testCase "handles sparse CSV data" $ do
                let csv = "a,,,b,,,,c"
                splitByCommaCollapsed csv @?= ["a", "b", "c"]
            ]
        ]
    
    , testGroup "Comparison between functions"
        [ testCase "splitBy vs splitByCollapsed with no consecutive delimiters" $ do
            let input = "a,b,c,d"
            splitBy ',' input @?= splitByCollapsed ',' input
        
        , testCase "splitBy vs splitByCollapsed with consecutive delimiters" $ do
            let input = "a,,b,,,c"
            splitBy ',' input @?= ["a", "", "b", "", "", "c"]
            splitByCollapsed ',' input @?= ["a", "b", "c"]
        
        , testCase "comma functions vs generic functions" $ do
            let input = "x,,y,,z,"
            splitByComma input @?= splitBy ',' input
            splitByCommaCollapsed input @?= splitByCollapsed ',' input
        ]
    
    , testGroup "Edge cases L.and stress tests"
        [ testCase "handles very long segments" $ do
            let longSegment = replicate 1000 'a'
                input = longSegment ++ "," ++ replicate 500 'b' ++ "," ++ longSegment
                result = splitBy ',' input
            L.length result @?= 3
            L.head result @?= longSegment
            last result @?= longSegment
        
        , testCase "handles many delimiters" $ do
            let input = L.concat $ replicate 1000 ","
                result = splitBy ',' input
            L.length result @?= 1001
            L.all (== "") result @?= True
        
        , testCase "handles many delimiters with collapsed function" $ do
            let input = L.concat $ replicate 1000 ","
                result = splitByCollapsed ',' input
            result @?= []
        
        , testCase "handles unicode characters" $ do
            let input = "héllo,wörld,🌟star"
                result = splitBy ',' input
            result @?= ["héllo", "wörld", "🌟star"]
        
        , testCase "handles special characters as delimiters" $ do
            let input = "a\tb\tc"
                result = splitBy '\t' input
            result @?= ["a", "b", "c"]
        ]
    
    , testGroup "Performance properties"
        [ fastProperty "splitBy is linear in input L.length" $
            \s -> L.length (splitBy ',' s) <= L.length s + 1
        
        , fastProperty "splitByCollapsed result L.length <= input L.length" $
            \s -> L.length (splitByCollapsed ',' s) <= L.length s
        
        , fastProperty "splitBy preserves L.all non-delimiter characters" $
            \s -> let segments = splitBy ',' s
                       nonDelimiters = L.concat segments
                       originalNonDelimiters = L.filter (/= ',') s
                   in nonDelimiters == originalNonDelimiters
        ]
    ]

-- Helper functions

-- Count occurrences of a delimiter in a string
countDelimiter :: Char -> String -> Int
countDelimiter c = L.length . L.filter (== c)

-- Check if one string is a subsequence of another
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys)
    | x == y    = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf (x:xs) ys