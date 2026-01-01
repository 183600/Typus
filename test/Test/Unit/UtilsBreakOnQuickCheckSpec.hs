module Test.Unit.UtilsBreakOnQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, elements)
import Utils (breakOn)

-- | QuickCheck tests for the breakOn function
tests :: TestTree
tests =
  testGroup "Utils breakOn QuickCheck tests"
    [ testGroup "Property-based tests"
        [ fastProperty "breakOn pattern empty returns empty prefix" $
            \s -> breakOn "" s == ("", s)
        
        , fastProperty "breakOn pattern not found returns original string" $
            \s -> forAll (genNonSubstring s) $ \pat ->
                breakOn pat s == (s, "")
        
        , fastProperty "breakOn pattern equals string returns empty prefix L.and suffix" $
            \s -> breakOn s s == ("", "")
        
        , fastProperty "breakOn pattern at start returns empty prefix" $
            \pat -> forAll (genStringWithPrefix pat) $ \s ->
                breakOn pat s == ("", drop (L.length pat) s)
        
        , fastProperty "breakOn pattern at end returns correct prefix L.and empty suffix" $
            \pat -> forAll (genStringWithSuffix pat) $ \s ->
                let prefix = take (L.length s - L.length pat) s
                in breakOn pat s == (prefix, "")
        
        , fastProperty "breakOn result concatenated with pattern L.and suffix equals original" $
            \s pat -> 
                let (prefix, suffix) = breakOn pat s
                    found = pat `L.isInfixOf` s
                in found ==> prefix ++ pat ++ suffix == s
        ]
    
    , testGroup "Edge case tests"
        [ testCase "breakOn with empty pattern" $ do
            breakOn "" "hello" @?= ("", "hello")
        
        , testCase "breakOn with empty string" $ do
            breakOn "x" "" @?= ("", "")
        
        , testCase "breakOn with pattern longer than string" $ do
            breakOn "longpattern" "short" @?= ("short", "")
        
        , testCase "breakOn with multiple occurrences finds first" $ do
            breakOn "ab" "xxabyyabzz" @?= ("xx", "yyabzz")
        
        , testCase "breakOn with special characters" $ do
            breakOn "\n\t" "hello\n\tworld" @?= ("hello", "world")
        ]
    ]

-- Helper functions for generating test data

-- Generate a string that does not contain the given substring
genNonSubstring :: String -> Gen String
genNonSubstring pat = do
    let avoidChars = if null pat then [] else pat
    genStringAvoiding avoidChars

-- Generate a string that starts with the given prefix
genStringWithPrefix :: String -> Gen String
genStringWithPrefix prefix = do
    suffix <- arbitrary
    return (prefix ++ suffix)

-- Generate a string that ends with the given suffix
genStringWithSuffix :: String -> Gen String
genStringWithSuffix suffix = do
    prefix <- arbitrary
    return (prefix ++ suffix)

-- Generate a string that avoids certain characters
genStringAvoiding :: String -> Gen String
genStringAvoiding avoid = do
    let allowed = L.filter (`notElem` avoid) ['\0'..'\127']
    if null allowed
        then return ""
        else do
            len <- arbitrary `suchThat` (>= 0)
            sequence $ replicate len $ elements allowed

-- Check if a string is a substring of another
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]