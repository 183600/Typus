{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Unit.TextProcessingPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements, forAll)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, breakOn)

tests :: TestTree
tests = testGroup "Text Processing Properties Tests"
  [ testGroup "Trim properties"
    [ testProperty "trim removes leading and trailing whitespace" $
        \s -> all isSpace (takeWhile isSpace s) ==> 
              all isSpace (reverse $ takeWhile isSpace $ reverse s) ==>
              trim s == s
    , testProperty "trim . trim = trim (idempotent)" $
        \s -> trim (trim s) == trim s
    , testProperty "trim never adds characters" $
        \s -> length (trim s) <= length s
    , testProperty "trim preserves non-whitespace content" $
        \s -> let t = filter (not . isSpace) s
               in filter (not . isSpace) (trim s) == t
    ]
  , testGroup "Split properties"
    [ testProperty "splitBy preserves total content" $
        \c s -> concat (splitBy c s) == s
    , testProperty "splitBy length property" $
        \c s -> length (splitBy c s) == length (filter (== c) s) + 1
    , testProperty "splitByCollapsed removes empty segments" $
        \c s -> all (not . null) (splitByCollapsed c s)
    , testProperty "splitByCollapsed length <= splitBy length" $
        \c s -> length (splitByCollapsed c s) <= length (splitBy c s)
    , testProperty "splitByComma = splitBy ','" $
        \s -> splitByComma s == splitBy ',' s
    , testProperty "splitByCommaCollapsed = splitByCollapsed ','" $
        \s -> splitByCommaCollapsed s == splitByCollapsed ',' s
    ]
  , testGroup "Comment removal properties"
    [ testProperty "removeLineComments never increases length" $
        \s -> length (removeLineComments s) <= length s
    , testProperty "removeLineComments preserves lines" $
        \s -> length (lines (removeLineComments s)) == length (lines s)
    , testProperty "removeComments never increases length" $
        \s -> length (removeComments s) <= length s
    , testProperty "removeComments removes // patterns" $
        \s -> "//" `isInfixOf` s ==> "//" `notIsInfixOf` removeComments s
    , testProperty "removeComments removes /* */ patterns" $
        forAll genStringWithBlockComment $ \s -> "/*" `isInfixOf` s ==> "/*" `notIsInfixOf` removeComments s
    ]
  , testGroup "Indentation properties"
    [ testProperty "normalizeIndentation preserves line count" $
        \s -> not (null s) ==> length (lines (normalizeIndentation s)) == length (lines s)
    , testProperty "normalizeIndentation doesn't create leading empty lines" $
        \s -> not (null s) ==> not (null (dropWhile (all isSpace) (lines (normalizeIndentation s))))
    , testProperty "normalizeIndentation preserves relative structure" $
        \s -> let original = lines s
                  normalized = lines (normalizeIndentation s)
                  originalNonEmpty = filter (not . all isSpace) original
                  normalizedNonEmpty = filter (not . all isSpace) normalized
              in length originalNonEmpty == length normalizedNonEmpty
    ]
  , testGroup "BreakOn properties"
    [ testProperty "breakOn pattern pattern = (\"\", pattern)" $
        \p -> not (null p) ==> breakOn p p == ("", "")
    , testProperty "breakOn empty pattern = (\"\", s)" $
        \s -> breakOn "" s == ("", s)
    , testProperty "breakOn concatenation property" $
        \p s -> let (before, after) = breakOn p s
                in if p `isInfixOf` s
                   then before ++ p ++ after == s
                   else before == s && after == ""
    , testProperty "breakOn never returns longer prefix than original" $
        \p s -> let (before, _) = breakOn p s
                in length before <= length s
    ]
  , testGroup "Combined operation properties"
    [ testProperty "trim after normalizeIndentation" $
        \s -> trim (normalizeIndentation s) == normalizeIndentation (trim s)
    , testProperty "removeComments after removeLineComments" $
        \s -> removeComments (removeLineComments s) == removeComments s
    , testProperty "splitBy after trim" $
        \c s -> splitBy c (trim s) == map trim (splitBy c s)
    ]
  ]

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

notIsInfixOf :: String -> String -> Bool
notIsInfixOf needle haystack = not (isInfixOf needle haystack)

-- Generators for specific test cases
genStringWithBlockComment :: Gen String
genStringWithBlockComment = do
  before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  return $ before ++ "/*" ++ comment ++ "*/" ++ after

-- Note: Arbitrary instance for String is provided by QuickCheck