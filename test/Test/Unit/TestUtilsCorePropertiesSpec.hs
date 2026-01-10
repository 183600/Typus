{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestUtilsCorePropertiesSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import TestSupport.Arbitrary ()

-- | Test suite for Utils core functionality
testUtilsCoreProperties :: TestTree
testUtilsCoreProperties = testGroup "Utils Core Properties Tests"
  [ testProperty "trim: trim (trim x) == trim x" $
      \x -> trim (trim x) == trim x
      
  , testProperty "splitBy: length (splitBy c x) >= 1" $
      \c x -> length (splitBy c x) >= 1
      
  , testProperty "splitBy: concat (splitBy c x) with delimiter = x" $
      \c x -> concat (intersperse [c] (splitBy c x)) == x
      
  , testProperty "splitByComma: splitByComma == splitBy ','" $
      \x -> splitByComma x == splitBy ',' x
      
  , testProperty "removeLineComments: removing comments twice is idempotent" $
      \x -> removeLineComments (removeLineComments x) == removeLineComments x
      
  , testProperty "normalizeIndentation: normalizing twice is idempotent" $
      \x -> normalizeIndentation (normalizeIndentation x) == normalizeIndentation x
      
  , testProperty "breakOn: if pattern found, concatenating parts with pattern = original" $
      \pat s -> case breakOn pat s of
                  (before, after) -> if pat `isInfixOf` s
                                     then before ++ pat ++ after == s
                                     else True
                                     
  , testProperty "safeProcessString: valid characters are preserved" $
      \s -> let filtered = filter isValidChar s
             in case safeProcessString s of
                  Left _ -> null filtered
                  Right result -> all isValidChar result
                  
  , testProperty "isValidChar: control characters are invalid (except newline, tab, carriage return)" $
      \c -> if c < ' ' && c `notElem` ['\n', '\t', '\r']
             then not (isValidChar c)
             else isValidChar c
             
  , testCase "removeComments: handles nested block comments correctly" $
      removeComments "/* outer /* inner */ still outer */ end" @?= " end"
      
  , testCase "removeComments: preserves string literals with comment-like content" $
      removeComments "const s = \"/* not a comment */\";" @?= "const s = \"/* not a comment */\";"
      
  , testCase "removeComments: preserves character literals with comment-like content" $
      removeComments "const c = '/* not a comment */';" @?= "const c = '/* not a comment */';"
      
  , testCase "normalizeIndentation: handles mixed tabs and spaces" $
      normalizeIndentation "\t    line1\n\t      line2\n    line3" @?= "line1\n  line2\nline3"
  ]

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys