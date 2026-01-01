{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..), arbitrary)
import Test.Tasty.HUnit (testCase, assert)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, normalizeIndentation)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof, listOf, elements)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- | Generate arbitrary strings with various whitespace patterns
newtype WhitespaceString = WhitespaceString String
  deriving (Show)

instance Arbitrary WhitespaceString where
  arbitrary = do
    content <- listOf $ oneof 
      [ arbitrary `suchThat` (not . null)
      , elements [" ", "\t", "\n", "  ", "\t\t", "\n\n", " \t ", " \n "]
      ]
    return $ WhitespaceString content

-- | Generate arbitrary strings with comma separators
newtype CommaString = CommaString String
  deriving (Show)

instance Arbitrary CommaString where
  arbitrary = do
    parts <- listOf (listOf $ arbitrary `suchThat` (/= ','))
    let sep = oneof [return ",", return ",,", return ",,,", return ""]
    separator <- sep
    return $ CommaString $ L.concat $ intersperse separator parts
    where
      intersperse _ [] = []
      intersperse _ [x] = [x]
      intersperse sep (x:xs) = x : sep : intersperse sep xs

-- | Generate strings with C-style line comments
newtype CommentString = CommentString String
  deriving (Show)

instance Arbitrary CommentString where
  arbitrary = do
    beforeComment <- listOf $ arbitrary `suchThat` (/= '/')
    comment <- listOf $ arbitrary `suchThat` (/= '\n')
    afterComment <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ CommentString $ beforeComment ++ "//" ++ comment ++ "\n" ++ afterComment

tests :: TestTree
tests = testGroup "Utils Boundary Tests"
  [ testProperty "trim idempotent" $ \(WhitespaceString s) ->
      let trimmedOnce = trim s
          trimmedTwice = trim trimmedOnce
      in trimmedOnce == trimmedTwice
  
  , testProperty "trim removes only leading/trailing whitespace" $ \(WhitespaceString s) ->
      let trimmed = trim s
          hasLeadingSpace = not (null trimmed) && isSpace (L.head trimmed)
          hasTrailingSpace = not (null trimmed) && isSpace (last trimmed)
      in not hasLeadingSpace && not hasTrailingSpace
  
  , testProperty "splitBy preserves empty segments" $ \(CommaString s) ->
      let segments = splitBy ',' s
          reconstructed = L.concat $ intersperse "," segments
      in reconstructed == s
  
  , testProperty "splitByCollapsed removes empty segments" $ \(CommaString s) ->
      let segments = splitByCollapsed ',' s
          hasEmpty = L.any null segments
      in not hasEmpty
  
  , testProperty "splitByComma equals splitBy with comma" $ \s ->
      splitByComma s == splitBy ',' s
  
  , testProperty "removeLineComments preserves content before comment" $ \(CommentString s) ->
      let beforeComment = takeWhile (/= '/') s
          result = removeLineComments s
      in beforeComment `L.isPrefixOf` result
  
  , testCase "trim handles empty string" $
      trim "" @?= ""
  
  , testCase "trim handles whitespace-only string" $
      trim "   \t\n  " @?= ""
  
  , testCase "splitBy handles empty string" $
      splitBy ',' "" @?= [""]
  
  , testCase "splitByCollapsed handles empty string" $
      splitByCollapsed ',' "" @?= []
  
  , testCase "removeLineComments handles string without comment" $
      removeLineComments "no comment here" @?= "no comment here"
  
  , testCase "normalizeIndentation handles single line" $
      normalizeIndentation "  hello" @?= "hello"
  
  , testProperty "normalizeIndentation preserves relative indentation" $ \s ->
      let lines' = lines s
          indented = L.map ("  " ++) lines'
          normalized = normalizeIndentation (unlines indented)
          originalLines = L.map (drop 2) (lines normalized)
      in L.length originalLines == L.length lines'
  ]