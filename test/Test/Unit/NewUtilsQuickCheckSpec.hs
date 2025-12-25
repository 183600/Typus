{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils
import Data.Char (isSpace)
import qualified Data.Text as T

-- | Test properties for Utils module
tests :: TestTree
tests = testGroup "Utils QuickCheck Tests"
  [ testProperty "trim: idempotent property" propTrimIdempotent
  , testProperty "trim: removes leading and trailing whitespace" propTrimRemovesWhitespace
  , testProperty "splitBy: preserves empty segments" propSplitByPreservesEmpty
  , testProperty "splitByCollapsed: removes empty segments" propSplitByCollapsedRemovesEmpty
  , testProperty "splitByComma: same as splitBy ','" propSplitByCommaEqualsSplitBy
  , testProperty "splitByCommaCollapsed: same as splitByCollapsed ','" propSplitByCommaCollapsedEqualsSplitByCollapsed
  , testProperty "removeLineComments: preserves content without comments" propRemoveLineCommentsPreservesContent
  , testProperty "removeComments: preserves content without comments" propRemoveCommentsPreservesContent
  , testProperty "removeComments: handles block comments correctly" propRemoveCommentsHandlesBlockComments
  , testProperty "normalizeIndentation: preserves relative indentation" propNormalizeIndentationPreservesRelative
  , testProperty "breakOn: correct split properties" propBreakOnCorrectSplit
  ]

-- | trim: Applying trim twice should give same result as applying once
propTrimIdempotent :: String -> Bool
propTrimIdempotent s = trim (trim s) == trim s

-- | trim: Result should have no leading or trailing whitespace
propTrimRemovesWhitespace :: String -> Bool
propTrimRemovesWhitespace s = 
  let trimmed = trim s
  in null trimmed || (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

-- | splitBy: Should preserve empty segments
propSplitByPreservesEmpty :: Char -> String -> Property
propSplitByPreservesEmpty delim s = 
  let parts = splitBy delim s
      rejoined = intercalate [delim] parts
  in counterexample ("Original: " ++ show s ++ ", Parts: " ++ show parts ++ ", Rejoined: " ++ show rejoined) $
     rejoined == s

-- | splitByCollapsed: Should remove empty segments
propSplitByCollapsedRemovesEmpty :: Char -> String -> Bool
propSplitByCollapsedRemovesEmpty delim s = 
  let parts = splitByCollapsed delim s
  in all (not . null) parts

-- | splitByComma: Should be equivalent to splitBy ','
propSplitByCommaEqualsSplitBy :: String -> Bool
propSplitByCommaEqualsSplitBy s = splitByComma s == splitBy ',' s

-- | splitByCommaCollapsed: Should be equivalent to splitByCollapsed ','
propSplitByCommaCollapsedEqualsSplitByCollapsed :: String -> Bool
propSplitByCommaCollapsedEqualsSplitByCollapsed s = 
  splitByCommaCollapsed s == splitByCollapsed ',' s

-- | removeLineComments: Should preserve content without line comments
propRemoveLineCommentsPreservesContent :: String -> Property
propRemoveLineCommentsPreservesContent s = 
  not ('/' `elem` s) ==> removeLineComments s == s

-- | removeComments: Should preserve content without comments
propRemoveCommentsPreservesContent :: String -> Property
propRemoveCommentsPreservesContent s = 
  not ('/' `elem` s) ==> removeComments s == s

-- | removeComments: Should handle block comments correctly
propRemoveCommentsHandlesBlockComments :: String -> String -> Property
propRemoveCommentsHandlesBlockComments prefix suffix = 
  let comment = "/* comment */"
      input = prefix ++ comment ++ suffix
      result = removeComments input
  in counterexample ("Input: " ++ show input ++ ", Result: " ++ show result) $
     not (comment `isInfixOf` result)

-- | normalizeIndentation: Should preserve relative indentation
propNormalizeIndentationPreservesRelative :: String -> Property
propNormalizeIndentationPreservesRelative s = 
  let lines' = lines s
  in length lines' > 1 ==> 
     let normalized = normalizeIndentation s
         normLines = lines normalized
         -- Check that relative indentation is preserved
         checkRelativeIndent [] [] = True
         checkRelativeIndent (l1:ls1) (l2:ls2) = 
           let indent1 = length (takeWhile isSpace l1)
               indent2 = length (takeWhile isSpace l2)
           in (if null (dropWhile isSpace l1) then null (dropWhile isSpace l2) else True) &&
              checkRelativeIndent ls1 ls2
         checkRelativeIndent _ _ = False
     in checkRelativeIndent lines' normLines

-- | breakOn: Should correctly split strings
propBreakOnCorrectSplit :: String -> String -> Property
propBreakOnCorrectSplit pat s = 
  not (null pat) ==> 
    let (before, after) = breakOn pat s
        expected = if pat `isInfixOf` s
                   then let parts = splitOn' pat s
                        in case parts of
                             [] -> (s, "")
                             [x] -> (x, "")
                             (x:xs) -> (x, intercalate pat xs)
                   else (s, "")
    in counterexample ("Pattern: " ++ show pat ++ ", String: " ++ show s ++ 
                      ", Result: (" ++ show before ++ ", " ++ show after ++ ")" ++
                      ", Expected: (" ++ show (fst expected) ++ ", " ++ show (snd expected) ++ ")") $
       (before, after) == expected

-- Helper function to split on first occurrence
splitOn' :: Eq a => [a] -> [a] -> [[a]]
splitOn' _ [] = [[]]
splitOn' pat str = 
  case findIndex' pat str of
    Nothing -> [str]
    Just idx -> 
      let (before, rest) = splitAt idx str
          after = drop (length pat) rest
      in [before, after]

-- Helper function to find index of sublist
findIndex' :: Eq a => [a] -> [a] -> Maybe Int
findIndex' pat str = findIndexHelper pat str 0

findIndexHelper :: Eq a => [a] -> [a] -> Int -> Maybe Int
findIndexHelper [] _ _ = Just 0
findIndexHelper _ [] _ = Nothing
findIndexHelper pat@(p:ps) str@(s:ss) idx
  | p == s = case findIndexHelper ps ss idx of
               Just 0 -> Just idx
               Just n -> findIndexHelper pat (tail str) (idx + 1)
               Nothing -> findIndexHelper pat (tail str) (idx + 1)
  | otherwise = findIndexHelper pat (tail str) (idx + 1)

-- Helper function for intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Helper function for isInfixOf
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'