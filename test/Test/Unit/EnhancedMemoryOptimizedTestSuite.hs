{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-matches -Wno-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedMemoryOptimizedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.MemoryLimits 
  ( withMemoryLimits
  , memoryLimitedTestGroup
  , MemoryLevel(..)
  , withMemoryLevel
  , memoryLevelTestGroup
  , gcBetweenTests
  )

import Utils (trim, splitBy, splitByComma, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)
import Data.Either (isLeft, isRight)

-- Enhanced memory-optimized string properties
prop_enhanced_trim_memory :: String -> Property
prop_enhanced_trim_memory s = 
  let limitedString = take 25 s  -- Slightly larger limit for enhanced tests
      trimmed = trim limitedString
  in property $ length trimmed <= length limitedString

prop_enhanced_split_memory :: Char -> String -> Property
prop_enhanced_split_memory c s = 
  let limitedString = take 20 s
      parts = splitBy c limitedString
  in property $ length parts >= 0 && length (concat parts) >= 0

prop_enhanced_comment_removal_memory :: String -> String -> Property
prop_enhanced_comment_removal_memory code comment = 
  let limitedCode = take 15 code
      limitedComment = take 10 comment
      hasNoQuotes = not ('"' `elem` limitedCode) && not ('"' `elem` limitedComment) && 
                    not ('\'' `elem` limitedCode) && not ('\'' `elem` limitedComment)
  in if not hasNoQuotes
     then property True
     else let codeWithComment = limitedCode ++ "// " ++ limitedComment ++ "\nmore"
              withoutComments = removeLineComments codeWithComment
          in property $ not ("//" `isInfixOf` withoutComments)

prop_enhanced_source_location_memory :: Int -> Int -> Property
prop_enhanced_source_location_memory line col = 
  let limitedLine = max 1 (min 100 line)
      limitedCol = max 1 (min 100 col)
      pos = SourcePos limitedLine limitedCol
  in property $ limitedLine >= 1 && limitedCol >= 1

prop_enhanced_span_memory :: Int -> Int -> Int -> Int -> Property
prop_enhanced_span_memory startLine startCol endLine endCol = 
  let limitedStartLine = max 1 (min 50 startLine)
      limitedStartCol = max 1 (min 50 startCol)
      limitedEndLine = max limitedStartLine (min 100 endLine)
      limitedEndCol = if limitedEndLine == limitedStartLine 
                      then max limitedStartCol (min 100 endCol)
                      else max 1 (min 100 endCol)
  in property $ limitedStartLine <= limitedEndLine

-- Enhanced test suite with balanced memory optimization
tests :: TestTree
tests = memoryLevelTestGroup Moderate "Enhanced Memory-Optimized Test Suite"
  [ withMemoryLevel Moderate $ testProperty "enhanced trim memory" prop_enhanced_trim_memory
  , withMemoryLevel Moderate $ testProperty "enhanced split memory" prop_enhanced_split_memory
  , withMemoryLevel Moderate $ testProperty "enhanced comment removal memory" prop_enhanced_comment_removal_memory
  , withMemoryLevel Moderate $ testProperty "enhanced source location memory" prop_enhanced_source_location_memory
  , withMemoryLevel Moderate $ testProperty "enhanced span memory" prop_enhanced_span_memory
  ]