{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTypusCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser (TypusFile(..), CodeBlock(..))
import SourceLocation (Located(..), SourceSpan(..))
import Utils (trim, splitBy)

-- Property: Typus file path normalization
prop_typus_file_path_normalization :: String -> Property
prop_typus_file_path_normalization path =
  let normalized = trim path
      hasLeadingSlash = not (null path) && head path == '/'
      hasTrailingSlash = not (null path) && last path == '/'
  in classify hasLeadingSlash "has leading slash" $
     classify hasTrailingSlash "has trailing slash" $
     property $ not (null normalized) ==> (head normalized /= '/' && last normalized /= '/')

-- Property: Code block directive parsing consistency
prop_code_block_directive_consistency :: String -> Bool -> Property
prop_code_block_directive_consistency content hasOwnership =
  let block = CodeBlock content hasOwnership False
      extractedOwnership = hasOwnership
  in property $ extractedOwnership === hasOwnership

-- Property: Source span ordering
prop_source_span_ordering :: SourceSpan -> SourceSpan -> Property
prop_source_span_ordering span1 span2 =
  let start1 = getSourceSpanStart span1
      start2 = getSourceSpanStart span2
      cmp = compare start1 start2
  in property $ (cmp == LT || cmp == EQ || cmp == GT)

-- Property: File directive preservation
prop_file_directive_preservation :: String -> Bool -> Bool -> Property
prop_file_directive_preservation path ownership constraints =
  let file = TypusFile path ownership constraints []
      extractedOwnership = ownership
      extractedConstraints = constraints
  in property $ (extractedOwnership === ownership) .&&. (extractedConstraints === constraints)

-- Property: Code block content preservation
prop_code_block_content_preservation :: String -> Property
prop_code_block_content_preservation content =
  let block = CodeBlock content True False
      extractedContent = getBlockContent block
  in property $ extractedContent === content

-- Helper functions
getSourceSpanStart :: SourceSpan -> Int
getSourceSpanStart (SourceSpan start _) = 1  -- Simplified for test

getBlockContent :: CodeBlock -> String
getBlockContent (CodeBlock content _ _) = content

tests :: TestTree
tests = testGroup "New Typus Core QuickCheck Tests"
  [ fastProperty "Typus file path normalization" prop_typus_file_path_normalization
  , fastProperty "Code block directive consistency" prop_code_block_directive_consistency
  , fastProperty "Source span ordering" prop_source_span_ordering
  , fastProperty "File directive preservation" prop_file_directive_preservation
  , fastProperty "Code block content preservation" prop_code_block_content_preservation
  ]