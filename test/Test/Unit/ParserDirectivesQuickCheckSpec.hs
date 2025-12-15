{-# LANGUAGE CPP #-}

module Test.Unit.ParserDirectivesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Parser
import SourceLocation (spanStart, spanEnd, posOffset)
import TestSupport.Arbitrary ()

prop_fileDirectives_default_all_nothing :: Property
prop_fileDirectives_default_all_nothing =
  let fd = defaultFileDirectives
  in fdOwnership fd === Nothing .&&.
     fdDependentTypes fd === Nothing .&&.
     fdConstraints fd === Nothing

prop_blockDirectives_default_all_nothing :: Property
prop_blockDirectives_default_all_nothing =
  let bd = defaultBlockDirectives
  in bdOwnership bd === Nothing .&&.
     bdDependentTypes bd === Nothing .&&.
     bdConstraints bd === Nothing

prop_codeBlock_has_span :: CodeBlock -> Property
prop_codeBlock_has_span block =
  let span = cbSpan block
      start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_typusFile_blocks_valid :: TypusFile -> Property
prop_typusFile_blocks_valid file =
  let blocks = tfBlocks file
      validBlock block = 
        let span = cbSpan block
            start = spanStart span
            end = spanEnd span
        in posOffset start <= posOffset end
  in property $ all validBlock blocks

tests :: TestTree
tests = testGroup "Parser Directives QuickCheck"
  [ fastProperty "FileDirectives default is all Nothing" prop_fileDirectives_default_all_nothing
  , fastProperty "BlockDirectives default is all Nothing" prop_blockDirectives_default_all_nothing
  , fastProperty "CodeBlock has valid span" prop_codeBlock_has_span
  , fastProperty "TypusFile blocks are valid" prop_typusFile_blocks_valid
  ]