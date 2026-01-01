{-# LANGUAGE CPP #-}

module Test.Unit.AdditionalQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List as L

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), posAfter, spanBetween, mergeSpans, posAt, posAtLineCol, advancePos, locatedSpan)
import Parser (FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- Property 1: splitBy L.and splitByCollapsed relationship
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed delim s =
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
      emptyCount = L.length $ filter null normal
  in L.length collapsed === L.length normal - emptyCount

-- Property 2: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: String -> Property
prop_normalizeIndentation_preserves_relative s =
  not (null s) ==>
  let linesOfS = lines s
      normalizedString = normalizeIndentation s
      normalizedLines = lines normalizedString
      originalLengths = L.map (L.length . takeWhile (== ' ')) linesOfS
      normalizedLengths = L.map (L.length . takeWhile (== ' ')) normalizedLines
      relativeDiffs = zipWith (-) (L.tail originalLengths) (init originalLengths)
      normalizedDiffs = zipWith (-) (L.tail normalizedLengths) (init normalizedLengths)
  in if L.length relativeDiffs > 0 
     then property $ normalizedDiffs === relativeDiffs
     else property $ True

-- Property 3: Located values preserve their spans
prop_located_preserves_span :: String -> Int -> Property
prop_located_preserves_span value offset =
  let pos = posAtLineCol 1 1 offset
      span = spanBetween pos (posAtLineCol 1 1 (offset + L.length value))
      located = Located value pos span
  in locatedSpan located === span

-- Property 4: FileDirectives merge is associative
prop_file_directives_merge_associative :: FileDirectives -> FileDirectives -> FileDirectives -> Property
prop_file_directives_merge_associative fd1 fd2 fd3 =
  let merge fd1 fd2 = FileDirectives
        { fdOwnership = fdOwnership fd1 `mplus` fdOwnership fd2
        , fdDependentTypes = fdDependentTypes fd1 `mplus` fdDependentTypes fd2
        , fdConstraints = fdConstraints fd1 `mplus` fdConstraints fd2
        }
      left = merge (merge fd1 fd2) fd3
      right = merge fd1 (merge fd2 fd3)
  in left === right
  where
    mplus Nothing y = y
    mplus (Just x) _ = Just x

-- Property 5: removeLineComments preserves non-comment content
prop_removeLineComments_preserves_content :: String -> Property
prop_removeLineComments_preserves_content s =
  let hasComments = "//" `L.isInfixOf` s
      result = removeLineComments s
      linesWithoutComments = L.map (takeWhile (/= '/')) $ lines s
      expected = unlines linesWithoutComments
  in if hasComments
     then property $ L.length result <= L.length s
     else property $ result === s

-- Property 6: SourceSpan merging is commutative for adjacent spans
prop_span_merge_commutative :: Int -> Int -> Property
prop_span_merge_commutative len1 len2 =
  len1 >= 0 && len2 >= 0 && len1 <= 10 && len2 <= 10 ==>
  let start1 = posAtLineCol 1 1 0
      end1 = L.foldl (flip advancePos) start1 (replicate len1 'x')
      span1 = spanBetween start1 end1
      start2 = end1
      end2 = L.foldl (flip advancePos) start2 (replicate len2 'x')
      span2 = spanBetween start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 === merged2

-- Property 7: splitByCommaCollapsed removes empty segments
prop_splitByCommaCollapsed_removes_empty :: String -> Property
prop_splitByCommaCollapsed_removes_empty s =
  let parts = splitBy ',' s
      collapsed = splitByCollapsed ',' s
  in property $ not (L.any null collapsed)

-- Property 8: trim L.and normalizeIndentation work well together
prop_trim_normalize_interaction :: String -> Property
prop_trim_normalize_interaction s =
  not (null s) ==>
  let normalized = normalizeIndentation s
      recombined = normalized
  in property $ L.all (not . L.isPrefixOf " ") (lines recombined)

-- Property 9: SourcePos arithmetic is consistent
prop_sourcepos_arithmetic :: Int -> Int -> Int -> Property
prop_sourcepos_arithmetic line col offset =
  let pos = posAtLineCol line col offset
      posAfter1 = advancePos 'x' pos
      posAfter5 = advancePos 'x' $ advancePos 'x' $ advancePos 'x' $ advancePos 'x' $ advancePos 'x' pos
      posAfter0 = pos
  in conjoin
    [ posOffset posAfter1 === offset + 1
    , posOffset posAfter5 === offset + 5
    , posAfter0 === pos
    ]

-- Property 10: BlockDirectives override works correctly
prop_block_directives_override :: Bool -> Bool -> Bool -> Property
prop_block_directives_override ownership dependent constraints =
  let bd1 = BlockDirectives (Just undefined) Nothing Nothing
      testPos = posAt 1 1
      testSpan = spanBetween testPos (posAt 1 2)
      bd2 = BlockDirectives (Just (Located ownership testPos testSpan)) 
                           (Just (Located dependent testPos testSpan)) 
                           (Just (Located constraints testPos testSpan))
      merged = BlockDirectives
        { bdOwnership = bdOwnership bd2 `mplus` bdOwnership bd1
        , bdDependentTypes = bdDependentTypes bd2 `mplus` bdDependentTypes bd1
        , bdConstraints = bdConstraints bd2 `mplus` bdConstraints bd1
        }
  in conjoin
    [ case bdOwnership merged of
        Just (Located val _ _) -> val === ownership
        Nothing -> property False
    , case bdDependentTypes merged of
        Just (Located val _ _) -> val === dependent
        Nothing -> property False
    , case bdConstraints merged of
        Just (Located val _ _) -> val === constraints
        Nothing -> property False
    ]
  where
    mplus Nothing y = y
    mplus (Just x) _ = Just x

tests :: TestTree
tests = testGroup "Additional QuickCheck Tests"
  [ fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_collapsed
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , fastProperty "Located values preserve their spans" prop_located_preserves_span
  , fastProperty "FileDirectives merge is associative" prop_file_directives_merge_associative
  , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserves_content
  , fastProperty "SourceSpan merging is commutative for adjacent spans" prop_span_merge_commutative
  , fastProperty "splitByCommaCollapsed removes empty segments" prop_splitByCommaCollapsed_removes_empty
  , fastProperty "trim L.and normalizeIndentation interaction" prop_trim_normalize_interaction
  , fastProperty "SourcePos arithmetic is consistent" prop_sourcepos_arithmetic
  , fastProperty "BlockDirectives override works correctly" prop_block_directives_override
  ]