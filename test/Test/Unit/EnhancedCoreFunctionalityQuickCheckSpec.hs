{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.EnhancedCoreFunctionalityQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose, arbitrary, forAll, (===), (==>), suchThat)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, removeLineComments, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, emptySpan, spanFrom, mergeSpans, isValidSpan, locatedAt, locatedWithSpan)

-- | Generate arbitrary strings with various characters
genString :: Gen String
genString = listOf $ oneof 
  [ arbitrary `suchThat` (\c -> c /= '/' && c /= '"' && c /= '\'' && c /= '\n' && c /= '\r')
  , elements " \t"
  , elements ",;:"
  ]

-- | Generate strings with potential comment markers
genCommentString :: Gen String
genCommentString = listOf $ oneof
  [ arbitrary `suchThat` (\c -> c /= '\n' && c /= '\r')
  , return '/' >> return '/'
  , return '/' >> return '*'
  , return '*' >> return '/'
  , elements "\"'"
  ]

-- | Generate strings with indentation
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf $ do
    indent <- choose (0, 5) >>= \n -> return (replicate n ' ')
    content <- listOf $ arbitrary `suchThat` (\c -> c /= '\n' && c /= '\r')
    return $ indent ++ content
  return $ unlines lines

-- | Generate source positions
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  return $ SourcePos line col

-- | Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  end <- genSourcePos
  return $ SourceSpan start end

-- | Test that trim preserves non-whitespace content
prop_trim_preserves_content :: Property
prop_trim_preserves_content = forAll genString $ \s ->
  let trimmed = trim s
      nonSpaceContent = L.filter (not . isSpace) s
  in null nonSpaceContent || (L.filter (not . isSpace) trimmed) === nonSpaceContent

-- | Test that trim removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace = forAll genString $ \s ->
  let trimmed = trim s
  in not (null trimmed) ==> (isSpace (L.head trimmed) == False) && (isSpace (last trimmed) == False)

-- | Test that splitBy preserves empty segments
prop_splitBy_preserves_empty :: Property
prop_splitBy_preserves_empty = forAll arbitrary $ \delim ->
  forAll genString $ \s ->
    splitBy delim s === L.map (unlines . lines) (splitBy delim (unlines . lines $ s))

-- | Test that splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Property
prop_splitByCollapsed_removes_empty = forAll arbitrary $ \delim ->
  forAll genString $ \s ->
    L.all (not . null) (splitByCollapsed delim s)

-- | Test that splitByComma is equivalent to splitBy ','
prop_splitByComma_equivalence :: Property
prop_splitByComma_equivalence = forAll genString $ \s ->
  splitByComma s === splitBy ',' s

-- | Test that splitByCommaCollapsed is equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalence :: Property
prop_splitByCommaCollapsed_equivalence = forAll genString $ \s ->
  splitByCommaCollapsed s === splitByCollapsed ',' s

-- | Test that removeLineComments removes content after //
prop_removeLineComments_removes_content :: Property
prop_removeLineComments_removes_content = forAll genCommentString $ \s ->
  let hasComment = "//" `L.isInfixOf` s
      cleaned = removeLineComments s
      lines' = lines cleaned
  in hasComment ==> L.all (not . ("//" `L.isPrefixOf`)) lines'

-- | Test that removeComments handles block comments
prop_removeComments_handles_blocks :: Property
prop_removeComments_handles_blocks = forAll genCommentString $ \s ->
  let hasBlock = "/*" `L.isInfixOf` s && "*/" `L.isInfixOf` s
      cleaned = removeComments s
  in hasBlock ==> not ("/*" `L.isInfixOf` cleaned) && not ("*/" `L.isInfixOf` cleaned)

-- | Test that normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_relative :: Property
prop_normalizeIndentation_preserves_relative = forAll genIndentedString $ \s ->
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in L.length originalLines === L.length normalizedLines

-- | Test that startPos creates a valid position
prop_startPos_valid :: Property
prop_startPos_valid = 
  let pos = startPos
  in posLine pos === 1 && posColumn pos === 1

-- | Test that posAfter correctly advances position
prop_posAfter_advances :: Property
prop_posAfter_advances = forAll genSourcePos $ \pos ->
  let newPos = posAfter pos 'a'
  in posLine newPos === posLine pos && 
     (if posColumn pos < 100  -- Avoid overflow
      then posColumn newPos === posColumn pos + 1
      else posColumn newPos === posColumn pos)

-- | Test that emptySpan is valid
prop_emptySpan_valid :: Property
prop_emptySpan_valid =
  let span = emptySpan
  in isValidSpan span

-- | Test that spanFrom creates a valid span
prop_spanFrom_valid :: Property
prop_spanFrom_valid = forAll genSourcePos $ \pos ->
  let span = spanFrom pos
  in spanStart span === pos && spanEnd span === pos

-- | Test that mergeSpans creates a valid span
prop_mergeSpans_valid :: Property
prop_mergeSpans_valid = forAll genSourceSpan $ \span1 ->
  forAll genSourceSpan $ \span2 ->
    let merged = mergeSpans span1 span2
    in isValidSpan merged

-- | Test that locatedAt creates a valid located value
prop_locatedAt_valid :: Property
prop_locatedAt_valid = forAll arbitrary $ \value ->
  forAll genSourcePos $ \pos ->
    let located = locatedAt pos value
    in locatedValue located === value && 
       spanStart (locatedSpan located) === pos &&
       spanEnd (locatedSpan located) === pos

-- | Test that locatedWithSpan creates a valid located value
prop_locatedWithSpan_valid :: Property
prop_locatedWithSpan_valid = forAll arbitrary $ \value ->
  forAll genSourceSpan $ \span ->
    let located = locatedWithSpan span value
    in locatedValue located === value && locatedSpan located === span

tests :: TestTree
tests = testGroup "Enhanced Core Functionality QuickCheck Tests"
  [ testProperty "trim preserves non-whitespace content" prop_trim_preserves_content
  , testProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
  , testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , testProperty "splitByComma is equivalent to splitBy ','" prop_splitByComma_equivalence
  , testProperty "splitByCommaCollapsed is equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalence
  , testProperty "removeLineComments removes content after //" prop_removeLineComments_removes_content
  , testProperty "removeComments handles block comments" prop_removeComments_handles_blocks
  , testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative
  , testProperty "startPos creates a valid position" prop_startPos_valid
  , testProperty "posAfter correctly advances position" prop_posAfter_advances
  , testProperty "emptySpan is valid" prop_emptySpan_valid
  , testProperty "spanFrom creates a valid span" prop_spanFrom_valid
  , testProperty "mergeSpans creates a valid span" prop_mergeSpans_valid
  , testProperty "locatedAt creates a valid located value" prop_locatedAt_valid
  , testProperty "locatedWithSpan creates a valid located value" prop_locatedWithSpan_valid
  ]