module Test.Unit.NewCabalUtilsQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, (===), )
              forceSingleTabIndentation, fixIndentation, breakOn)
import Data.Char 
              content <- listOf $ arbitrary `suchThat` (/= '\n')
  leading <- listOf $ elements " \t"
  trailing <- listOf $ elements " \t"
  return $ leading ++ content ++ trailing
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Generate strings with commas
genCommaString :: Gen String
                              genCommaString = listOf $ elements ['a'..'z'] ++ [',', ' ', '\t']

-- Generate strings with comments
genCommentString :: Gen String
                              genCommentString = do
              lines <- listOf1 $ do
              content <- listOf $ arbitrary `suchThat` (`notElem` "/\"'*\n")
    hasComment <- arbitrary
    if hasComment
      then do
              commentType <- elements ["//", "/*"]
        comment <- listOf $ arbitrary `suchThat` (`notElem` "\n")
        if                               commentType == "/*"
          then do
              hasEnd <- arbitrary
            if hasEnd
              then return $ content ++ "/*" ++ comment ++ "*/"
              else return $ content ++ "/*" ++ comment
          else return $ content ++ "//" ++ comment
      else return content
  return $ unlines lines

-- Generate indented strings
genIndentedString :: Gen String
                              genIndentedString = do
              lines <- listOf1 $ do
              indent <- listOf $ elements " \t"
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ indent ++ content
  return $ unlines lines

-- ============================================================================
-- Utils QuickCheck Tests
-- ============================================================================

-- Test trim function properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent                               s = trim (trim s) === trim s

prop_trim_no_leading_trailing_whitespace :: String -> Property
prop_trim_no_leading_trailing_whitespace                               s = 
  let trimmed = trim s
in not (null trimmed) ==> 
     (not (isSpace (L.head trimmed) .&&. not (isSpace (last trimmed))

prop_trim_preserves_internal_content :: String -> Property
prop_trim_preserves_internal_content                               s =
  let trimmed = trim s
                              original = L.filter (not . isSpace) s
                                    filtered = L.filter (not . isSpace) trimmed
  in                               original === filtered

-- Test splitBy function properties
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim                               s = 
  let parts = splitBy delim s
                                    delimiterCount = L.length (L.filter (== delim) s)
  in L.length                               parts === delimiterCount + 1

prop_splitBy_reconstruction :: Char -> String -> Property
prop_splitBy_reconstruction delim                               s =
  let parts = splitBy delim s
                              reconstructed = L.concat (intersperse [delim] parts)
  in                               reconstructed === s

prop_splitByCollapsed_no_empty_parts :: Char -> String -> Property
prop_splitByCollapsed_no_empty_parts delim                               s =
  let parts = splitByCollapsed delim s
in L.all (not . null) parts

-- Test splitByComma L.and splitByCommaCollapsed
prop_splitByComma_equals_splitBy :: String -> Property
prop_splitByComma_equals_splitBy                               s = splitByComma                               s === splitBy ',' s

prop_splitByCommaCollapsed_equals_splitByCollapsed :: String -> Property
prop_splitByCommaCollapsed                               s = splitByCommaCollapsed                               s === splitByCollapsed ',' s

-- Test removeLineComments function properties
prop_removeLineComments_no_comment_markers :: String -> Property
prop_removeLineComments_no_comment_markers                               s =
  not ("//" `L.isInfixOf` s) ==> removeLineComments                               s === s

prop_removeLineComments_preserves_line_structure :: String -> Property
prop_removeLineComments_preserves_line_structure                               s =
  let original = lines s
                              cleaned = lines (removeLineComments s)
  in L.length                               original === L.length cleaned

-- Test removeComments function properties
prop_removeComments_no_comment_markers :: String -> Property
prop_removeComments_no_comment_markers                               s =
  not (("//" `L.isInfixOf` s) || ("/*" `L.isInfixOf` s) ==> removeComments                               s === s

prop_removeLineComments_is_subset_of_removeComments :: String -> Property
prop_removeLineComments_is_subset_of_removeComments                               s =
  let lineRemoved = removeLineComments s
                                    allRemoved = removeComments s
  in L.length lineRemoved >= L.length allRemoved

-- Test normalizeIndentation function properties
prop_normalizeIndentation_preserves_relative_indentation :: String -> Property
prop_normalizeIndentation_preserves_relative_indentation                               s =
  let normalized = normalizeIndentation s
                              originalLines = L.filter (not . L.all isSpace) (lines s)
                                    normalizedLines = L.filter (not . L.all isSpace) (lines normalized)
  in L.length                               originalLines === L.length normalizedLines

prop_normalizeIndentation_removes_common_prefix :: String -> Property
prop_normalizeIndentation_removes_common_prefix                               s =
  let normalized = normalizeIndentation s
                                    normalizedLines = lines normalized
      in L.all (not . isPrefixOf "    ") normalizedLines

-- Test forceSingleTabIndentation function properties
prop_forceSingleTabIndentation_adds_tab_to_nonempty :: String -> Property
prop_forceSingleTabIndentation_adds_tab_to_nonempty                               s =
  let forced = forceSingleTabIndentation s
      lines' = lines forced
                                    nonEmptyLines = L.filter (not . null) lines'
  in L.all ("\t" `L.isPrefixOf`) nonEmptyLines

-- Test fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalizeIndentation :: String -> Property
prop_fixIndentation_equals_normalizeIndentation                               s = 
  fixIndentation                               s === normalizeIndentation s

-- Test breakOn function properties
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern                               s = breakOn ""                               s === ("", s)

prop_breakOn_reconstruction :: String -> String -> Property
prop_breakOn_reconstruction pat                               s =
  let (before, after) = breakOn pat s
  in property $ before ++ pat ++                               after === s || (null after && not (pat `L.isInfixOf` s)

prop_breakOn_not_found :: String -> String -> Property
prop_breakOn_not_found pat                               s =
  not (pat `L.isInfixOf` s) ==> breakOn pat                               s === (s, "")

-- Helper function
intersperse :: a -> [a] -> [[a]]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = [x] ++ [sep] ++ intersperse sep xs

tests :: TestTree
tests =   testGroup "New Cabal Utils QuickCheck Tests"
  [ testGroup "trim function tests"
      [             testProperty "trim is idempotent" prop_trim_idempotent
      ,             testProperty "trim removes leading/trailing whitespace" prop_trim_no_leading_trailing_whitespace
      ,             testProperty "trim preserves internal content" prop_trim_preserves_internal_content
      ]
  , testGroup "split function tests"
      [             testProperty "splitBy creates correct number of parts" prop_splitBy_length
      ,             testProperty "splitBy reconstruction property" prop_splitBy_reconstruction
      ,             testProperty "splitByCollapsed has no empty parts" prop_splitByCollapsed_no_empty_parts
      ,             testProperty "splitByComma equals splitBy with comma" prop_splitByComma_equals_splitBy
      ,             testProperty "splitByCommaCollapsed equals splitByCollapsed with comma" prop_splitByCommaCollapsed_equals_splitByCollapsed
      ]
  , testGroup "comment removal tests"
      [             testProperty "removeLineComments preserves strings without comments" prop_removeLineComments_no_comment_markers
      ,             testProperty "removeLineComments preserves line structure" prop_removeLineComments_preserves_line_structure
      ,             testProperty "removeComments preserves strings without comment markers" prop_removeComments_no_comment_markers
      ,             testProperty "removeLineComments is subset of removeComments" prop_removeLineComments_is_subset_of_removeComments
      ]
  , testGroup "indentation tests"
      [             testProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_preserves_relative_indentation
      ,             testProperty "normalizeIndentation removes common prefix" prop_normalizeIndentation_removes_common_prefix
      ,             testProperty "forceSingleTabIndentation adds tab to non-empty lines" prop_forceSingleTabIndentation_adds_tab_to_nonempty
      ,             testProperty "fixIndentation equals normalizeIndentation" prop_fixIndentation_equals_normalizeIndentation
      ]
  , testGroup "breakOn function tests"
      [             testProperty "breakOn with empty pattern" prop_breakOn_empty_pattern
      ,             testProperty "breakOn reconstruction property" prop_breakOn_reconstruction
      ,             testProperty "breakOn when pattern not found" prop_breakOn_not_found
      ]
  ]