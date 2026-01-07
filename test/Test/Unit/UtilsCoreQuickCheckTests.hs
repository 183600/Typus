module Test.Unit.UtilsCoreQuickCheckTests where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), testProperties, Property, (===), forAll, Gen, Arbitrary(..), oneof, elements, listOf, listOf1, resize, suchThat, property)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Utils 
prop_trim_idempotent                               s = trim (trim s) == trim s
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


-- | trim: result should not start L.or end with whitespace
prop_trim_no_whitespace :: String -> Bool
prop_trim_no_whitespace                               s = 
    let trimmed = trim s
    in null trimmed || 
       (not (isSpace (L.head trimmed) && not (isSpace (last trimmed))

-- | splitBy: splitting L.and then joining with the same delimiter should reconstruct original
prop_splitBy_join :: Char -> String -> Bool
prop_splitBy_join delim                               s = concatMap (\x -> x ++ [delim]) (splitBy delim s) `L.isPrefixOf` s

-- | splitBy vs splitByCollapsed: splitByCollapsed should have no empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Bool
prop_splitByCollapsed_no_empty delim                               s = 
    let collapsed = splitByCollapsed delim s
    in L.all (not . null) collapsed

-- | splitByComma: should be equivalent to splitBy ','
prop_splitByComma_equivalent :: String -> Bool
prop_splitByComma_equivalent                               s = splitByComma                               s == splitBy ',' s

-- | splitByCommaCollapsed: should be equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalent :: String -> Bool
prop_splitByCommaCollapsed_equivalent                               s = splitByCommaCollapsed                               s == splitByCollapsed ',' s

-- | removeLineComments: removing comments twice should be idempotent
prop_removeLineComments_idempotent :: String -> Bool
prop_removeLineComments_idempotent                               s = removeLineComments (removeLineComments s) == removeLineComments s

-- | removeLineComments: result should not contain line comment patterns
prop_removeLineComments_no_comments :: String -> Bool
prop_removeLineComments_no_comments                               s = 
    let cleaned = removeLineComments s
    in not ("//" `L.isPrefixOf` cleaned)

-- | removeComments: removing comments twice should be idempotent
prop_removeComments_idempotent :: String -> Bool
prop_removeComments_idempotent                               s = removeComments (removeComments s) == removeComments s

-- | normalizeIndentation: normalizing twice should be idempotent
prop_normalizeIndentation_idempotent :: String -> Bool
prop_normalizeIndentation_idempotent                               s = normalizeIndentation (normalizeIndentation s) == normalizeIndentation s

-- | breakOn: should find first occurrence L.or return original
prop_breakOn_correctness :: String -> String -> Bool
prop_breakOn_correctness needle                               haystack = 
    let (before, after) = breakOn needle haystack
    in if needle `L.isInfixOf` haystack
       then needle `L.isInfixOf` after && not (needle `L.isInfixOf` before)
       else                               before == haystack && null after
  where
      isInfixOf needle                               haystack = needle `Data.List.L.isInfixOf` haystack

-- | splitBy: empty string should return list with one empty string
prop_splitBy_empty :: Char -> Bool
prop_splitBy_empty                               delim = splitBy delim "" == [""]

-- | splitByCollapsed: empty string should return empty list
prop_splitByCollapsed_empty :: Char -> Bool
prop_splitByCollapsed_empty                               delim = splitByCollapsed delim "" == []

-- | trim: trimming empty string should return empty string
prop_trim_empty :: Bool
                              prop_trim_empty = trim "" == ""

-- | splitBy: consecutive delimiters should produce empty strings
prop_splitBy_consecutive :: Char -> Int -> Bool
prop_splitBy_consecutive delim                               n = 
    let input = replicate delim n
                                      result = splitBy delim input
    in L.length                               result == n + 1 && L.all null result
  where
      replicate d                               0 = ""
    replicate d                               k = d : replicate d (k-1)

-- | splitByCollapsed: consecutive delimiters should be collapsed
prop_splitByCollapsed_consecutive :: Char -> Int -> Bool
prop_splitByCollapsed_consecutive delim                               n = 
    let input = replicate delim n
                                      result = splitByCollapsed delim input
    in null result
  where
      replicate d                               0 = ""
    replicate d                               k = d : replicate d (k-1)

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "Utils Core QuickCheck Tests"
  [ testProperties "String Processing Properties"
    [ ("trim idempotent", prop_trim_idempotent)
    , ("trim no whitespace", prop_trim_no_whitespace)
    , ("trim empty", prop_trim_empty)
    ]

  , testProperties "Splitting Properties"
    [ ("splitBy join", prop_splitBy_join)
    , ("splitByCollapsed no empty", prop_splitByCollapsed_no_empty)
    , ("splitByComma equivalent", prop_splitByComma_equivalent)
    , ("splitByCommaCollapsed equivalent", prop_splitByCommaCollapsed_equivalent)
    , ("splitBy empty", prop_splitBy_empty)
    , ("splitByCollapsed empty", prop_splitByCollapsed_empty)
    , ("splitBy consecutive", prop_splitBy_consecutive)
    , ("splitByCollapsed consecutive", prop_splitByCollapsed_consecutive)
    ]

  , testProperties "Comment Processing Properties"
    [ ("removeLineComments idempotent", prop_removeLineComments_idempotent)
    , ("removeLineComments no comments", prop_removeLineComments_no_comments)
    , ("removeComments idempotent", prop_removeComments_idempotent)
    ]

  , testProperties "Indentation Properties"
    [ ("normalizeIndentation idempotent", prop_normalizeIndentation_idempotent)
    ]

  , testProperties "Search Properties"
    [ ("breakOn correctness", prop_breakOn_correctness)
    ]
  ]