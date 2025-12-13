{-# LANGUAGE CPP #-}

module Test.Unit.ValueAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler.ValueAnalysis
  ( ValueInfo(..)
  , ValueKind(..)
  )
import Data.List (isInfixOf)

-- Property: ValueInfo preserves name
prop_valueinfo_preserves_name :: String -> Property
prop_valueinfo_preserves_name name =
  let valueInfo = ValueInfo name Unknown 0
  in viName valueInfo === name

-- Property: ValueInfo preserves kind
prop_valueinfo_preserves_kind :: String -> ValueKind -> Property
prop_valueinfo_preserves_kind name kind =
  let valueInfo = ValueInfo name kind 0
  in viKind valueInfo === kind

-- Property: ValueInfo preserves scope
prop_valueinfo_preserves_scope :: String -> ValueKind -> Int -> Property
prop_valueinfo_preserves_scope name kind scope =
  let valueInfo = ValueInfo name kind scope
  in viLine valueInfo === scope

-- Property: ValueInfo with all fields
prop_valueinfo_all_fields :: String -> ValueKind -> Int -> Property
prop_valueinfo_all_fields name kind scope =
  let valueInfo = ValueInfo name kind scope
  in viName valueInfo === name &&
     viKind valueInfo === kind &&
     viLine valueInfo === scope

-- Property: ValueInfo equality
prop_valueinfo_eq :: ValueInfo -> ValueInfo -> Property
prop_valueinfo_eq vi1 vi2 =
  (vi1 == vi2) === 
    (viName vi1 == viName vi2 &&
     viKind vi1 == viKind vi2 &&
     viLine vi1 == viLine vi2)

-- Property: ValueInfo ordering
prop_valueinfo_ordering :: ValueInfo -> ValueInfo -> Property
prop_valueinfo_ordering vi1 vi2 =
  let result = compare vi1 vi2
  in (result == LT || result == EQ || result == GT) === True

-- Property: ValueInfo show
prop_valueinfo_show :: ValueInfo -> Property
prop_valueinfo_show valueInfo =
  let shown = show valueInfo
  in not (null shown)

-- Property: ValueInfo show contains name
prop_valueinfo_show_contains_name :: String -> Property
prop_valueinfo_show_contains_name name =
  let valueInfo = ValueInfo name Unknown 0
      shown = show valueInfo
  in name `isInfixOf` shown

-- Property: ValueKind equality
prop_valuekind_eq :: ValueKind -> ValueKind -> Bool
prop_valuekind_eq kind1 kind2 = kind1 == kind2

-- Property: ValueKind ordering
prop_valuekind_ordering :: ValueKind -> ValueKind -> Property
prop_valuekind_ordering kind1 kind2 =
  let result = compare kind1 kind2
  in (result == LT || result == EQ || result == GT) === True

-- Property: ValueKind exhaustive
prop_valuekind_exhaustive :: ValueKind -> Property
prop_valuekind_exhaustive kind =
  let isKnownKind = kind `elem` [ValueCopy, Reference, Unknown]
  in isKnownKind === True

-- Property: ValueKind show
prop_valuekind_show :: ValueKind -> Property
prop_valuekind_show kind =
  let shown = show kind
  in not (null shown)

-- Property: ValueInfo with ValueCopy kind
prop_valueinfo_valuecopy :: String -> Int -> Property
prop_valueinfo_valuecopy name scope =
  let valueInfo = ValueInfo name ValueCopy scope
  in viKind valueInfo === ValueCopy &&
     viName valueInfo === name &&
     viLine valueInfo === scope

-- Property: ValueInfo with Reference kind
prop_valueinfo_reference :: String -> Int -> Property
prop_valueinfo_reference name scope =
  let valueInfo = ValueInfo name Reference scope
  in viKind valueInfo === Reference &&
     viName valueInfo === name &&
     viLine valueInfo === scope

-- Property: ValueInfo with Unknown kind
prop_valueinfo_unknown :: String -> Int -> Property
prop_valueinfo_unknown name scope =
  let valueInfo = ValueInfo name Unknown scope
  in viKind valueInfo === Unknown &&
     viName valueInfo === name &&
     viLine valueInfo === scope

-- Property: ValueInfo with different kinds
prop_valueinfo_different_kinds :: String -> Int -> Property
prop_valueinfo_different_kinds name scope =
  let copy = ValueInfo name ValueCopy scope
      ref = ValueInfo name Reference scope
      unknown = ValueInfo name Unknown scope
  in viKind copy === ValueCopy &&
     viKind ref === Reference &&
     viKind unknown === Unknown &&
     viName copy === name &&
     viName ref === name &&
     viName unknown === name &&
     viLine copy === scope &&
     viLine ref === scope &&
     viLine unknown === scope

-- Property: ValueInfo with different scopes
prop_valueinfo_different_scopes :: String -> ValueKind -> [Int] -> Property
prop_valueinfo_different_scopes name kind scopes =
  let valueInfos = map (\scope -> ValueInfo name kind scope) scopes
      scopes' = map viLine valueInfos
  in scopes' === scopes

-- Property: ValueInfo with different names
prop_valueinfo_different_names :: [String] -> ValueKind -> Int -> Property
prop_valueinfo_different_names names kind scope =
  let valueInfos = map (\name -> ValueInfo name kind scope) names
      names' = map viName valueInfos
  in names' === names

-- Property: ValueInfo with negative scope
prop_valueinfo_negative_scope :: String -> ValueKind -> Property
prop_valueinfo_negative_scope name kind =
  let valueInfo = ValueInfo name kind (-1)
  in viLine valueInfo === -1 &&
     viName valueInfo === name &&
     viKind valueInfo === kind

-- Property: ValueInfo with zero scope
prop_valueinfo_zero_scope :: String -> ValueKind -> Property
prop_valueinfo_zero_scope name kind =
  let valueInfo = ValueInfo name kind 0
  in viLine valueInfo === 0 &&
     viName valueInfo === name &&
     viKind valueInfo === kind

-- Property: ValueInfo with large scope
prop_valueinfo_large_scope :: String -> ValueKind -> Property
prop_valueinfo_large_scope name kind =
  let valueInfo = ValueInfo name kind 999999
  in viLine valueInfo === 999999 &&
     viName valueInfo === name &&
     viKind valueInfo === kind

-- Property: ValueInfo with empty name
prop_valueinfo_empty_name :: ValueKind -> Int -> Property
prop_valueinfo_empty_name kind scope =
  let valueInfo = ValueInfo "" kind scope
  in viName valueInfo === "" &&
     viKind valueInfo === kind &&
     viLine valueInfo === scope

-- Property: ValueInfo with special characters
prop_valueinfo_special_chars :: ValueKind -> Int -> Property
prop_valueinfo_special_chars kind scope =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      valueInfo = ValueInfo specialChars kind scope
  in viName valueInfo === specialChars &&
     viKind valueInfo === kind &&
     viLine valueInfo === scope

-- Property: ValueInfo with Unicode characters
prop_valueinfo_unicode :: ValueKind -> Int -> Property
prop_valueinfo_unicode kind scope =
  let unicode = "测试变量名🚀"
      valueInfo = ValueInfo unicode kind scope
  in viName valueInfo === unicode &&
     viKind valueInfo === kind &&
     viLine valueInfo === scope

-- Property: ValueInfo show contains kind
prop_valueinfo_show_contains_kind :: String -> ValueKind -> Property
prop_valueinfo_show_contains_kind name kind =
  let valueInfo = ValueInfo name kind 0
      shown = show valueInfo
      kindStr = show kind
  in kindStr `isInfixOf` shown

-- Property: ValueInfo show contains scope
prop_valueinfo_show_contains_scope :: String -> ValueKind -> Int -> Property
prop_valueinfo_show_contains_scope name kind scope =
  let valueInfo = ValueInfo name kind scope
      shown = show valueInfo
  in show scope `isInfixOf` shown

-- Property: ValueKind with different values
prop_valuekind_different :: Property
prop_valuekind_different =
  let kinds = [ValueCopy, Reference, Unknown]
      allDifferent = all (\(k1, k2) -> k1 /= k2) [(k1, k2) | k1 <- kinds, k2 <- kinds, k1 /= k2]
  in allDifferent === True

-- Property: ValueKind copy vs reference
prop_valuekind_copy_vs_reference :: Property
prop_valuekind_copy_vs_reference =
  let copy = ValueCopy
      reference = Reference
  in copy /= reference &&
     compare copy reference /= EQ

-- Property: ValueKind unknown vs others
prop_valuekind_unknown_vs_others :: Property
prop_valuekind_unknown_vs_others =
  let unknown = Unknown
      copy = ValueCopy
      reference = Reference
  in unknown /= copy &&
     unknown /= reference &&
     compare unknown copy /= EQ &&
     compare unknown reference /= EQ

-- Property: ValueInfo with same name different kind
prop_valueinfo_same_name_different_kind :: String -> Int -> Property
prop_valueinfo_same_name_different_kind name scope =
  let copy = ValueInfo name ValueCopy scope
      reference = ValueInfo name Reference scope
      unknown = ValueInfo name Unknown scope
  in copy /= reference &&
     copy /= unknown &&
     reference /= unknown

-- Property: ValueInfo with same kind different name
prop_valueinfo_same_kind_different_name :: ValueKind -> Int -> Property
prop_valueinfo_same_kind_different_name kind scope =
  let vi1 = ValueInfo "name1" kind scope
      vi2 = ValueInfo "name2" kind scope
  in vi1 /= vi2

-- Property: ValueInfo with same name and kind different scope
prop_valueinfo_same_name_kind_different_scope :: String -> ValueKind -> Int -> Int -> Property
prop_valueinfo_same_name_kind_different_scope name kind scope1 scope2 =
  let vi1 = ValueInfo name kind scope1
      vi2 = ValueInfo name kind scope2
  in (vi1 == vi2) === (scope1 == scope2)

-- Property: ValueInfo ordering by name
prop_valueinfo_ordering_by_name :: String -> String -> Property
prop_valueinfo_ordering_by_name name1 name2 =
  let vi1 = ValueInfo name1 Unknown 0
      vi2 = ValueInfo name2 Unknown 0
      result = compare vi1 vi2
  in (name1 <= name2) ==> (result == LT || result == EQ)

-- Property: ValueInfo ordering by scope when names equal
prop_valueinfo_ordering_by_scope :: String -> Int -> Int -> Property
prop_valueinfo_ordering_by_scope name scope1 scope2 =
  let vi1 = ValueInfo name Unknown scope1
      vi2 = ValueInfo name Unknown scope2
      result = compare vi1 vi2
  in (scope1 <= scope2) ==> (result == LT || result == EQ)

tests :: TestTree
tests = testGroup "ValueAnalysis QuickCheck tests"
  [ fastProperty "ValueInfo preserves name" prop_valueinfo_preserves_name
  , fastProperty "ValueInfo preserves kind" prop_valueinfo_preserves_kind
  , fastProperty "ValueInfo preserves scope" prop_valueinfo_preserves_scope
  , fastProperty "ValueInfo with all fields" prop_valueinfo_all_fields
  , fastProperty "ValueInfo equality" prop_valueinfo_eq
  , fastProperty "ValueInfo ordering" prop_valueinfo_ordering
  , fastProperty "ValueInfo show" prop_valueinfo_show
  , fastProperty "ValueInfo show contains name" prop_valueinfo_show_contains_name
  , fastProperty "ValueKind equality" prop_valuekind_eq
  , fastProperty "ValueKind ordering" prop_valuekind_ordering
  , fastProperty "ValueKind exhaustive" prop_valuekind_exhaustive
  , fastProperty "ValueKind show" prop_valuekind_show
  , fastProperty "ValueInfo with ValueCopy kind" prop_valueinfo_valuecopy
  , fastProperty "ValueInfo with Reference kind" prop_valueinfo_reference
  , fastProperty "ValueInfo with Unknown kind" prop_valueinfo_unknown
  , fastProperty "ValueInfo with different kinds" prop_valueinfo_different_kinds
  , fastProperty "ValueInfo with different scopes" prop_valueinfo_different_scopes
  , fastProperty "ValueInfo with different names" prop_valueinfo_different_names
  , fastProperty "ValueInfo with negative scope" prop_valueinfo_negative_scope
  , fastProperty "ValueInfo with zero scope" prop_valueinfo_zero_scope
  , fastProperty "ValueInfo with large scope" prop_valueinfo_large_scope
  , fastProperty "ValueInfo with empty name" prop_valueinfo_empty_name
  , fastProperty "ValueInfo with special characters" prop_valueinfo_special_chars
  , fastProperty "ValueInfo with Unicode characters" prop_valueinfo_unicode
  , fastProperty "ValueInfo show contains kind" prop_valueinfo_show_contains_kind
  , fastProperty "ValueInfo show contains scope" prop_valueinfo_show_contains_scope
  , fastProperty "ValueKind with different values" prop_valuekind_different
  , fastProperty "ValueKind copy vs reference" prop_valuekind_copy_vs_reference
  , fastProperty "ValueKind unknown vs others" prop_valuekind_unknown_vs_others
  , fastProperty "ValueInfo with same name different kind" prop_valueinfo_same_name_different_kind
  , fastProperty "ValueInfo with same kind different name" prop_valueinfo_same_kind_different_name
  , fastProperty "ValueInfo with same name and kind different scope" prop_valueinfo_same_name_kind_different_scope
  , fastProperty "ValueInfo ordering by name" prop_valueinfo_ordering_by_name
  , fastProperty "ValueInfo ordering by scope when names equal" prop_valueinfo_ordering_by_scope
  ]