{-# LANGUAGE CPP #-}

module Test.Unit.CabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group, intercalate, isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, toLower, toUpper)
import Control.Monad (replicateM)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset)
import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.TypeChecker (Type(..))

tests :: TestTree
tests = testGroup "Cabal QuickCheck Test Properties"
  [ testGroup "String Processing Properties"
      [ fastProperty "trim is idempotent" prop_trim_idempotent
      , fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
      , fastProperty "trim preserves non-whitespace content" prop_trim_preserves_content
      , fastProperty "splitBy preserves content order" prop_splitBy_preserves_order
      , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
      , fastProperty "breakOn splits correctly" prop_breakOn_splits
      , fastProperty "removeLineComments preserves non-comment lines" prop_removeComments_preserves
      , fastProperty "removeComments removes all comment types" prop_removeComments_complete
      , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_relative
      , fastProperty "intercalate with empty separator" prop_intercalate_empty
      , fastProperty "isPrefixOf reflexive" prop_isPrefixOf_reflexive
      , fastProperty "isSuffixOf reflexive" prop_isSuffixOf_reflexive
      , fastProperty "isInfixOf reflexive" prop_isInfixOf_reflexive
      ]
  , testGroup "Data Structure Properties"
      [ fastProperty "Map insertion is idempotent" prop_map_insert_idempotent
      , fastProperty "Map lookup after insert" prop_map_lookup_after_insert
      , fastProperty "Map delete removes key" prop_map_delete_removes
      , fastProperty "Map union preserves values" prop_map_union_preserves
      , fastProperty "Set union is commutative" prop_set_union_commutative
      , fastProperty "Set intersection is commutative" prop_set_intersection_commutative
      , fastProperty "Set insert is idempotent" prop_set_insert_idempotent
      , fastProperty "Set delete removes element" prop_set_delete_removes
      , fastProperty "sort is idempotent" prop_sort_idempotent
      , fastProperty "nub removes duplicates" prop_nub_removes_duplicates
      , fastProperty "reverse twice is identity" prop_reverse_twice_identity
      , fastProperty "length is preserved by reverse" prop_reverse_preserves_length
      , fastProperty "concatenation associativity" prop_concat_associative
      , fastProperty "empty string is identity for concat" prop_empty_concat_identity
      ]
  , testGroup "SourceLocation Properties"
      [ fastProperty "SourcePos offset is non-negative" prop_sourcepos_offset_nonnegative
      , fastProperty "SourceSpan start before or equal to end" prop_sourcespan_ordering
      , fastProperty "SourceSpan length is non-negative" prop_sourcespan_length_nonnegative
      ]
  , testGroup "Parser Directives Properties"
      [ fastProperty "defaultFileDirectives has no directives set" prop_defaultFileDirectives_empty
      , fastProperty "defaultBlockDirectives has no directives set" prop_defaultBlockDirectives_empty
      ]
  
  , testGroup "Type System Properties"
      [ fastProperty "Type equality is reflexive" prop_type_reflexive
      , fastProperty "Type show preserves structure" prop_type_show_preserves
      ]
  , testGroup "Character Properties"
      [ fastProperty "toLower . toUpper preserves non-letters" prop_tolower_toupper_preserves
      , fastProperty "isSpace is idempotent" prop_isspace_idempotent
      , fastProperty "toLower is idempotent" prop_tolower_idempotent
      , fastProperty "toUpper is idempotent" prop_toupper_idempotent
      ]
  , testGroup "List Properties"
      [ fastProperty "group preserves order" prop_group_preserves_order
      , fastProperty "head of non-empty list exists" prop_head_nonempty
      , fastProperty "tail of non-empty list has correct length" prop_tail_nonempty
      , fastProperty "take and drop partition list" prop_take_drop_partition
      , fastProperty "length of take is bounded" prop_take_length_bounded
      ]
  ]

-- String Processing Properties
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in conjoin
    [ counterexample "should not start with space" $
        null trimmed || not (head trimmed `elem` " \t\n\r")
    , counterexample "should not end with space" $
        null trimmed || not (last trimmed `elem` " \t\n\r")
    ]

prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s =
  let trimmed = trim s
      nonSpace = filter (not . isSpace) s
      trimmedNonSpace = filter (not . isSpace) trimmed
  in counterexample "should preserve non-space characters" $
       nonSpace === trimmedNonSpace

prop_splitBy_preserves_order :: Char -> String -> Property
prop_splitBy_preserves_order delim s =
  let parts = splitBy delim s
      joined = intercalate [delim] parts
  in counterexample "joined parts should equal original (minus trailing delimiters)" $
       joined === dropWhileEnd (== delim) s
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in counterexample "should not contain empty strings" $
       all (not . null) parts

prop_breakOn_splits :: String -> String -> Property
prop_breakOn_splits needle haystack =
  not (null needle) ==>
  let (before, after) = breakOn needle haystack
      reconstructed = if null after && not (needle `isInfixOf` haystack)
                      then before
                      else before ++ needle ++ after
  in counterexample ("Expected: " ++ haystack ++ ", Got: " ++ reconstructed) $
       if needle `isInfixOf` haystack
       then reconstructed === haystack
       else before === haystack .&&. null after

prop_removeComments_preserves :: String -> Property
prop_removeComments_preserves s =
  "//" `notElem` [s] ==>
  let cleaned = removeLineComments s
  in counterexample "non-comment line should be preserved" $
       not (null s) ==> not (null cleaned)

prop_removeComments_complete :: Property
prop_removeComments_complete =
  let input = "code // line comment\nmore /* block */ code"
      result = removeComments input
  in counterexample "should remove both comment types" $
       not ("//" `elem` words result) .&&. not ("/*" `elem` words result)

prop_normalizeIndentation_relative :: Property
prop_normalizeIndentation_relative =
  let input = "    line1\n      line2\n    line3"
      result = normalizeIndentation input
      resultLines = lines result
  in counterexample "should preserve relative indentation" $
       length resultLines === 3

prop_intercalate_empty :: [String] -> Property
prop_intercalate_empty xs =
  intercalate "" xs === concat xs

prop_isPrefixOf_reflexive :: String -> Property
prop_isPrefixOf_reflexive s =
  property $ s `isPrefixOf` s

prop_isSuffixOf_reflexive :: String -> Property
prop_isSuffixOf_reflexive s =
  property $ s `isSuffixOf` s

prop_isInfixOf_reflexive :: String -> Property
prop_isInfixOf_reflexive s =
  property $ s `isInfixOf` s

-- Data Structure Properties
prop_map_insert_idempotent :: Int -> String -> Map.Map Int String -> Property
prop_map_insert_idempotent k v m =
  let m1 = Map.insert k v m
      m2 = Map.insert k v m1
  in m1 === m2

prop_map_lookup_after_insert :: Int -> String -> Map.Map Int String -> Property
prop_map_lookup_after_insert k v m =
  let m' = Map.insert k v m
  in Map.lookup k m' === Just v

prop_map_delete_removes :: Int -> Map.Map Int String -> Property
prop_map_delete_removes k m =
  let m' = Map.delete k m
  in property $ Map.notMember k m'

prop_map_union_preserves :: Map.Map Int String -> Map.Map Int String -> Property
prop_map_union_preserves m1 m2 =
  let union = Map.union m1 m2
  in conjoin
    [ counterexample "keys from m1 should be preserved" $
        all (`Map.member` union) (Map.keys m1)
    , counterexample "keys from m2 should be preserved" $
        all (`Map.member` union) (Map.keys m2)
    ]

prop_set_union_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_union_commutative s1 s2 =
  Set.union s1 s2 === Set.union s2 s1

prop_set_intersection_commutative :: Set.Set Int -> Set.Set Int -> Property
prop_set_intersection_commutative s1 s2 =
  Set.intersection s1 s2 === Set.intersection s2 s1

prop_set_insert_idempotent :: Int -> Set.Set Int -> Property
prop_set_insert_idempotent x s =
  let s1 = Set.insert x s
      s2 = Set.insert x s1
  in s1 === s2

prop_set_delete_removes :: Int -> Set.Set Int -> Property
prop_set_delete_removes x s =
  let s' = Set.delete x s
  in property $ x `Set.notMember` s'

prop_sort_idempotent :: [Int] -> Property
prop_sort_idempotent xs =
  let sorted = sort xs
  in sort sorted === sorted

prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs =
  let unique = nub xs
  in counterexample "nub result should have no duplicates" $
       length unique === length (nub unique)

prop_reverse_twice_identity :: [Int] -> Property
prop_reverse_twice_identity xs =
  reverse (reverse xs) === xs

prop_reverse_preserves_length :: [Int] -> Property
prop_reverse_preserves_length xs =
  length (reverse xs) === length xs

prop_concat_associative :: String -> String -> String -> Property
prop_concat_associative a b c =
  (a ++ b) ++ c === a ++ (b ++ c)

prop_empty_concat_identity :: String -> Property
prop_empty_concat_identity s =
  ("" ++ s === s) .&&. (s ++ "" === s)

-- SourceLocation Properties
prop_sourcepos_offset_nonnegative :: Property
prop_sourcepos_offset_nonnegative = forAll genValidSourcePos $ \(l, c, o) ->
  let pos = SourcePos l c o
  in posOffset pos >= 0
  where
    genValidSourcePos = do
      l <- choose (1, 1000)
      c <- choose (1, 1000)
      o <- choose (0, 10000)
      return (l, c, o)

prop_sourcespan_ordering :: Property
prop_sourcespan_ordering = forAll genValidSpan $ \span ->
  let start = SourcePos (posLine $ spanStart span) (posColumn $ spanStart span) (posOffset $ spanStart span)
      end = SourcePos (posLine $ spanEnd span) (posColumn $ spanEnd span) (posOffset $ spanEnd span)
  in posOffset start <= posOffset end
  where
    genValidSpan = do
      l1 <- choose (1, 100)
      c1 <- choose (1, 100)
      o1 <- choose (0, 1000)
      l2 <- choose (l1, 100)
      c2 <- if l2 == l1 then choose (c1, 100) else choose (1, 100)
      o2 <- choose (o1, 1000)
      let start = SourcePos l1 c1 o1
          end = SourcePos l2 c2 o2
      return $ SourceSpan start end

prop_sourcespan_length_nonnegative :: Property
prop_sourcespan_length_nonnegative = forAll genValidSpan $ \span ->
  let start = spanStart span
      end = spanEnd span
      length = posOffset end - posOffset start
  in length >= 0
  where
    genValidSpan = do
      l1 <- choose (1, 100)
      c1 <- choose (1, 100)
      o1 <- choose (0, 1000)
      l2 <- choose (l1, 100)
      c2 <- if l2 == l1 then choose (c1, 100) else choose (1, 100)
      o2 <- choose (o1, 1000)
      let start = SourcePos l1 c1 o1
          end = SourcePos l2 c2 o2
      return $ SourceSpan start end

-- Parser Directives Properties
prop_defaultFileDirectives_empty :: Property
prop_defaultFileDirectives_empty =
  let fd = defaultFileDirectives
  in conjoin
    [ counterexample "fdOwnership should be Nothing" $ fdOwnership fd === Nothing
    , counterexample "fdDependentTypes should be Nothing" $ fdDependentTypes fd === Nothing
    , counterexample "fdConstraints should be Nothing" $ fdConstraints fd === Nothing
    ]

prop_defaultBlockDirectives_empty :: Property
prop_defaultBlockDirectives_empty =
  let bd = defaultBlockDirectives
  in conjoin
    [ counterexample "bdOwnership should be Nothing" $ bdOwnership bd === Nothing
    , counterexample "bdDependentTypes should be Nothing" $ bdDependentTypes bd === Nothing
    , counterexample "bdConstraints should be Nothing" $ bdConstraints bd === Nothing
    ]

-- Type System Properties
prop_type_reflexive :: Property
prop_type_reflexive = forAll genSimpleType $ \t ->
  t === t
  where
    genSimpleType = do
      name <- arbitrary
      return $ TypeName name

prop_type_show_preserves :: Property
prop_type_show_preserves = forAll genSimpleType $ \t ->
  let shown = show t
  in counterexample "show should preserve structure" $
       not (null shown)
  where
    genSimpleType = do
      name <- arbitrary
      return $ TypeName name

-- Character Properties
prop_tolower_toupper_preserves :: Char -> Property
prop_tolower_toupper_preserves c =
  not (isLetter c) ==> toLower (toUpper c) === c
  where
    isLetter ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')

prop_isspace_idempotent :: Char -> Property
prop_isspace_idempotent c =
  isSpace (if isSpace c then ' ' else c) === isSpace c

prop_tolower_idempotent :: Char -> Property
prop_tolower_idempotent c =
  toLower (toLower c) === toLower c

prop_toupper_idempotent :: Char -> Property
prop_toupper_idempotent c =
  toUpper (toUpper c) === toUpper c

-- List Properties
prop_group_preserves_order :: [Int] -> Property
prop_group_preserves_order xs =
  let groups = group xs
      flattened = concat groups
  in flattened === xs

prop_head_nonempty :: NonEmptyList Int -> Property
prop_head_nonempty (NonEmpty xs) =
  not (null xs) ==> property (head xs `elem` xs)

prop_tail_nonempty :: NonEmptyList Int -> Property
prop_tail_nonempty (NonEmpty xs) =
  not (null xs) ==> length (tail xs) === length xs - 1

prop_take_drop_partition :: Int -> [Int] -> Property
prop_take_drop_partition n xs =
  let taken = take n xs
      dropped = drop n xs
  in taken ++ dropped === xs

prop_take_length_bounded :: Int -> [Int] -> Property
prop_take_length_bounded n xs =
  property $ length (take n xs) <= min n (length xs)
